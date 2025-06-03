//! Shared BSN AST core used by both macro and assets.

use syn::{
    braced, bracketed, parenthesized,
    parse::{discouraged::Speculative, Parse, ParseStream},
    punctuated::{Pair, Punctuated},
    token::{Brace, Bracket, Paren},
    Block, Expr, ExprPath, Ident, Index, Path, Result, Token,
};

pub use quote;
pub use syn;

/// Low-level `syn`-based bsn AST that may be used by both the macro and the asset loader.
pub struct BsnAst {
    /// The root entity
    pub root: BsnAstEntity,
}

impl Parse for BsnAst {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            root: input.parse()?,
        })
    }
}

/// AST-representation of a BSN entity.
pub struct BsnAstEntity {
    /// Inherited scenes
    pub inherits: Punctuated<BsnAstInherit, Token![,]>,
    /// Components patch
    pub patch: BsnAstPatch,
    /// Optional children bracket token
    pub children_bracket: Option<Bracket>,
    /// Child entities
    pub children: Punctuated<BsnAstChild, Token![,]>,
    /// Key for this entity
    pub key: Option<BsnAstKey>,
}

impl Parse for BsnAstEntity {
    fn parse(input: ParseStream) -> Result<Self> {
        let key = if input.peek(Ident) && input.peek2(Token![:]) && !input.peek3(Token![:]) {
            // Static key
            let key = input.parse::<Ident>()?;
            input.parse::<Token![:]>()?;
            Some(BsnAstKey::Static(key.to_string()))
        } else {
            // Look for dynamic key
            let fork = input.fork();
            if fork.peek(Brace) {
                let block: Block = fork.parse()?;
                if fork.peek(Token![:]) && !fork.peek2(Token![:]) {
                    fork.parse::<Token![:]>()?;
                    input.advance_to(&fork);
                    Some(BsnAstKey::Dynamic(block))
                } else {
                    None
                }
            } else {
                None
            }
        };

        let mut inherits = Punctuated::new();
        let patch;
        if input.peek(Paren) {
            let content;
            let paren = parenthesized![content in input];

            let mut patch_tuple = Punctuated::new();

            loop {
                if content.is_empty() {
                    break;
                }

                if content.peek(Token![:]) {
                    content.parse::<Token![:]>()?;
                    inherits = content.parse_terminated(BsnAstInherit::parse, Token![,])?;
                    break;
                }

                let patch = content.parse::<BsnAstPatch>()?;
                patch_tuple.push_value(patch);
                if content.is_empty() {
                    break;
                }

                if content.peek(Token![:]) || (content.peek(Token![,]) && content.peek2(Token![:]))
                {
                    content.parse::<Token![,]>().ok();
                    content.parse::<Token![:]>()?;
                    inherits = content.parse_terminated(BsnAstInherit::parse, Token![,])?;
                    break;
                }

                if content.peek(Token![,]) {
                    let punct = content.parse()?;
                    patch_tuple.push_punct(punct);
                }
            }

            patch = BsnAstPatch::Tuple(paren, patch_tuple);
        } else {
            patch = BsnAstPatch::parse(input)?;
        }

        let (children_bracket, children) = if input.peek(Bracket) {
            let content;
            let bracket = bracketed![content in input];
            (
                Some(bracket),
                content.parse_terminated(BsnAstChild::parse, Token![,])?,
            )
        } else {
            (None, Punctuated::new())
        };

        Ok(Self {
            inherits,
            patch,
            children_bracket,
            children,
            key,
        })
    }
}

/// AST-representation of a single child item of a BSN entity.
pub enum BsnAstChild {
    /// A child entity using the BSN syntax.
    Entity(BsnAstEntity),
    /// An expression prefixed with `..`, evaluating to a `Scene`.
    Spread(Expr),
}

impl Parse for BsnAstChild {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(Token![..]) {
            input.parse::<Token![..]>()?;
            Ok(BsnAstChild::Spread(input.parse()?))
        } else {
            Ok(BsnAstChild::Entity(input.parse()?))
        }
    }
}

/// AST for a BSN entity key.
pub enum BsnAstKey {
    /// A static key: `key: ...`
    Static(String),
    /// A dynamic key: `{key}: ...`
    Dynamic(Block),
}

/// AST for a top-level BSN patch (a component or a tuple of patches).
pub enum BsnAstPatch {
    /// A struct patch
    StructPatch(ExprPath, BsnAstStruct),
    /// An enum patch
    EnumPatch(ExprPath, BsnAstEnum),
    /// A type path followed by a single `@`-prefixed expression evaluating to props for the component to be constructed. E.g. `Component@"prop value"`
    ///
    /// This can be used in cases where the type of the component is not inferable from the expression.
    TypedExpr(ExprPath, Token![@], Expr),
    /// An unbraced expression, works for expressions that are not ambigous with any other patch variants.
    ///
    /// This is an inferred expression, meaning it only works for blanket construct types.
    InferredExpr(Expr),
    /// A braced expression, allowing expressions that are ambigous with other patch variants.
    ///
    /// This is an inferred expression, meaning it only works for blanket construct types.
    BracedInferredExpr(Block),
    /// A tuple of patches
    Tuple(Paren, Punctuated<BsnAstPatch, Token![,]>),
}

impl Parse for BsnAstPatch {
    fn parse(input: ParseStream) -> Result<BsnAstPatch> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Paren) {
            // Tuple
            let content;
            let paren = parenthesized![content in input];
            let tuple = content.parse_terminated(BsnAstPatch::parse, Token![,])?;
            return Ok(BsnAstPatch::Tuple(paren, tuple));
        }

        if lookahead.peek(Brace) {
            // Braced expression
            let block = input.parse::<Block>()?;
            return Ok(BsnAstPatch::BracedInferredExpr(block));
        }

        if !lookahead.peek(Ident) {
            return Err(lookahead.error());
        }

        let fork = input.fork();
        let path = fork.parse::<ExprPath>()?;
        if fork.peek(Token![@]) {
            // Typed expression
            input.advance_to(&fork);
            let token = input.parse::<Token![@]>()?;
            let expr = input.parse::<Expr>()?;
            return Ok(BsnAstPatch::TypedExpr(path, token, expr));
        }

        let val = BsnAstValue::parse_top_level(input)?;
        let patch = match val {
            BsnAstValue::StructPatch(path, struct_patch) => {
                BsnAstPatch::StructPatch(path.unwrap(), struct_patch)
            }
            BsnAstValue::EnumPatch(path, enum_patch) => BsnAstPatch::EnumPatch(path, enum_patch),
            BsnAstValue::Expr(expr) => BsnAstPatch::InferredExpr(expr),
            BsnAstValue::BracedExpr(block) => BsnAstPatch::BracedInferredExpr(block),
        };

        Ok(patch)
    }
}

/// A struct patch.
#[derive(Clone)]
pub enum BsnAstStruct {
    /// A struct patch without fields.
    Unit,
    /// A struct patch with named fields.
    Named(Brace, Punctuated<BsnAstNamedField, Token![,]>),
    /// A tuple-like struct patch with unnamed fields.
    TupleLike(Paren, Punctuated<BsnAstTupleField, Token![,]>),
}

/// Named prop field for a struct patch.
#[derive(Clone)]
pub struct BsnAstNamedField {
    /// Field name
    pub name: Ident,
    /// Colon token
    pub colon_token: Token![:],
    /// Field value
    pub value: BsnAstProp,
}

/// Unnamed prop field for a tuple-like patch.
#[derive(Clone)]
pub struct BsnAstTupleField {
    /// Index of the field
    pub index: Index,
    /// Field value
    pub value: BsnAstProp,
}

impl Parse for BsnAstNamedField {
    fn parse(input: ParseStream) -> Result<BsnAstNamedField> {
        let name = input.parse::<Ident>()?;
        let colon_token = input.parse::<Token![:]>()?;
        let value = input.parse::<BsnAstProp>()?;
        Ok(BsnAstNamedField {
            name,
            colon_token,
            value,
        })
    }
}

impl Parse for BsnAstStruct {
    fn parse(input: ParseStream) -> Result<BsnAstStruct> {
        if input.peek(Brace) {
            let content;
            let brace_token = braced![content in input];
            let fields = content.parse_terminated(BsnAstNamedField::parse, Token![,])?;
            return Ok(BsnAstStruct::Named(brace_token, fields));
        } else if input.peek(Paren) {
            let content;
            let paren_token = parenthesized![content in input];
            let fields = content.parse_terminated(BsnAstProp::parse, Token![,])?;
            let fields = fields
                .into_pairs()
                .map(Pair::into_tuple)
                .enumerate()
                .map(|(index, (value, punct))| {
                    Pair::new(
                        BsnAstTupleField {
                            index: index.into(),
                            value,
                        },
                        punct,
                    )
                })
                .collect();
            return Ok(BsnAstStruct::TupleLike(paren_token, fields));
        }

        Ok(BsnAstStruct::Unit)
    }
}

/// An enum variant and its struct patch.
#[derive(Clone)]
pub struct BsnAstEnum {
    /// `::`-token between the type path and the variant
    pub variant_separator: Token![::],
    /// Variant ident
    pub variant: Ident,
    /// Struct patch
    pub struct_patch: BsnAstStruct,
}

impl BsnAstEnum {
    fn parse_with_path(input: ParseStream, path: &mut Path) -> Result<BsnAstEnum> {
        let variant = path.segments.pop().unwrap().into_value().ident;
        let variant_separator = path.segments.pop_punct().unwrap();
        let struct_patch = input.parse::<BsnAstStruct>()?;
        Ok(BsnAstEnum {
            variant_separator,
            variant,
            struct_patch,
        })
    }
}

/// AST for a BSN property.
#[derive(Clone)]
pub enum BsnAstProp {
    /// A value not prefixed with `@`, this may contain nested partial patches.
    Value(BsnAstValue),
    /// An expression prefixed with `@`.
    Props(Expr),
}

impl Parse for BsnAstProp {
    fn parse(input: ParseStream) -> Result<BsnAstProp> {
        if input.peek(Token![@]) {
            input.parse::<Token![@]>()?;
            let expr = input.parse::<Expr>()?;
            Ok(BsnAstProp::Props(expr))
        } else {
            let val = input.parse::<BsnAstValue>()?;
            Ok(BsnAstProp::Value(val))
        }
    }
}

/// AST for a value in a BSN patch.
#[derive(Clone)]
pub enum BsnAstValue {
    /// A struct patch. Path is optional for non top-level structs with **named** fields. E.g. `Vec3 { .. }` or `{ ... }`
    StructPatch(Option<ExprPath>, BsnAstStruct),
    /// An enum patch, type path + variant is always required. E.g. `Val::Px(..)`
    EnumPatch(ExprPath, BsnAstEnum),
    /// An unbraced expression, works for expressions that are not ambigous with structs or enums.
    Expr(Expr),
    /// An expression surrounded by braces. Alternative for expressions that are ambigous with structs or enums.
    BracedExpr(Block),
}

impl Parse for BsnAstValue {
    fn parse(input: ParseStream) -> Result<BsnAstValue> {
        Self::parse_internal(input, false)
    }
}

impl BsnAstValue {
    /// Parse as a top-level component patch.
    pub fn parse_top_level(input: ParseStream) -> Result<BsnAstValue> {
        Self::parse_internal(input, true)
    }

    fn parse_internal(input: ParseStream, top_level: bool) -> Result<BsnAstValue> {
        if input.peek(Brace) {
            let content;
            let brace_token = braced![content in input];

            if !top_level
                && content.peek(Ident)
                && content.peek2(Token![:])
                && !content.peek3(Token![:])
            {
                // Named struct patch without path
                let fields = content.parse_terminated(BsnAstNamedField::parse, Token![,])?;
                return Ok(BsnAstValue::StructPatch(
                    None,
                    BsnAstStruct::Named(brace_token, fields),
                ));
            }

            // Braced expression
            return Ok(BsnAstValue::BracedExpr(Block {
                brace_token,
                stmts: content.call(Block::parse_within)?,
            }));
        }

        let fork = input.fork();
        if let Ok(mut path) = fork.parse::<ExprPath>() {
            if last_is_capitalized(&path.path) {
                input.advance_to(&fork);

                if path.path.segments.len() > 1 {
                    // Enum patch
                    let enum_patch = BsnAstEnum::parse_with_path(input, &mut path.path)?;
                    return Ok(BsnAstValue::EnumPatch(path, enum_patch));
                }

                // Struct patch
                let struct_patch = input.parse::<BsnAstStruct>()?;
                if !top_level && matches!(struct_patch, BsnAstStruct::Unit) {
                    // Treat unit-like paths as expressions if not top-level
                    // This allows for constants to be used without braces
                    return Ok(BsnAstValue::Expr(path.into()));
                }

                return Ok(BsnAstValue::StructPatch(Some(path), struct_patch));
            }
        }

        // Unbraced expression
        let expr = input.parse::<Expr>()?;
        Ok(BsnAstValue::Expr(expr))
    }
}

fn last_is_capitalized(path: &Path) -> bool {
    let last_segment = path.segments.last().unwrap().ident.to_string();
    let capitalized = last_segment.chars().next().unwrap().is_uppercase();
    capitalized
}

/// An inherited patch function path, with optional `,`-separated parameters surrounded by `()`.
#[derive(Clone)]
pub struct BsnAstInherit(pub Path, pub Punctuated<Expr, Token![,]>);

impl Parse for BsnAstInherit {
    fn parse(input: ParseStream) -> Result<BsnAstInherit> {
        let path = input.parse::<Path>()?;

        // Optional params
        let params = if input.peek(Paren) {
            let content;
            parenthesized![content in input];
            content.parse_terminated(Expr::parse, Token![,])?
        } else {
            Punctuated::new()
        };
        Ok(BsnAstInherit(path, params))
    }
}
