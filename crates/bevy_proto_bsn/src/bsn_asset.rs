use bevy::{
    asset::{io::Reader, AssetLoader, AsyncReadExt, LoadContext},
    prelude::*,
};
use thiserror::Error;

use bevy_proto_bsn_ast::{
    quote::ToTokens,
    syn::{Block, Expr, ExprBlock, ExprCall, ExprLit, ExprStruct, Lit, Member},
    *,
};

pub(crate) fn bsn_asset_plugin(app: &mut App) {
    app.init_asset::<Bsn>();
    app.init_asset_loader::<BsnLoader>();
}

/// Asset loader for loading `.bsn`-files as [`Bsn`]s.
#[derive(Default)]
pub struct BsnLoader;

/// Error type for [`BsnLoader`].
#[non_exhaustive]
#[derive(Debug, Error)]
pub enum BsnLoaderError {
    /// An [IO](std::io) Error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    /// A syntax error
    #[error("Syntax error: {0}")]
    SyntaxError(String),
}

impl AssetLoader for BsnLoader {
    type Asset = Bsn;
    type Settings = ();
    type Error = BsnLoaderError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut content = String::new();
        reader.read_to_string(&mut content).await?;

        let ast = syn::parse_str::<BsnAstEntity>(&content).map_err(|e| {
            let start = e.span().start();
            BsnLoaderError::SyntaxError(format!("{} at {}:{}", e, start.line, start.column))
        })?;

        let bsn = Bsn::from(&ast);

        Ok(bsn)
    }

    fn extensions(&self) -> &[&str] {
        &["proto_bsn", "bsn"]
    }
}

/// A non type-aware representation of a BSN-tree.
///
/// Can be loaded from a `.bsn` file using [`BsnLoader`].
#[derive(Default, Debug, Clone, Asset, TypePath, Hash)]
pub struct Bsn {
    /// The root entity of the BSN-tree.
    pub root: BsnEntity,
}

/// A non type-aware BSN entity.
#[derive(Default, Debug, Clone, Hash)]
pub struct BsnEntity {
    /// Components of the entity.
    pub components: Vec<BsnComponent>,
    /// Child entities
    pub children: Vec<BsnEntity>,
    /// Optional key used for retaining.
    pub key: Option<BsnKey>,
}

/// A non type-aware representation of a BSN key.
#[derive(Debug, Clone, Hash)]
pub enum BsnKey {
    /// A static key: `key: ...`
    Static(String),
    /// A dynamic key: `{<expr>}: ...`
    Dynamic(String),
}

/// A non type-aware representation of a BSN component.
#[derive(Debug, Clone, Hash)]
pub enum BsnComponent {
    /// A struct patch.
    StructPatch(String, BsnStruct),
    /// An enum patch.
    EnumPatch(String, BsnEnum),
    /// A typed expression prefixed with its type path followed by `@`.
    TypedExpr(String, String),
    /// An expression without braces
    InferredExpr(String),
    /// A braced `{ ... }` expression.
    BracedExpr(String),
}

/// A non type-aware representation of a BSN struct.
#[derive(Debug, Clone, Hash)]
pub enum BsnStruct {
    /// No fields specified
    Unit,
    /// Named fields
    Named(Vec<(String, BsnValue)>),
    /// Tuple-like fields
    TupleLike(Vec<BsnValue>),
}

/// A non type-aware representation of a BSN enum.
#[derive(Debug, Clone, Hash)]
pub struct BsnEnum {
    /// Enum variant
    variant: String,
    /// Variant fields as a struct patch
    struct_patch: BsnStruct,
}

/// A value in a BSN tree.
#[derive(Debug, Clone, Hash)]
pub enum BsnValue {
    /// A literal bool
    Bool(bool),
    /// A literal number
    Number(String),
    /// A literal string
    String(String),
    /// A literal character
    Char(char),
    /// A path or ident. Could be a non-patch unit struct/enum variant, or a constant/local ident.
    Path(String),
    /// A struct patch with optional path if named fields.
    StructPatch(Option<String>, BsnStruct),
    /// An enum patch consisting of its type path, variant and struct patch.
    EnumPatch(String, BsnEnum),
    /// A named struct (not in a patch context).
    NamedStruct(String, Vec<(String, BsnValue)>),
    /// A call-like expression (not in a patch context). Could be a tuple struct, a tuple-like enum variant, or a function/method call.
    Call(String, Vec<BsnValue>),
    /// A tuple of values.
    Tuple(Vec<BsnValue>),
    /// Prop value
    Prop(Box<BsnValue>),
    /// An unknown expression.
    UnknownExpr(String),
}

impl From<&BsnAstEntity> for Bsn {
    fn from(ast: &BsnAstEntity) -> Self {
        Bsn {
            root: BsnEntity::from(ast),
        }
    }
}

impl From<&BsnAstEntity> for BsnEntity {
    fn from(ast: &BsnAstEntity) -> Self {
        BsnEntity {
            components: BsnComponent::vec_from_ast_patch(&ast.patch),
            children: ast
                .children
                .iter()
                .filter_map(|c| match c {
                    BsnAstChild::Entity(entity) => Some(BsnEntity::from(entity)),
                    _ => None,
                })
                .collect(),
            key: ast.key.as_ref().map(BsnKey::from),
        }
    }
}

impl From<&BsnAstKey> for BsnKey {
    fn from(key: &BsnAstKey) -> Self {
        match key {
            BsnAstKey::Static(key) => BsnKey::Static(key.clone()),
            BsnAstKey::Dynamic(key) => BsnKey::Dynamic(key.to_token_stream().to_string()),
        }
    }
}

impl BsnComponent {
    /// Converts and flattens a [`BsnAstPatch`] patch to a [`Vec<BsnComponent>`].
    fn vec_from_ast_patch(patch: &BsnAstPatch) -> Vec<BsnComponent> {
        let mut components = Vec::new();
        Self::convert_components(&mut components, patch);
        components
    }

    fn convert_components(components: &mut Vec<BsnComponent>, patch: &BsnAstPatch) {
        match patch {
            BsnAstPatch::StructPatch(path, struct_patch) => {
                let path = path.to_compact_string();
                components.push(BsnComponent::StructPatch(path, struct_patch.into()));
            }
            BsnAstPatch::EnumPatch(path, enum_patch) => {
                let path = path.to_compact_string();
                components.push(BsnComponent::EnumPatch(path, enum_patch.into()));
            }
            BsnAstPatch::TypedExpr(path, _, expr) => {
                components.push(BsnComponent::TypedExpr(
                    path.to_compact_string(),
                    expr.to_token_stream().to_string(),
                ));
            }
            BsnAstPatch::InferredExpr(expr) => {
                components.push(BsnComponent::InferredExpr(
                    expr.to_token_stream().to_string(),
                ));
            }
            BsnAstPatch::BracedInferredExpr(block) => {
                components.push(BsnComponent::BracedExpr(
                    block.to_token_stream().to_string(),
                ));
            }
            BsnAstPatch::Tuple(_, patches) => {
                for patch in patches {
                    Self::convert_components(components, patch);
                }
            }
        }
    }
}

// impl From<&BsnAstProp> for BsnProp {
//     fn from(prop: &BsnAstProp) -> Self {
//         match prop {
//             BsnAstProp::Value(value) => BsnProp::Value(value.into()),
//             BsnAstProp::Props(value) => BsnProp::Props(value.into()),
//         }
//     }
// }

impl From<&BsnAstStruct> for BsnStruct {
    fn from(struct_patch: &BsnAstStruct) -> Self {
        match struct_patch {
            BsnAstStruct::Unit => BsnStruct::Unit,
            BsnAstStruct::Named(_, fields) => BsnStruct::Named(
                fields
                    .iter()
                    .map(|field| (field.name.to_string(), BsnValue::from(&field.value)))
                    .collect(),
            ),
            BsnAstStruct::TupleLike(_, fields) => BsnStruct::TupleLike(
                fields
                    .iter()
                    .map(|field| BsnValue::from(&field.value))
                    .collect(),
            ),
        }
    }
}

impl From<&BsnAstEnum> for BsnEnum {
    fn from(struct_patch: &BsnAstEnum) -> Self {
        BsnEnum {
            variant: struct_patch.variant.to_string(),
            struct_patch: BsnStruct::from(&struct_patch.struct_patch),
        }
    }
}

impl From<&BsnAstProp> for BsnValue {
    fn from(prop: &BsnAstProp) -> Self {
        match prop {
            BsnAstProp::Value(value) => value.into(),
            BsnAstProp::Props(value) => BsnValue::Prop(Box::new(value.into())),
        }
    }
}

impl From<&BsnAstValue> for BsnValue {
    fn from(ast_value: &BsnAstValue) -> Self {
        match ast_value {
            BsnAstValue::StructPatch(maybe_path, struct_patch) => {
                let path = maybe_path.as_ref().map(ToTokensExt::to_compact_string);
                BsnValue::StructPatch(path, BsnStruct::from(struct_patch))
            }
            BsnAstValue::EnumPatch(path, enum_patch) => {
                let path = path.to_compact_string();
                BsnValue::EnumPatch(path, BsnEnum::from(enum_patch))
            }
            BsnAstValue::Expr(expr) => expr.into(),
            BsnAstValue::BracedExpr(block) => (&Expr::Block(ExprBlock {
                block: block.clone(),
                attrs: vec![],
                label: None,
            }))
                .into(),
        }
    }
}

impl From<&Expr> for BsnValue {
    fn from(expr: &Expr) -> Self {
        match &expr {
            Expr::Lit(lit) => lit.into(),
            Expr::Tuple(expr_tuple) => {
                let mut tuple = Vec::new();
                for expr in &expr_tuple.elems {
                    tuple.push(expr.into());
                }
                BsnValue::Tuple(tuple)
            }
            Expr::Path(path) => BsnValue::Path(path.to_compact_string()),
            Expr::Struct(strct) => strct.into(),
            Expr::Call(call) => call.into(),
            Expr::Paren(paren) => paren.expr.as_ref().into(),
            expr => BsnValue::UnknownExpr(expr.to_token_stream().to_string()),
        }
    }
}

impl From<&Block> for BsnValue {
    fn from(block: &Block) -> Self {
        BsnValue::UnknownExpr(block.to_token_stream().to_string())
    }
}

impl From<&ExprLit> for BsnValue {
    fn from(lit: &ExprLit) -> Self {
        match &lit.lit {
            Lit::Bool(b) => BsnValue::Bool(b.value),
            Lit::Int(i) => BsnValue::Number(i.to_token_stream().to_string()),
            Lit::Float(f) => BsnValue::Number(f.to_token_stream().to_string()),
            Lit::Str(s) => BsnValue::String(s.value()),
            Lit::Char(c) => BsnValue::Char(c.value()),
            _ => BsnValue::UnknownExpr(lit.to_token_stream().to_string()),
        }
    }
}

impl From<&ExprStruct> for BsnValue {
    fn from(strct: &ExprStruct) -> Self {
        let path = strct.path.to_compact_string();
        let fields = strct
            .fields
            .iter()
            .map(|field| match &field.member {
                Member::Named(name) => (name.to_string(), BsnValue::from(&field.expr)),
                _ => unreachable!(),
            })
            .collect();
        BsnValue::NamedStruct(path, fields)
    }
}

impl From<&ExprCall> for BsnValue {
    fn from(call: &ExprCall) -> Self {
        let path = match call.func.as_ref() {
            Expr::Path(path) => path.to_compact_string(),
            _ => {
                return BsnValue::UnknownExpr(call.to_token_stream().to_string());
            }
        };

        let fields_or_params = call.args.iter().map(BsnValue::from).collect();

        BsnValue::Call(path, fields_or_params)
    }
}

trait ToTokensExt {
    fn to_compact_string(&self) -> String;
}

impl<T: ToTokens> ToTokensExt for T {
    fn to_compact_string(&self) -> String {
        self.to_token_stream().to_string().replace(" ", "")
    }
}

/// Trait for types that can be converted to `.bsn`-strings. Powers saving of BSN assets.
pub trait ToBsnString {
    /// Convert to a string in `.bsn` format.
    fn to_bsn_string(&self) -> String;
}

trait Joined {
    fn joined(&self, separator: &str) -> String;
}

impl<T> Joined for Vec<T>
where
    T: ToBsnString,
{
    fn joined(&self, separator: &str) -> String {
        self.iter()
            .map(ToBsnString::to_bsn_string)
            .collect::<Vec<_>>()
            .join(separator)
    }
}

impl<T> ToBsnString for (String, T)
where
    T: ToBsnString,
{
    fn to_bsn_string(&self) -> String {
        format!("{}: {}", self.0, self.1.to_bsn_string())
    }
}

impl ToBsnString for Bsn {
    fn to_bsn_string(&self) -> String {
        self.root.to_bsn_string()
    }
}

impl ToBsnString for BsnEntity {
    fn to_bsn_string(&self) -> String {
        let components = match self.components.len() {
            0 => "()".to_string(),
            1 => self.components[0].to_bsn_string(),
            _ => format!("({})", self.components.joined(", ")),
        };
        let children = if self.children.is_empty() {
            "".to_string()
        } else {
            format!(" [{}]", self.children.joined(", "))
        };
        format!("{}{}", components, children)
    }
}

impl ToBsnString for BsnKey {
    fn to_bsn_string(&self) -> String {
        match self {
            BsnKey::Static(key) => format!("{}: ", key),
            BsnKey::Dynamic(key) => format!("{{{}}}: ", key),
        }
    }
}

impl ToBsnString for BsnComponent {
    fn to_bsn_string(&self) -> String {
        match self {
            BsnComponent::StructPatch(path, struct_patch) => match struct_patch {
                BsnStruct::Unit => path.clone(),
                BsnStruct::Named(_) => format!("{} {}", path, struct_patch.to_bsn_string()),
                BsnStruct::TupleLike(_) => format!("{}{}", path, struct_patch.to_bsn_string()),
            },
            BsnComponent::EnumPatch(path, enum_patch) => {
                format!("{}::{}", path, enum_patch.to_bsn_string())
            }
            BsnComponent::TypedExpr(path, expr) => format!("{}@{}", path, expr),
            BsnComponent::InferredExpr(expr) | BsnComponent::BracedExpr(expr) => expr.clone(),
        }
    }
}

impl ToBsnString for BsnStruct {
    fn to_bsn_string(&self) -> String {
        match self {
            BsnStruct::Unit => "".to_string(),
            BsnStruct::Named(fields) => format!("{{ {} }}", fields.joined(", ")),
            BsnStruct::TupleLike(fields) => format!("({})", fields.joined(", ")),
        }
    }
}

impl ToBsnString for BsnEnum {
    fn to_bsn_string(&self) -> String {
        format!("{} {}", self.variant, self.struct_patch.to_bsn_string())
    }
}

impl ToBsnString for BsnValue {
    fn to_bsn_string(&self) -> String {
        match self {
            BsnValue::Bool(b) => b.to_string(),
            BsnValue::Number(n) => n.clone(),
            BsnValue::String(s) => format!("\"{}\"", s),
            BsnValue::Char(c) => format!("'{}'", c),
            BsnValue::Path(p) => p.clone(),
            BsnValue::StructPatch(path, struct_patch) => match struct_patch {
                BsnStruct::Unit => panic!("Unit struct in non top-level patch context"),
                BsnStruct::Named(_) => format!(
                    "{}{}",
                    path.as_ref()
                        .map(|path| format!("{} ", path))
                        .unwrap_or_else(|| "".to_string()),
                    struct_patch.to_bsn_string()
                ),
                BsnStruct::TupleLike(_) => format!(
                    "{}{}",
                    path.as_ref()
                        .expect("Tuple-like struct patches must have a type path"),
                    struct_patch.to_bsn_string()
                ),
            },
            BsnValue::EnumPatch(path, enum_patch) => {
                format!("{}::{}", path, enum_patch.to_bsn_string())
            }
            BsnValue::NamedStruct(path, fields) => {
                format!("{} {{ {} }}", path, fields.joined(", "))
            }
            BsnValue::Call(path, args) => {
                format!("{}({})", path, args.joined(", "))
            }
            BsnValue::Prop(value) => format!("@{}", value.to_bsn_string()),
            BsnValue::Tuple(fields) => format!("({})", fields.joined(", ")),
            BsnValue::UnknownExpr(expr) => expr.clone(),
        }
    }
}
