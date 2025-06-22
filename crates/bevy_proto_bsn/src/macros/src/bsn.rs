use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse2,
    punctuated::{Pair, Punctuated},
    ExprPath, Ident, Member, Path, Token,
};

use bevy_proto_bsn_ast::*;

pub fn bsn(item: TokenStream) -> TokenStream {
    match parse2::<BsnAst>(item) {
        Ok(bsn) => bsn.to_token_stream(),
        Err(e) => e.to_compile_error(),
    }
}

fn bevy_proto_bsn_path() -> Path {
    Path::from(format_ident!("bevy_proto_bsn"))
}

trait ToTokensInternal {
    fn to_tokens(&self, tokens: &mut TokenStream);

    fn to_token_stream(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        tokens
    }
}

impl<T, P> ToTokensInternal for Punctuated<T, P>
where
    T: ToTokensInternal,
    P: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for pair in self.pairs() {
            match pair {
                Pair::Punctuated(a, b) => {
                    a.to_tokens(tokens);
                    b.to_tokens(tokens);
                }
                Pair::End(a) => a.to_tokens(tokens),
            }
        }
    }
}

impl ToTokensInternal for BsnAst {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        bsn_ast_entity_to_tokens(tokens, &self.root);
    }
}

fn bsn_ast_entity_to_tokens(tokens: &mut TokenStream, entity: &BsnAstEntity) {
    let bevy_proto_bsn = bevy_proto_bsn_path();
    let patch = &entity.patch.to_token_stream();
    let inherits = entity
        .inherits
        .iter()
        .map(ToTokensInternal::to_token_stream);

    let children = entity.children.iter().map(|c| {
        let mut tokens = TokenStream::new();
        bsn_ast_child_to_tokens(&mut tokens, c);
        tokens
    });

    let key = entity.key.to_token_stream();

    let output = quote! {
        #bevy_proto_bsn::EntityPatch {
            inherit: (#(#inherits,)*),
            patch: #patch,
            children: (#(#children,)*),
            key: #key
        }
    };

    output.to_tokens(tokens);
}

fn bsn_ast_child_to_tokens(tokens: &mut TokenStream, child: &BsnAstChild) {
    match child {
        BsnAstChild::Entity(entity) => bsn_ast_entity_to_tokens(tokens, entity),
        BsnAstChild::Spread(expr) => expr.to_tokens(tokens),
    };
}

impl ToTokensInternal for Option<BsnAstKey> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Some(BsnAstKey::Static(key)) => quote! {
                Some(#key.into())
            },
            Some(BsnAstKey::Dynamic(block)) => quote! {
                Some(#block.into())
            },
            None => quote! { None },
        }
        .to_tokens(tokens);
    }
}

impl ToTokensInternal for BsnAstPatch {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let bevy_proto_bsn = bevy_proto_bsn_path();
        match self {
            BsnAstPatch::StructPatch(path, struct_patch) => {
                let mut assignments = TokenStream::new();
                let mut field_path = Punctuated::new();
                let props_ident = format_ident!("__props");
                field_path.push(Member::Named(props_ident.clone()));
                struct_to_assignments(&mut assignments, &mut field_path, struct_patch, None, false);
                quote! {
                    #path::patch(move |#props_ident| {
                        #assignments
                    })
                }
                .to_tokens(tokens);
            }
            BsnAstPatch::EnumPatch(path, enum_patch) => {
                let mut assignments = TokenStream::new();
                let mut field_path = Punctuated::new();
                let props_ident = format_ident!("__props");
                field_path.push(Member::Named(props_ident.clone()));
                enum_to_assignments(&mut assignments, &mut field_path, path, enum_patch);
                quote! {
                    #path::patch(move |mut #props_ident| {
                        #assignments
                    })
                }
                .to_tokens(tokens);
            }
            BsnAstPatch::TypedExpr(path, _, expr) => {
                quote! {
                    #path::patch(move |mut __props| {
                        *__props = (#expr).into();
                    })
                }
                .to_tokens(tokens);
            }
            BsnAstPatch::InferredExpr(expr) => {
                quote! {
                    #bevy_proto_bsn::ConstructPatch::new_inferred(move |mut __props| {
                        *__props = #expr;
                    })
                }
                .to_tokens(tokens);
            }
            BsnAstPatch::BracedInferredExpr(block) => {
                quote! {
                    #bevy_proto_bsn::ConstructPatch::new_inferred(move |mut __props| {
                        *__props = #block;
                    })
                }
                .to_tokens(tokens);
            }
            BsnAstPatch::Tuple(paren, tuple) => {
                paren.surround(tokens, |tokens| {
                    tuple.to_tokens(tokens);
                });
            }
        }
    }
}

fn struct_to_assignments(
    tokens: &mut TokenStream,
    field_path: &mut Punctuated<Member, Token![.]>,
    struct_patch: &BsnAstStruct,
    field_name_overrides: Option<&Vec<Ident>>,
    deref_assignment: bool,
) {
    match struct_patch {
        BsnAstStruct::Unit => {}
        BsnAstStruct::Named(_, fields) => {
            for (i, field) in fields.iter().enumerate() {
                if let Some(overrides) = field_name_overrides {
                    field_path.push(overrides[i].clone().into());
                } else {
                    field_path.push(field.name.clone().into());
                }
                prop_to_assignments(tokens, field_path, &field.value, deref_assignment);
                field_path.pop();
                field_path.pop_punct();
            }
        }
        BsnAstStruct::TupleLike(_, fields) => {
            for (i, field) in fields.iter().enumerate() {
                if let Some(overrides) = field_name_overrides {
                    field_path.push(overrides[i].clone().into());
                } else {
                    field_path.push(field.index.clone().into());
                }
                prop_to_assignments(tokens, field_path, &field.value, deref_assignment);
                field_path.pop();
                field_path.pop_punct();
            }
        }
    }
}

fn enum_to_assignments(
    tokens: &mut TokenStream,
    field_path: &mut Punctuated<Member, Token![.]>,
    type_path: &ExprPath,
    enum_patch: &BsnAstEnum,
) {
    let BsnAstEnum {
        variant_separator,
        variant,
        struct_patch,
    } = enum_patch;

    let (destructure, assignments, default_assignment) = match struct_patch {
        BsnAstStruct::Unit => (quote! { { .. } }, quote! {}, quote! {}),
        BsnAstStruct::Named(_, fields) => {
            let field_names = fields.iter().map(|field| field.name.clone());
            let field_names2 = field_names.clone();
            let mut field_path = Punctuated::new();
            let mut assignments = TokenStream::new();
            struct_to_assignments(&mut assignments, &mut field_path, struct_patch, None, true);
            (
                quote! { { #(#field_names),*, .. } },
                assignments,
                quote! { { #(#field_names2 : Default::default()),* } },
            )
        }
        BsnAstStruct::TupleLike(_, fields) => {
            let field_names = fields
                .iter()
                .map(|field| format_ident!("_{}", field.index))
                .collect();
            let default_fields = fields.iter().map(|_| quote! { Default::default() });
            let mut field_path = Punctuated::new();
            let mut assignments = TokenStream::new();
            struct_to_assignments(
                &mut assignments,
                &mut field_path,
                struct_patch,
                Some(&field_names),
                true,
            );
            (
                quote! { ( #(#field_names),*, .. ) },
                assignments,
                quote! { ( #(#default_fields),* ) },
            )
        }
    };

    let enum_path = quote! {
        #type_path #variant_separator #variant
    };

    let deref_token = match field_path.first() {
        Some(Member::Named(first)) if field_path.len() == 1 && first.eq("__props") => {
            Some(Token![*](proc_macro2::Span::call_site()))
        }
        _ => None,
    };

    quote! {
        if !matches!(#field_path, #enum_path { .. }) {
            #deref_token #field_path = #enum_path #default_assignment;
        }
        if let #enum_path #destructure = &mut #field_path {
            #assignments
        }
    }
    .to_tokens(tokens);
}

fn prop_to_assignments(
    tokens: &mut TokenStream,
    field_path: &mut Punctuated<Member, Token![.]>,
    prop: &BsnAstProp,
    deref_assignment: bool,
) {
    let deref_token = if deref_assignment {
        Some(Token![*](proc_macro2::Span::call_site()))
    } else {
        None
    };

    let bevy_proto_bsn = bevy_proto_bsn_path();
    match prop {
        BsnAstProp::Value(val) => match val {
            BsnAstValue::StructPatch(_, struct_patch) => {
                struct_to_assignments(tokens, field_path, struct_patch, None, false);
            }
            BsnAstValue::EnumPatch(type_path, enum_patch) => {
                enum_to_assignments(tokens, field_path, type_path, enum_patch);
            }
            BsnAstValue::Expr(expr) => {
                quote! {
                    #deref_token #field_path = (#expr).into();
                }
                .to_tokens(tokens);
            }
            BsnAstValue::BracedExpr(block) => {
                quote! {
                    #deref_token #field_path = #block.into();
                }
                .to_tokens(tokens);
            }
        },
        BsnAstProp::Props(expr) => {
            quote! {
                #deref_token #field_path = #bevy_proto_bsn::ConstructProp::Props((#expr).into())
            }
            .to_tokens(tokens);
        }
    }
}

impl ToTokensInternal for BsnAstInherit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let BsnAstInherit(path, params) = &self;
        quote! {
            (#path (#params))
        }
        .to_tokens(tokens);
    }
}
