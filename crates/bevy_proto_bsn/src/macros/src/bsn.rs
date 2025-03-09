use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse2,
    punctuated::{Pair, Punctuated},
    Path,
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
        let mut entity_index = 0;
        bsn_ast_entity_to_tokens(tokens, &self.root, &mut entity_index);
    }
}

fn bsn_ast_entity_to_tokens(
    tokens: &mut TokenStream,
    entity: &BsnAstEntity,
    entity_index: &mut usize,
) {
    let bevy_proto_bsn = bevy_proto_bsn_path();
    let patch = &entity.patch.to_token_stream();
    let inherits = entity
        .inherits
        .iter()
        .map(ToTokensInternal::to_token_stream);

    let my_entity_index = *entity_index;
    *entity_index += 1;

    let children = entity.children.iter().map(|c| {
        let mut tokens = TokenStream::new();
        bsn_ast_child_to_tokens(&mut tokens, c, entity_index);
        tokens
    });

    let key = entity.key.to_token_stream();

    #[cfg(not(feature = "hot_macro"))]
    let output = quote! {
        #bevy_proto_bsn::EntityPatch {
            inherit: (#(#inherits,)*),
            patch: #patch,
            children: (#(#children,)*),
            key: #key
        }
    };

    #[cfg(feature = "hot_macro")]
    let output = {
        quote! {
            #bevy_proto_bsn::EntityPatch {
                inherit: (#(#inherits,)*),
                patch: #patch,
                children: (#(#children,)*),
                key: #key,
                hot_id: #bevy_proto_bsn::hot_macro::EntityPatchId::new(
                    file!(),
                    line!(),
                    column!(),
                    #my_entity_index
                ),
                hot_patch: None,
            }
        }
    };

    output.to_tokens(tokens);
}

fn bsn_ast_child_to_tokens(
    tokens: &mut TokenStream,
    child: &BsnAstChild,
    entity_index: &mut usize,
) {
    match child {
        BsnAstChild::Entity(entity) => bsn_ast_entity_to_tokens(tokens, entity, entity_index),
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
            BsnAstPatch::Patch(path, fields) => {
                let assignments = fields.iter().map(|(member, prop)| {
                    let member = member.to_token_stream();
                    let prop = prop.to_token_stream();
                    quote! {
                        __props.#member = #prop;
                    }
                });
                quote! {
                    #path::patch(move |__props| {
                        #(#assignments)*
                    })
                }
            }
            BsnAstPatch::Tuple(tuple) => {
                let tuple = tuple.to_token_stream();
                quote! {
                    (#tuple)
                }
            }
            BsnAstPatch::Expr(expr) => quote! {
                #bevy_proto_bsn::ConstructPatch::new_inferred(move |__props| {
                    *__props = #expr;
                })
            },
        }
        .to_tokens(tokens);
    }
}

impl ToTokensInternal for BsnAstProp {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let bevy_proto_bsn = bevy_proto_bsn_path();
        match self {
            BsnAstProp::Value(expr) => quote! {
                (#expr).into()
            },
            BsnAstProp::Props(expr) => quote! {
                #bevy_proto_bsn::ConstructProp::Props((#expr).into())
            },
        }
        .to_tokens(tokens);
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
