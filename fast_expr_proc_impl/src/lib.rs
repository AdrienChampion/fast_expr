//! Zip-spec-trait implementation procedural macro.
//!
//! This macro is a helper for end-user to implement zip-spec-traits.

extern crate proc_macro;

use std::collections::BTreeMap as Map;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn::{
    self,
    parse::{Parse, ParseStream},
};

mod expr_map;
mod expr_zip;

mod keyword {
    use super::*;

    syn::custom_keyword! { zip }
    syn::custom_keyword! { init }
    syn::custom_keyword! { step }
    syn::custom_keyword! { fold }
    syn::custom_keyword! { go_up }
    syn::custom_keyword! { map }
}

/// Helper for zip-spec-trait implementation.
///
/// This macro is not meant to be used directly. The main fast_expr procedural macro will generate
/// custom helper macros for your ADTs which will rely on this procedural macro.
#[proc_macro]
pub fn fast_expr_impl(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut instance = syn::parse_macro_input!(stream as Instance);

    let tokens = match instance.work() {
        Ok(()) => {
            let tokens = instance.to_token_stream();
            quote! {
                pub const OUTPUT: &str = stringify!(#tokens);
            }
        }
        Err(e) => e.to_compile_error(),
    };
    proc_macro::TokenStream::from(tokens)
}

struct Instance {
    pub zip_trait: rust::Trait,
    pub expr_map: expr_map::ExprMap,
    pub exprs: Vec<expr_zip::ExprZip>,
    pub user_typ_map: Map<rust::Id, TokenStream>,
    pub user_fn_map: Map<
        rust::Id,
        (
            Vec<TokenStream>,
            Vec<(TokenStream, TokenStream)>,
            // TokenStream,
        ),
    >,
}

impl Parse for Instance {
    fn parse(input: ParseStream) -> Res<Self> {
        let mut zip_trait = rust::Trait::parse(input)?;
        let expr_map = input.parse()?;
        let mut exprs = vec![];
        while !input.is_empty() {
            exprs.push(input.parse()?)
        }

        // Filter out methods that already have an implementation (default).
        zip_trait.items = zip_trait
            .items
            .into_iter()
            .filter(|item| {
                if let syn::TraitItem::Method(method) = item {
                    method.default.is_none()
                } else {
                    true
                }
            })
            .collect();

        Ok(Self {
            zip_trait,
            expr_map,
            exprs,
            user_typ_map: Map::new(),
            user_fn_map: Map::new(),
        })
    }
}

impl ToTokens for Instance {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.zip_trait.to_tokens(tokens);
        self.expr_map.to_tokens(tokens);
        let user_typ_map = self
            .user_typ_map
            .iter()
            .map(|(id, def)| quote! { #id => #def });
        let user_fn_map = self.user_fn_map.iter().map(|(id, (args, redefs))| {
            if redefs.is_empty() {
                quote! { #id { #(#args),* } }
            } else {
                let redefs = redefs.iter().map(|(src, tgt)| quote! { #tgt <- #src });
                quote! { #id {
                    #(#args),* where #(#redefs),*
                } }
            }
        });
        tokens.extend(quote! {
            {
                #(#user_typ_map),* ;
                #(#user_fn_map),* ;
            }
        })
    }
}

impl Instance {
    fn work(&mut self) -> Res<()> {
        for expr_zip in &self.exprs {
            let expr_val = self.expr_map.get(&expr_zip.e_id)?;

            // Result type.
            let res_typ = &expr_zip.res_typ;
            let _prev = self
                .user_typ_map
                .insert(expr_val.res_typ_id.clone(), res_typ.to_token_stream());
            if _prev.is_some() {
                panic!("[internal] non-empty user type map")
            }

            for variant_zip in &expr_zip.variants {
                if let Some(v_id) = variant_zip.v_id() {
                    let variant_val = expr_val.get(v_id)?;

                    let go_up_id = &variant_val.go_up_id;
                    let (args, redefs): (Vec<_>, Vec<_>) = variant_zip.bindings_do(
                        |pats| {
                            let (mut args, mut redefs) = (vec![], vec![]);

                            for field_pat in pats {
                                let d_id = match &field_pat.member {
                                    syn::Member::Named(d_id) => d_id,
                                    syn::Member::Unnamed(idx) => {
                                        return Err(syn::Error::new_spanned(
                                            idx,
                                            "expected identifier",
                                        ));
                                    }
                                };
                                args.push(d_id.to_token_stream());
                                if field_pat.colon_token.is_some() {
                                    redefs.push((
                                        d_id.to_token_stream(),
                                        field_pat.pat.to_token_stream(),
                                    ))
                                }
                            }

                            Ok((args, redefs))
                        },
                        |pats| {
                            Ok((
                                pats.iter().map(|pat| pat.to_token_stream()).collect(),
                                vec![],
                            ))
                        },
                    )?;

                    let _prev = self.user_fn_map.insert(go_up_id.clone(), (args, redefs));
                    if _prev.is_some() {
                        return Err(syn::Error::new_spanned(
                            v_id,
                            format!("trying to zip variant `{}` twice", v_id.to_token_stream()),
                        ));
                    }
                } else {
                    return Err(syn::Error::new_spanned(
                        &variant_zip.bindings,
                        "illegal variant pattern",
                    ));
                }
            }
        }
        Ok(())
    }
}
