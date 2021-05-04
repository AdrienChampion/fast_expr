//! Structures for the expression-map part of the macro.

use std::collections::BTreeMap as Map;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn::{
    self,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

use crate::keyword;

pub struct ExprMap {
    pub key: keyword::map,
    pub brace: syn::token::Brace,
    pub map: Map<rust::Id, ExprVal>,
}
impl ExprMap {
    pub fn get(&self, e_id: &rust::Id) -> Res<&ExprVal> {
        self.map.get(e_id).ok_or_else(|| {
            let mut msg = format!(
                "unknown expression ID `{}`, expected{}",
                e_id.to_token_stream(),
                if self.map.len() > 1 { "one of" } else { "" },
            );
            let mut pref = " ";
            for e_id in self.map.keys() {
                msg.push_str(&format!("{}`{}`", pref, e_id.to_token_stream()));
                pref = ", ";
            }
            syn::Error::new_spanned(e_id, msg)
        })
    }
}
impl ToTokens for ExprMap {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let key = &self.key;
        let map = self.map.values();
        tokens.extend(quote! {
            #key {
                #(#map),*
            }
        })
    }
}
impl Parse for ExprMap {
    fn parse(input: ParseStream) -> Res<Self> {
        let key = input.parse()?;

        let (brace, map) = {
            let brace_content;
            let brace = syn::braced!(brace_content in input);
            let map = brace_content.parse_terminated::<_, rust::Token![,]>(ExprVal::parse)?;

            (brace, map)
        };

        let mut h_map = Map::new();
        for expr_val in map {
            let span = expr_val.e_id.span();
            let _prev = h_map.insert(expr_val.e_id.clone(), expr_val);
            if _prev.is_some() {
                return Err(syn::Error::new(
                    span,
                    "expression identifiers must be unique",
                ));
            }
        }

        Ok(Self {
            key,
            brace,
            map: h_map,
        })
    }
}

pub struct ExprVal {
    pub e_id: rust::Id,
    pub paren: syn::token::Paren,
    pub res_typ_id: rust::Id,
    pub map: Map<rust::Id, VariantVal>,
}
impl ExprVal {
    pub fn get(&self, v_id: &rust::Id) -> Res<&VariantVal> {
        self.map.get(v_id).ok_or_else(|| {
            let mut msg = format!(
                "unknown variant ID `{}`, expected{}",
                v_id.to_token_stream(),
                if self.map.len() > 1 { "one of" } else { "" },
            );
            let mut pref = " ";
            for e_id in self.map.keys() {
                msg.push_str(&format!("{}`{}`", pref, e_id.to_token_stream()));
                pref = ", ";
            }
            syn::Error::new_spanned(v_id, msg)
        })
    }
}
impl ToTokens for ExprVal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let e_id = &self.e_id;
        let res_typ_id = &self.res_typ_id;
        let map = self.map.values();
        tokens.extend(quote! {
            #e_id(#res_typ_id #(, #map)*)
        })
    }
}
impl Parse for ExprVal {
    fn parse(input: ParseStream) -> Res<Self> {
        let e_id = input.parse()?;

        let (paren, res_typ_id, map) = {
            let paren_content;
            let paren = syn::parenthesized!(paren_content in input);
            let res_typ_id = paren_content.parse()?;
            let _: rust::Token![,] = paren_content.parse()?;
            let map = paren_content.parse_terminated::<_, rust::Token![,]>(VariantVal::parse)?;

            let mut h_map = Map::new();
            for variant_val in map {
                let id_span = variant_val.v_id.span();
                let _prev = h_map.insert(variant_val.v_id.clone(), variant_val);
                if _prev.is_some() {
                    return Err(syn::Error::new(
                        id_span,
                        "variant identifiers must be unique",
                    ));
                }
            }

            (paren, res_typ_id, h_map)
        };

        Ok(Self {
            e_id,
            paren,
            res_typ_id,
            map,
        })
    }
}

pub struct VariantVal {
    pub v_id: rust::Id,
    pub paren: syn::token::Paren,
    pub go_up_id: rust::Id,
    pub colls: Punctuated<CollDesc, rust::Token![,]>,
}
impl ToTokens for VariantVal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            v_id,
            go_up_id,
            colls,
            paren: _paren,
        } = self;
        let colls = colls.iter();
        tokens.extend(quote! {
            #v_id(#go_up_id #(, #colls)*)
        })
    }
}
impl Parse for VariantVal {
    fn parse(input: ParseStream) -> Res<Self> {
        let v_id = input.parse()?;

        let (paren, go_up_id, colls) = {
            let paren_content;
            let paren = syn::parenthesized!(paren_content in input);
            let go_up_id = paren_content.parse()?;

            let colls = if !paren_content.is_empty() {
                let _: rust::Token![,] = paren_content.parse()?;
                paren_content.parse_terminated(CollDesc::parse)?
            } else {
                Punctuated::new()
            };

            (paren, go_up_id, colls)
        };

        Ok(Self {
            v_id,
            paren,
            go_up_id,
            colls,
        })
    }
}

pub struct CollDesc {
    pub paren: syn::token::Paren,
    pub acc_typ: rust::Id,
    pub init_fn: rust::Id,
    pub fold_fn: rust::Id,
}
impl ToTokens for CollDesc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            acc_typ,
            init_fn,
            fold_fn,
            paren: _paren,
        } = self;
        tokens.extend(quote! { (#acc_typ, #init_fn, #fold_fn) })
    }
}
impl Parse for CollDesc {
    fn parse(input: ParseStream) -> Res<Self> {
        let paren_content;
        let paren = syn::parenthesized!(paren_content in input);

        let acc_typ = paren_content.parse()?;
        let _: rust::Token![,] = paren_content.parse()?;

        let init_fn = paren_content.parse()?;
        let _: rust::Token![,] = paren_content.parse()?;

        let fold_fn = paren_content.parse()?;
        if !paren_content.is_empty() {
            let _: rust::Token![,] = paren_content.parse()?;
            if !paren_content.is_empty() {
                return Err(paren_content.error("expected closing parenthesis"));
            }
        }

        Ok(Self {
            paren,
            acc_typ,
            init_fn,
            fold_fn,
        })
    }
}
