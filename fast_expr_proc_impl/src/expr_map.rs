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
    pub desc: VariantDesc,
    pub go_up_id: rust::Id,
    pub colls: Punctuated<CollDesc, rust::Token![,]>,
}
impl ToTokens for VariantVal {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            v_id,
            desc,
            go_up_id,
            colls,
            paren: _paren,
        } = self;
        let colls = colls.iter();
        tokens.extend(quote! {
            #v_id(#desc, #go_up_id #(, #colls)*)
        })
    }
}
impl Parse for VariantVal {
    fn parse(input: ParseStream) -> Res<Self> {
        let v_id = input.parse()?;

        let paren_content;
        let paren = syn::parenthesized!(paren_content in input);
        let desc = paren_content.parse()?;
        let _: rust::Token![,] = paren_content.parse()?;
        let go_up_id = paren_content.parse()?;

        let colls = if !paren_content.is_empty() {
            let _: rust::Token![,] = paren_content.parse()?;
            paren_content.parse_terminated(CollDesc::parse)?
        } else {
            Punctuated::new()
        };

        Ok(Self {
            v_id,
            paren,
            desc,
            go_up_id,
            colls,
        })
    }
}

pub enum VariantDesc {
    Struct {
        brace: syn::token::Brace,
        fields: Punctuated<rust::Id, rust::Token![,]>,
    },
    Tuple(syn::LitInt),
    UnitStruct,
}
impl ToTokens for VariantDesc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Struct {
                brace: _brace,
                fields,
            } => quote! { { #fields } }.to_tokens(tokens),
            Self::Tuple(len) => len.to_tokens(tokens),
            Self::UnitStruct => quote!(_).to_tokens(tokens),
        }
    }
}
impl Parse for VariantDesc {
    fn parse(input: ParseStream) -> Res<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(rust::Token![_]) {
            let _: rust::Token![_] = input.parse()?;
            Ok(Self::UnitStruct)
        } else if lookahead.peek(syn::LitInt) {
            let lit_int: syn::LitInt = input.parse()?;
            if lit_int.suffix() == "usize" {
                Ok(Self::Tuple(lit_int))
            } else {
                Err(syn::Error::new(
                    lit_int.span(),
                    format!(
                        "[VariantDesc] unexpected integer suffix `{}`",
                        lit_int.suffix()
                    ),
                ))
            }
        } else if lookahead.peek(syn::token::Brace) {
            let brace_content;
            let brace = syn::braced!(brace_content in input);
            let fields = brace_content.parse_terminated(rust::Id::parse)?;
            Ok(Self::Struct { brace, fields })
        } else {
            Err(lookahead.error())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CollData {
    Field(rust::Id),
    Idx(syn::LitInt),
}
impl CollData {
    #[allow(dead_code)]
    pub fn span(&self) -> proc_macro2::Span {
        match self {
            Self::Field(id) => id.span(),
            Self::Idx(idx) => idx.span(),
        }
    }
}
impl ToTokens for CollData {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Field(id) => id.to_tokens(tokens),
            Self::Idx(idx) => idx.to_tokens(tokens),
        }
    }
}
impl Parse for CollData {
    fn parse(input: ParseStream) -> Res<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(rust::Id) {
            let id = input.parse()?;
            Ok(Self::Field(id))
        } else if lookahead.peek(syn::LitInt) {
            let lit_int: syn::LitInt = input.parse()?;
            if lit_int.suffix() == "usize" {
                Ok(Self::Idx(lit_int))
            } else {
                Err(syn::Error::new(
                    lit_int.span(),
                    format!(
                        "[CollData] unexpected integer literal suffix `{}`",
                        lit_int.suffix()
                    ),
                ))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

pub struct CollDesc {
    pub data: CollData,
    pub fat_arrow: rust::Token![=>],
    pub brace: syn::token::Brace,
    pub acc_typ: rust::Id,
    pub init_fn: rust::Id,
    pub fold_fn: rust::Id,
}
impl ToTokens for CollDesc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self {
            data,
            fat_arrow,
            acc_typ,
            init_fn,
            fold_fn,
            brace: _brace,
        } = self;
        tokens.extend(quote! { #data #fat_arrow { #acc_typ, #init_fn, #fold_fn} })
    }
}
impl Parse for CollDesc {
    fn parse(input: ParseStream) -> Res<Self> {
        // panic!("coll desc bad: {}", input);
        // let paren_content;
        // let paren = syn::parenthesized!(paren_content in input);
        // panic!("coll desc okay");

        let data = input.parse()?;
        let fat_arrow: rust::Token![=>] = input.parse()?;

        let brace_content;
        let brace = syn::braced!(brace_content in input);

        let acc_typ = brace_content.parse()?;
        let _: rust::Token![,] = brace_content.parse()?;

        let init_fn = brace_content.parse()?;
        let _: rust::Token![,] = brace_content.parse()?;

        let fold_fn = brace_content.parse()?;
        if !brace_content.is_empty() {
            let _: rust::Token![,] = brace_content.parse()?;
            if !brace_content.is_empty() {
                return Err(brace_content.error("expected closing brace"));
            }
        }

        Ok(Self {
            data,
            fat_arrow,
            brace,
            acc_typ,
            init_fn,
            fold_fn,
        })
    }
}
