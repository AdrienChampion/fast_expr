//! Structures for the user-input part of the macro.

// use std::{collections::BTreeMap as Map, ops::Deref};

fast_expr_gen::prelude! {}

use crate::keyword;

use fast_expr_gen::syn::{
    self,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

use syn::{PatStruct, PatTupleStruct, PatWild};

pub struct ExprZip {
    pub key: keyword::zip,
    pub paren: syn::token::Paren,
    pub e_id: rust::Id,
    pub e_lt: Option<rust::Lifetime>,
    pub e_targs: syn::PathArguments,
    pub e_param_id: rust::Id,
    pub colon: syn::Token![:],
    pub expr_typ: rust::Typ,
    pub fat_arrow: syn::Token![=>],
    pub res_typ: rust::Typ,
    pub brace: syn::token::Brace,
    pub variants: Punctuated<FullVariantZip, syn::Token![,]>,
}
impl Parse for ExprZip {
    fn parse(input: ParseStream) -> Res<Self> {
        let key = input.parse()?;

        let paren_content;
        let paren = syn::parenthesized!(paren_content in input);
        let e_param_id = paren_content.parse()?;
        let colon = paren_content.parse()?;
        let expr_typ = paren_content.parse()?;
        let fat_arrow = paren_content.parse()?;
        let res_typ = paren_content.parse()?;
        if !paren_content.is_empty() {
            return Err(paren_content.error("expected closing paren"));
        }

        let brace_content;
        let brace = syn::braced!(brace_content in input);
        let variants = brace_content.parse_terminated(FullVariantZip::parse)?;

        let (e_id, e_targs, e_lt) = {
            let mut e_lt = None;
            let mut expr_typ = &expr_typ;
            let mut first = true;
            'find_id: loop {
                match expr_typ {
                    rust::Typ::Reference(reference) => {
                        e_lt = reference.lifetime.clone();
                        if let Some(mutable) = reference.mutability {
                            return Err(syn::Error::new_spanned(
                                mutable,
                                "unsupported mutable expression reference",
                            ));
                        }
                        expr_typ = &*reference.elem;
                    }
                    rust::Typ::Path(path) => {
                        if path.qself.is_some() {
                            return Err(syn::Error::new_spanned(
                                path,
                                "expected expression type path",
                            ));
                        }
                        if let Some(last_segment) = path.path.segments.iter().next_back() {
                            break 'find_id (
                                last_segment.ident.clone(),
                                last_segment.arguments.clone(),
                                e_lt,
                            );
                        } else {
                            return Err(syn::Error::new_spanned(
                                path,
                                "not a legal path to an expression type",
                            ));
                        }
                    }
                    other => {
                        return Err(syn::Error::new_spanned(
                            other,
                            format!(
                                "expected {}expression type",
                                if first { "(reference of an) " } else { "" }
                            ),
                        ))
                    }
                }
                first = false;
            }
        };

        Ok(Self {
            key,
            paren,
            e_id,
            e_lt,
            e_targs,
            e_param_id,
            colon,
            expr_typ,
            fat_arrow,
            res_typ,
            brace,
            variants,
        })
    }
}

pub struct FullVariantZip {
    pub v_id: rust::Id,
    pub bindings: Bindings,
    pub fat_arrow: syn::Token![=>],
    pub zip: VariantZip,
}
impl FullVariantZip {
    pub fn v_id(&self) -> &rust::Id {
        &self.v_id
    }
    pub fn bindings_do<'a, T>(
        &'a self,
        struct_like_do: impl FnOnce(&'a Punctuated<syn::FieldPat, rust::Token![,]>) -> Res<T>,
        tuple_like_do: impl FnOnce(&'a Punctuated<syn::Pat, rust::Token![,]>) -> Res<T>,
    ) -> Res<T> {
        self.bindings.bindings_do(struct_like_do, tuple_like_do)
    }
    pub fn go_up_def(&self) -> (Option<&Vec<syn::Attribute>>, &rust::Expr) {
        self.zip.go_up_def()
    }
    pub fn get_multi(&self) -> Option<&Multi> {
        self.zip.get_multi()
    }
}
impl Parse for FullVariantZip {
    fn parse(input: ParseStream) -> Res<Self> {
        let bindings: Bindings = input.parse()?;
        let fat_arrow = input.parse()?;
        let zip = input.parse()?;
        let v_id = bindings.v_id()?.clone();
        Ok(Self {
            v_id,
            bindings,
            fat_arrow,
            zip,
        })
    }
}

pub enum Bindings {
    Struct(PatStruct),
    Tuple(PatTupleStruct),
    Wild(PatWild),
}
impl Bindings {
    pub fn v_id(&self) -> Res<&rust::Id> {
        let path = match self {
            Self::Struct(pat) => &pat.path,
            Self::Tuple(pat) => &pat.path,
            Self::Wild(wild) => {
                return Err(syn::Error::new_spanned(
                    wild,
                    "unsupported wildcard in variant pattern",
                ))
            }
        };
        path.get_ident()
            .ok_or_else(|| syn::Error::new_spanned(path, "expected variant identifier"))
    }

    // pub get_id_from(&self, id: crate::expr_map::CollData) -> Res<&rust::Id> {
    //     use crate::expr_map::CollData;
    //     match (self, id) {
    //         (self::Struct(pat), CollData::Field(id)) => {
    //             let mut res = None;
    //             for field in &pat.fields {
    //                 if field.colon_token.is_some() {
    //                     bail!(syn::Error::new_spanned(field.pat, "unexpected pattern"))
    //                 }
    //                 match &field.member {
    //                     syn::Member::Named(pat_id) => if id == pat_id {
    //                         Ok(pat_id)
    //                     } else {
    //                         continue;
    //                     },
    //                     syn::Member::Unnamed(_) => bail!(
    //                         syn::Error::new_spanned(field.member, "unexpected unnamed pattern")
    //                     ),
    //                 }
    //             }
    //             res.ok_or_else(syn::Error::new_spanned(id), "unknown identifier")
    //         }
    //         (Self::Tuple(pat), CollData::Idx(idx)) => {
    //             let mut idx = idx.base10_parse()?;
    //             for pat in &pat.pat.elems {
    //                 if idx == 0 {
    //                     match pat {
    //                         syn::Pat::Ident(id) => return Ok(id),
    //                         pat => bail!(syn::Error::new_spanned(pat, "unexpected pattern")),
    //                     }
    //                 } else {
    //                     continue;
    //                 }
    //             }

    //         }
    //     }
    // }

    pub fn bindings_do<'a, T>(
        &'a self,
        struct_like_do: impl FnOnce(&'a Punctuated<syn::FieldPat, rust::Token![,]>) -> Res<T>,
        tuple_like_do: impl FnOnce(&'a Punctuated<syn::Pat, rust::Token![,]>) -> Res<T>,
    ) -> Res<T> {
        match self {
            Self::Struct(pat) => struct_like_do(&pat.fields),
            Self::Tuple(pat) => tuple_like_do(&pat.pat.elems),
            Self::Wild(wild) => {
                return Err(syn::Error::new_spanned(
                    wild,
                    "unsupported wildcard in variant pattern",
                ))
            }
        }
    }
}
impl Parse for Bindings {
    fn parse(input: ParseStream) -> Res<Self> {
        let pat = syn::Pat::parse(input)?;
        match pat {
            syn::Pat::Struct(pat) => Ok(Self::Struct(pat)),
            syn::Pat::TupleStruct(pat) => Ok(Self::Tuple(pat)),
            syn::Pat::Wild(pat) => Ok(Self::Wild(pat)),
            pat => Err(syn::Error::new_spanned(
                pat,
                "expected `_`, `Variant { ... }` or `Variant(...)`",
            )),
        }
    }
}
impl ToTokens for Bindings {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Struct(pat) => pat.to_tokens(tokens),
            Self::Tuple(pat) => pat.to_tokens(tokens),
            Self::Wild(pat) => pat.to_tokens(tokens),
        }
    }
}

pub enum VariantZip {
    Single(GoUpDef),
    Multi(Multi),
}
impl VariantZip {
    pub fn go_up_def(&self) -> (Option<&Vec<syn::Attribute>>, &rust::Expr) {
        match self {
            Self::Single(go_up) => (None, &go_up.expr),
            Self::Multi(multi) => multi.go_up_def(),
        }
    }
    pub fn get_multi(&self) -> Option<&Multi> {
        match self {
            Self::Single(_) => None,
            Self::Multi(multi) => Some(multi),
        }
    }
}
impl Parse for VariantZip {
    fn parse(input: ParseStream) -> Res<Self> {
        let span = input.span();
        let lookahead = input.lookahead1();

        if Multi::peek(&lookahead) {
            Ok(Self::Multi(input.parse()?))
        } else {
            match GoUpDef::parse(input) {
                Ok(go_up) => Ok(Self::Single(go_up)),
                Err(mut e) => {
                    e.extend(syn::Error::new(
                        span,
                        format!("expected `{}` keyword, or expression", {
                            let key = keyword::zip { span: span };
                            quote!(#key)
                        }),
                    ));
                    return Err(e);
                }
            }
        }
    }
}

pub struct Multi {
    pub key: keyword::zip,
    pub brace: syn::token::Brace,
    pub go_up: GoUp,
    pub folds: Vec<Fold>,
}
impl Multi {
    pub fn peek(lookahead: &syn::parse::Lookahead1) -> bool {
        lookahead.peek(keyword::zip)
    }
    pub fn go_up_def(&self) -> (Option<&Vec<syn::Attribute>>, &rust::Expr) {
        (Some(&self.go_up.attrs), self.go_up.def())
    }
    pub fn folds(&self) -> &[Fold] {
        &self.folds
    }
}
impl Parse for Multi {
    fn parse(input: ParseStream) -> Res<Self> {
        let span = input.span();
        let key = input.parse()?;
        let brace_content;
        let brace = syn::braced!(brace_content in input);
        let list: Punctuated<GoUpOrFold, syn::Token![,]> =
            brace_content.parse_terminated(GoUpOrFold::parse)?;
        let mut go_up = None;
        let mut folds = vec![];

        for item in list {
            match item {
                GoUpOrFold::GoUp(up) => {
                    go_up = if let None = go_up {
                        Some(up)
                    } else {
                        return Err(syn::Error::new_spanned(
                            up.key,
                            "only one `go_up` per variant is allowed",
                        ));
                    }
                }
                GoUpOrFold::Fold(fold) => folds.push(fold),
            }
        }

        let go_up = go_up.ok_or_else(|| syn::Error::new(span, "no `go_up` operation found"))?;

        Ok(Self {
            key,
            brace,
            go_up,
            folds,
        })
    }
}

pub enum GoUpOrFold {
    GoUp(GoUp),
    Fold(Fold),
}
impl Parse for GoUpOrFold {
    fn parse(input: ParseStream) -> Res<Self> {
        let attrs = syn::Attribute::parse_outer(input)?;
        let lookahead = input.lookahead1();

        if lookahead.peek(keyword::go_up) {
            let mut go_up = GoUp::parse(input)?;
            go_up.attrs = attrs;
            Ok(Self::GoUp(go_up))
        } else if lookahead.peek(keyword::fold) {
            let mut fold = Fold::parse(input)?;
            fold.attrs = attrs;
            Ok(Self::Fold(fold))
        } else {
            Err(lookahead.error())
        }
    }
}

pub struct GoUp {
    pub attrs: Vec<syn::Attribute>,
    pub key: keyword::go_up,
    pub fat_arrow: syn::Token![=>],
    pub def: GoUpDef,
}
impl GoUp {
    pub fn def(&self) -> &rust::Expr {
        &self.def.expr
    }
}
impl Parse for GoUp {
    fn parse(input: ParseStream) -> Res<Self> {
        Ok(Self {
            attrs: vec![],
            key: input.parse()?,
            fat_arrow: input.parse()?,
            def: input.parse()?,
        })
    }
}

pub struct GoUpDef {
    pub expr: rust::Expr,
}
impl Parse for GoUpDef {
    fn parse(input: ParseStream) -> Res<Self> {
        Ok(Self {
            expr: rust::Expr::parse(input)?,
        })
    }
}

pub struct Fold {
    pub attrs: Vec<syn::Attribute>,
    pub key: keyword::fold,
    pub paren: syn::token::Paren,
    pub field: rust::Id,
    pub target: Option<(syn::Token![=>], rust::Typ)>,
    pub brace: syn::token::Brace,
    pub init: Init,
    pub step: Step,
}
impl Parse for Fold {
    fn parse(input: ParseStream) -> Res<Self> {
        let key = input.parse()?;

        let paren_content;
        let paren = syn::parenthesized!(paren_content in input);
        let field = paren_content.parse()?;
        let target = if paren_content.is_empty() {
            None
        } else {
            Some((paren_content.parse()?, paren_content.parse()?))
        };
        if !paren_content.is_empty() {
            return Err(paren_content.error("expected `)`"));
        }

        let brace_content;
        let brace = syn::braced!(brace_content in input);
        let init = brace_content.parse()?;
        let _: syn::Token![,] = brace_content.parse()?;
        let step = brace_content.parse()?;

        if !brace_content.is_empty() {
            let _: syn::Token![,] = brace_content.parse()?;
        }

        Ok(Self {
            attrs: vec![],
            key,
            paren,
            field,
            target,
            brace,
            init,
            step,
        })
    }
}

pub struct Init {
    pub attrs: Vec<syn::Attribute>,
    pub key: keyword::init,
    pub fat_arrow: syn::Token![=>],
    pub expr: rust::Expr,
}
impl Parse for Init {
    fn parse(input: ParseStream) -> Res<Self> {
        let attrs = if !input.peek(keyword::init) {
            syn::Attribute::parse_outer(input)?
        } else {
            vec![]
        };
        Ok(Self {
            attrs,
            key: input.parse()?,
            fat_arrow: input.parse()?,
            expr: input.parse()?,
        })
    }
}

pub struct Step {
    pub attrs: Vec<syn::Attribute>,
    pub key: keyword::step,
    pub paren: syn::token::Paren,
    pub acc_pat: syn::Pat,
    pub next_pat: syn::Pat,
    pub fat_arrow: syn::Token![=>],
    pub expr: syn::Expr,
}
impl Parse for Step {
    fn parse(input: ParseStream) -> Res<Self> {
        let attrs = if !input.peek(keyword::step) {
            syn::Attribute::parse_outer(input)?
        } else {
            vec![]
        };
        let key = input.parse()?;
        let content;
        let paren = syn::parenthesized!(content in input);
        let args: Punctuated<syn::Pat, syn::Token![,]> = Punctuated::parse_terminated(&content)?;
        let mut args = args.into_iter();

        let acc_pat = args.next().ok_or_else(|| {
            syn::Error::new(
                paren.span,
                "expected two parameters (accumulator and next value), found nothing",
            )
        })?;
        let next_pat = args.next().ok_or_else(|| {
            syn::Error::new(
                paren.span,
                "expected two parameters (accumulator and next value), found only one",
            )
        })?;
        if let Some(arg) = args.next() {
            return Err(syn::Error::new_spanned(
                arg,
                "expected exactly two parameters (accumulator and next value)",
            ));
        }

        let fat_arrow = input.parse()?;
        let expr = input.parse()?;

        Ok(Self {
            attrs,
            key,
            paren,
            acc_pat,
            next_pat,
            fat_arrow,
            expr,
        })
    }
}
