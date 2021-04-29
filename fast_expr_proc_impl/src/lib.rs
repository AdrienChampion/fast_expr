//! Zip-spec-trait implementation procedural macro.
//!
//! This macro is a helper for end-user to implement zip-spec-traits.

extern crate proc_macro;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn;

/// Helper for zip-spec-trait implementation.
///
/// This macro is not meant to be used directly. The main fast_expr procedural macro will generate
/// custom helper macros for your ADTs which will rely on this procedural macro.
#[proc_macro]
pub fn fast_expr_impl(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let instance = syn::parse_macro_input!(stream as instance::Instance);

    let tokens = instance.to_token_stream();
    let res_tokens = quote! {
        pub const OUTPUT: &str = stringify!(#tokens);
    };
    proc_macro::TokenStream::from(res_tokens)
}

mod instance {
    fast_expr_gen::prelude! {}

    use fast_expr_gen::syn::{
        self,
        parse::{Parse, ParseStream},
        punctuated::Punctuated,
    };

    pub mod keyword {
        use super::*;

        syn::custom_keyword! { zip }
        syn::custom_keyword! { init }
        syn::custom_keyword! { step }
        syn::custom_keyword! { fold }
        syn::custom_keyword! { go_up }
    }

    pub struct Instance {
        pub zip_trait: rust::Trait,
        pub exprs: Vec<ExprZip>,
    }

    impl Parse for Instance {
        fn parse(input: ParseStream) -> Res<Self> {
            let mut zip_trait = rust::Trait::parse(input)?;
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
            Ok(Self { zip_trait, exprs })
        }
    }

    impl ToTokens for Instance {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.zip_trait.to_tokens(tokens)
        }
    }

    pub struct ExprZip {
        pub key: keyword::zip,
        pub paren: syn::token::Paren,
        pub expr_ident: rust::Id,
        pub colon: syn::Token![:],
        pub expr_ty: rust::Typ,
        pub fat_arrow: syn::Token![=>],
        pub res_ty: rust::Typ,
        pub brace: syn::token::Brace,
        pub variants: Punctuated<variant::FullVariantZip, syn::Token![,]>,
    }
    impl Parse for ExprZip {
        fn parse(input: ParseStream) -> Res<Self> {
            let key = input.parse()?;

            let paren_content;
            let paren = syn::parenthesized!(paren_content in input);
            let expr_ident = paren_content.parse()?;
            let colon = paren_content.parse()?;
            let expr_ty = paren_content.parse()?;
            let fat_arrow = paren_content.parse()?;
            let res_ty = paren_content.parse()?;
            if !paren_content.is_empty() {
                return Err(paren_content.error("expected closing paren"));
            }

            let brace_content;
            let brace = syn::braced!(brace_content in input);
            let variants = brace_content.parse_terminated(variant::FullVariantZip::parse)?;

            Ok(Self {
                key,
                paren,
                expr_ident,
                colon,
                expr_ty,
                fat_arrow,
                res_ty,
                brace,
                variants,
            })
        }
    }

    pub mod variant {
        use super::*;

        use syn::{PatStruct, PatTupleStruct, PatWild};

        pub struct FullVariantZip {
            pub bindings: Bindings,
            pub fat_arrow: syn::Token![=>],
            pub zip: VariantZip,
        }
        impl Parse for FullVariantZip {
            fn parse(input: ParseStream) -> Res<Self> {
                Ok(Self {
                    bindings: input.parse()?,
                    fat_arrow: input.parse()?,
                    zip: input.parse()?,
                })
            }
        }

        pub enum Bindings {
            Struct(PatStruct),
            Tuple(PatTupleStruct),
            Wild(PatWild),
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

        pub enum VariantZip {
            Single(GoUpDef),
            Multi(Multi),
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

                let go_up =
                    go_up.ok_or_else(|| syn::Error::new(span, "no `go_up` operation found"))?;

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
                let lookahead = input.lookahead1();

                if lookahead.peek(keyword::go_up) {
                    Ok(Self::GoUp(input.parse()?))
                } else if lookahead.peek(keyword::fold) {
                    Ok(Self::Fold(input.parse()?))
                } else {
                    Err(lookahead.error())
                }
            }
        }

        pub struct GoUp {
            pub key: keyword::go_up,
            pub fat_arrow: syn::Token![=>],
            pub def: GoUpDef,
        }
        impl Parse for GoUp {
            fn parse(input: ParseStream) -> Res<Self> {
                Ok(Self {
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
            pub key: keyword::init,
            pub fat_arrow: syn::Token![=>],
            pub expr: rust::Expr,
        }
        impl Parse for Init {
            fn parse(input: ParseStream) -> Res<Self> {
                Ok(Self {
                    key: input.parse()?,
                    fat_arrow: input.parse()?,
                    expr: input.parse()?,
                })
            }
        }

        pub struct Step {
            pub key: keyword::step,
            pub paren: syn::token::Paren,
            pub acc_pat: syn::Pat,
            pub next_pat: syn::Pat,
            pub fat_arrow: syn::Token![=>],
            pub expr: syn::Expr,
        }
        impl Parse for Step {
            fn parse(input: ParseStream) -> Res<Self> {
                let key = input.parse()?;
                let content;
                let paren = syn::parenthesized!(content in input);
                let args: Punctuated<syn::Pat, syn::Token![,]> =
                    Punctuated::parse_terminated(&content)?;
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
                    key,
                    paren,
                    acc_pat,
                    next_pat,
                    fat_arrow,
                    expr,
                })
            }
        }
    }
}
