//! Re-exports and common type/helpers used throughout fast expr.

pub use std::{
    collections::{BTreeMap as Map, BTreeSet as Set},
    convert::{TryFrom, TryInto},
    fmt::Display,
    io::Write,
    ops::Deref,
    path::PathBuf,
};

pub trait AsPath: AsRef<std::path::Path> {}
impl<T> AsPath for T where T: AsRef<std::path::Path> {}

pub use either::Either;
pub use lazy_static::lazy_static;
pub use proc_macro2::{Span, TokenStream};
pub use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
pub use smallvec::smallvec;
pub use syn::{
    parse::{Parse, ParseStream},
    Error,
};

pub use crate::{
    check, conf,
    cxt::{self, Cxt},
    doc,
    err::{self, Result as Res},
    expr, front, gen, log, logln, rust,
};

/// Used to indicate whether something was generated.
pub type WasGenerated = bool;

/// Used to distinguish between an owned and referenced value or type.
pub type IsOwn = bool;

/// Boolean indicating whether some data is a recursive collection.
pub type IsColl = bool;

/// Used to distinguish between tuple-like and struct-like variants.
pub type IsTupleLike = bool;

/// Alias for a small vector storing up to 8 elements on the stack.
pub type SVec<T> = smallvec::SmallVec<[T; 8]>;

/// Custom pair type so that we can implement traits for it.
pub struct Pair<L, R> {
    pub lft: L,
    pub rgt: R,
}
implement! {
    impl(L, R) Pair<L, R> {
        From<(L, R)> {
            |pair| Pair { lft: pair.0, rgt: pair.1 }
        }
    }
}

/// Call-site span.
pub fn default_span() -> proc_macro2::Span {
    proc_macro2::Span::call_site()
}

/// Safe indices.
pub mod idx {
    use super::*;

    safe_index::new! {
        /// Data indices.
        Data,
        /// Map from data indices to something.
        map: DataMap with iter: DataIter,
    }

    safe_index::new! {
        /// Variant indices.
        Variant,
        /// Map from variant indices to something.
        map: VariantMap with iter: VariantIter,
    }

    safe_index::new! {
        /// Expression indices.
        Expr,
        /// Map from expression indices to something.
        map: ExprMap with iter: ExprIter,
    }

    safe_index::new! {
        /// Collection indices.
        Coll,
        /// Map from collection indices to something.
        map: CollMap with iter: CollIter,
    }

    safe_index::new! {
        /// Expression type parameter indices.
        TParam,
        /// Map from expression type parameter indices to something.
        map: TParamMap with iter: TParamIter,
    }

    safe_index::new! {
        /// Zipper type parameter indices.
        ZipTParam,
        /// Map from zipper type parameter indices to something.
        map: ZipTParamMap with iter: ZipTParamIter,
    }

    safe_index::new! {
        /// Top-level type parameter.
        TopTParam,
        /// Map from top-level type parameter indices to something.
        map: TopTParamMap with iter: TopTParamIter,
    }

    safe_index::new! {
        /// Index for a builtin type.
        ///
        /// **NEVER** create a `TypPath` value from an integer of the code surrounding its
        /// definition. Even then, make sure you know what you are doing.
        TypPath,
        /// Map from top-level type parameter indices to something.
        map: TypPathMap with iter: TypPathIter,
    }

    macro_rules! idx_syn_quote {
        {
            $($kw:ident for $ty:ty),* $(,)?
        } => {
            pub mod kw {
                $( syn::custom_keyword!($kw); )*
            }

            $(
                impl_parse_and_tokens! {
                    for $ty {
                        fn parse(input) -> Res<Self> {
                            let _ = kw::$kw::parse(input)?;
                            let (_, content) = braced!(input);
                            let idx: syn::LitInt = content.parse()?;
                            let idx: usize = idx.base10_parse()?;
                            Ok(idx.into())
                        }
                        fn to_tokens(&self, tokens) {
                            let kw_token = kw::$kw(Span::call_site());
                            kw_token.to_tokens(tokens);
                            let idx = self.get();
                            quote!({#idx}).to_tokens(tokens)
                        }
                    }
                }
            )*
        }
    }

    idx_syn_quote! {
        d_idx for Data,
        v_idx for Variant,
        e_idx for Expr,
        c_idx for Coll,
        tp_idx for TParam,
        ztp_idx for ZipTParam,
        ttp_idx for TopTParam,
        p_idx for TypPath,
    }
}

pub type StaticPref = &'static [&'static str];
pub type StaticTypId = &'static str;

#[derive(Debug, Clone, Copy)]
pub struct StaticTypPath {
    pub pref: StaticPref,
    pub id: StaticTypId,
}
impl StaticTypPath {
    pub const fn new(pref: StaticPref, id: StaticTypId) -> Self {
        Self { pref, id }
    }
    pub fn to_typ(&self, span: rust::Span, args: Option<rust::GenericArgs>) -> rust::Typ {
        let pref = self.pref.iter().map(|id| rust::Id::new(id, span));
        let id = rust::Id::new(self.id, span);
        rust::typ::simple_path(pref, id, args)
    }
}
implement! {
    impl StaticTypPath {
        From<(StaticPref, StaticTypId)> {
            |pair| Self { pref: pair.0, id: pair.1 }
        }
    }
}

pub mod builtin {
    pub mod path {
        use crate::prelude::{idx, StaticTypPath};

        macro_rules! stp_array {
            [ $( $($path_segment:ident ::)* > $target:ident ),* $(,)? ] => {{
                $({
                    #[allow(unused_imports)]
                    use $($path_segment ::)* $target;
                })*
                &[
                    $(StaticTypPath {
                        pref: &[$(stringify!($path_segment)),*],
                        id: stringify!($target),
                    }),*
                ]
            }}
        }

        macro_rules! getters {
            { $(
                $getter:ident(
                    $id:ident at $idx:expr
                    $(, last_segment == $last_segment:ident)?
                )
            ),* $(,)? } => {$(
                pub const fn $getter() -> idx::TypPath {
                    let idx = $idx;
                    // let id = stringify!($id);
                    // match PATHS[idx].id {
                    //     stringify!($id) => (),
                    //     _ => panic!(
                    //         "[internal static path] expected `{}`, got `{}`",
                    //         id,
                    //         PATHS[idx].id,
                    //     ),
                    // }
                    // $(
                    //     let last_segment = stringify!($last_segment);
                    //     if PATHS[idx].pref.last() != Some(&last_segment) {
                    //         panic!(
                    //             "[internal static path] expected `{}`, got `{}`",
                    //             last_segment,
                    //             PATHS[idx].pref.last().unwrap_or(&"<nothing>")
                    //         )
                    //     }
                    // )?

                    idx::TypPath::new(idx)
                }
            )*};
        }

        const PATHS: &[StaticTypPath] = stp_array![
            // 0
            std::boxed::>Box,
            std::option::>Option,

            // 2
            std::vec::>Vec,
            std::slice::>Iter,
            std::vec::>IntoIter,

            // 5
            std::collections::>HashSet,
            std::collections::hash_set::>Iter,
            std::collections::hash_set::>IntoIter,

            // 8
            std::collections::>BTreeSet,
            std::collections::btree_set::>Iter,
            std::collections::btree_set::>IntoIter,
        ];

        getters! {
            box_path(Box at 0),

            vec_path(Vec at 2),
            vec_ref_iter_path(Iter at 3, last_segment == slice),
            vec_own_iter_path(IntoIter at 4, last_segment == vec),

            hash_set_path(HashSet at 5),
            hash_set_ref_iter_path(Iter at 6, last_segment == hash_set),
            hash_set_own_iter_path(IntoIter at 7, last_segment == hash_set),

            btree_set_path(HashSet at 8),
            btree_set_ref_iter_path(Iter at 9, last_segment == btree_set),
            btree_set_own_iter_path(IntoIter at 10, last_segment == btree_set),
        }

        impl idx::TypPath {
            /// Static type path associated with an index.
            pub fn get_path(&self) -> &'static StaticTypPath {
                &PATHS[self.get()]
            }
        }
    }
}
