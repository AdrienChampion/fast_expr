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
pub use proc_macro2::TokenStream;
pub use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
pub use smallvec::smallvec;
pub use syn::Error;

pub use crate::{
    check, conf,
    cxt::{self, Cxt},
    doc,
    err::{self, Result as Res},
    expr, front, gen, log, logln,
    rust::{self, Ident, Type},
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

/// Safe indices.
pub mod idx {
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
}

/// A static slice to static string slices.
pub type StaticPref = &'static [&'static str];
/// A static string slice.
pub type StaticTypId = &'static str;

/// A static path.
#[derive(Debug, Clone, Copy)]
pub struct StaticTypPath {
    /// Path prefix.
    pub pref: StaticPref,
    /// Path final id.
    pub id: StaticTypId,
}
impl StaticTypPath {
    /// Constructor.
    pub const fn new(pref: StaticPref, id: StaticTypId) -> Self {
        Self { pref, id }
    }

    /// Type version of itself.
    pub fn to_typ(&self, span: rust::Span, args: Option<rust::GenericArgs>) -> Type {
        let pref = self.pref.iter().map(|id| Ident::new(id, span));
        let id = Ident::new(self.id, span);
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

// /// A function parameter for zip functions.
// ///
// /// Can come either from constant data, recursive one-expr data, or recursive many-expr data.
// #[derive(Debug, Clone)]
// pub enum FnParamKind {
//     Cst(Type),
//     One(idx::Expr, Type),
//     Res(idx::Expr),
//     ManyAcc(idx::Coll),
//     Many(idx::Coll, idx::Expr),
// }
// impl FnParamKind {
//     /// Turns itself into tokens.
//     pub fn to_tokens(&self, cxt: cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
//         match self {
//             Self::Cst(typ) => {
//                 if is_own {
//                     typ.to_token_stream()
//                 } else {
//                     syn::parse_quote!( &#typ )
//                 }
//             }
//             Self::One(_e_idx, typ) => {
//                 if is_own {
//                     typ.to_token_stream()
//                 } else {
//                     syn::parse_quote!( &#typ )
//                 }
//             }
//             Self::Res(e_idx) =>
//             _ => todo!(),
//         }
//     }
// }
