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
