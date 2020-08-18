//! Re-exports and common type/helpers used throughout fast expr.

pub use std::{
    collections::{BTreeMap as Map, BTreeSet as Set},
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
pub use quote::{quote, ToTokens, TokenStreamExt};
pub use smallvec::smallvec;
pub use syn::Error;

pub use crate::{
    check, cxt,
    err::{self, Result as Res},
    expr, front, gen, log, logln, rust,
};

/// Used to indicate whether something was generated.
pub type WasGenerated = bool;

/// Used to distinguish between an owned and referenced value or type.
pub type IsOwn = bool;

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

// pub trait WriteExt<'txt>: Sized {
//     /// Writes itself into a formatter.
//     fn write_fmt(&self, w: &mut std::fmt::Formatter, cxt: Option<&cxt::Cxt<'txt>>) -> URes;

//     /// Writes itself into a writer.
//     fn write(&self, w: &mut impl Write, cxt: &cxt::Cxt<'txt>) -> URes {
//         write!(w, "{}", self.display(cxt))?;
//         Ok(())
//     }

//     /// Turns itself into something that can be displayed.
//     fn display<'me, 'cxt>(
//         &'me self,
//         cxt: &'cxt cxt::Cxt<'txt>,
//     ) -> Pair<&'me Self, Option<&'cxt cxt::Cxt<'txt>>> {
//         (self, Some(cxt)).into()
//     }
//     /// Turns itself into something that can be displayed in debug mode.
//     fn debug<'me, 'cxt>(&'me self) -> Pair<&'me Self, Option<&'cxt cxt::Cxt<'txt>>> {
//         (self, None).into()
//     }
//     fn display_or_debug<'me, 'cxt>(
//         &'me self,
//         cxt: Option<&'cxt cxt::Cxt<'txt>>,
//     ) -> Pair<&'me Self, Option<&'cxt cxt::Cxt<'txt>>> {
//         (self, cxt).into()
//     }
// }
// implement! {
//     impl('txt, T) Pair<&'_ T, Option<&'_ cxt::Cxt<'txt>>> {
//         Display where (
//             T: WriteExt<'txt>
//         ) {
//             |self, fmt| self.lft.write_fmt(fmt, self.rgt).map_err(|_| std::fmt::Error)
//         }
//     }
//     impl('txt, T) Pair<&'_ T, &'_ cxt::Cxt<'txt>> {
//         Display where (
//             T: WriteExt<'txt>
//         ) {
//             |self, fmt| self.lft.write_fmt(fmt, Some(self.rgt)).map_err(|_| std::fmt::Error)
//         }
//     }
// }

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
