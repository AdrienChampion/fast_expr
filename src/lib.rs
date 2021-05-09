//! `fast_expr` generates (semi-)zippers over your expression structures automatically.
//!
//! If you are looking for simple examples, go to the [`examples`][examples] module.
//!
//! [examples]: ./examples/index.html

/// Convenience macro to produce [`ZipDo`][zipdo] values.
///
/// [zipdo]: ./enum.ZipDo.html (ZipDo enum)
#[macro_export]
macro_rules! zip_do {
    (down: $val:expr) => {
        $crate::ZipDo::GoDown($val)
    };
    (up: $val:expr) => {
        $crate::ZipDo::GoUp($val)
    };
    (subst: $val:expr) => {
        $crate::ZipDo::Subst($val)
    };
}

#[cfg(any(test, feature = "examples"))]
pub mod examples;

/// Compile the documentation with `--features examples` to see the examples.
#[cfg(not(any(test, feature = "examples")))]
pub mod examples {}

pub use fast_expr_proc::fast_expr;
pub use fast_expr_proc_impl::fast_expr_impl;

pub use fast_expr_gen::{down, early, proceed, subst, up, CollDer, ZipDo, ZipUp};

/// Internal types: not dangerous, but not meant for users.
pub mod internal {
    pub use fast_expr_gen::internal::{Empty, Sink};
}

/// Recursive wrappers for a single expression.
pub mod wrap {
    pub use std::boxed::Box;
}

/// Recursive collection wrappers.
pub mod coll {
    pub use std::collections::{BTreeMap, HashMap};
    pub use std::vec::Vec;
}
