//! `fast_expr` generates (semi-)zippers over your expression structures automatically.
//!
//! If you are looking for simple examples, go to the [`examples`][examples] module.
//!
//! [examples]: ./examples/index.html

#[cfg(any(test, feature = "examples"))]
pub mod examples;

/// Compile the documentation with `--features examples` to see the examples.
#[cfg(not(any(test, feature = "examples")))]
pub mod examples {}

pub use fast_expr_proc::fast_expr;

pub use fast_expr_gen::{CollDer, ZipDo};

/// Internal types: not dangerous, but not meant for users.
pub mod internal {
    pub use fast_expr_gen::internal::{Empty, Sink};
}
