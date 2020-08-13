//! Error-handling.

pub use syn::{Error, Result};

/// Parsing errors, stored in a global list.
pub mod parsing {
    use std::sync::RwLock;

    prelude! {}

    lazy_static::lazy_static! {
        /// Global list of parse errors.
        static ref ERRS: RwLock<Vec<Error>> = RwLock::new(vec![]);
    }

    /// Pushes a parse error.
    pub fn push(e: impl Into<Error>) {
        ERRS.write()
            .expect("global list of parse errors is corrupted")
            .push(e.into())
    }

    /// Drains all parse errors.
    pub fn drain() -> Option<Vec<Error>> {
        let mut errs = ERRS
            .write()
            .expect("global list of parse errors is corrupted");
        if errs.is_empty() {
            None
        } else {
            Some(errs.drain(0..).collect())
        }
    }
}
