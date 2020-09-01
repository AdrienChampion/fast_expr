//! Fast_expr demos.

pub extern crate fast_expr;

/// Common imports.
pub mod prelude {
    pub use std::collections::BTreeMap;
}

/// Imports the prelude.
macro_rules! prelude {
    () => {
        use crate::prelude::*;
    };
}

pub mod explicit;
