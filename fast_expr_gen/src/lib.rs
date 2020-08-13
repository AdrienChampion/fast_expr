//! Code generation crate for fast-expr.

pub extern crate quote;
pub extern crate syn;

#[macro_use]
pub mod macros;

pub mod cxt;
pub mod err;
pub mod expr;
pub mod front;
pub mod prelude;
pub mod rust;

prelude! {}

pub fn generate_context(expr: front::Expr) -> Result<cxt::Cxt> {
    cxt::from_front(expr)
}
