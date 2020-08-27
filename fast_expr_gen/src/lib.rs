//! Code generation crate for fast-expr.

pub extern crate quote;
pub extern crate syn;

#[macro_use]
pub mod macros;

pub mod check;
pub mod cxt;
pub mod err;
pub mod expr;
pub mod front;
pub mod gen;
pub mod prelude;
pub mod rust;

prelude! {}

pub fn generate_context(top: front::Top) -> Res<impl prelude::ToTokens> {
    cxt::Top::new(top).map(|top| {
        top.dbg_log_to_file();
        top
    })
}
