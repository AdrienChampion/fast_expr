//! Fast expr.

pub mod examples;

pub use fast_expr_proc::expr;

/// An empty enumeration.
pub enum Empty {}

/// Stores an accumulator and an iterator, used when zipping over a collection.
pub struct CollDer<Acc, Iter> {
    pub acc: Acc,
    pub iter: Iter,
}
