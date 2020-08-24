//! Fast expr.

pub mod examples;

pub use fast_expr_proc::expr;

/// An empty enumeration.
pub enum Empty {}
/// Some phantom data, this type has no values.
pub type Sink<T> = (std::marker::PhantomData<T>, Empty);

/// Stores an accumulator and an iterator, used when zipping over a collection.
pub struct CollDer<Acc, Iter> {
    pub acc: Acc,
    pub iter: Iter,
}

/// Can zip over some expression type and produce a result.
pub trait Zipper<Expr> {
    /// Type of the result of zipping over an expression.
    type Res;
    /// Zips over an expression.
    fn zip(&mut self, expr: Expr) -> Self::Res;
}

pub enum ZipDo<Down, Expr, Res> {
    GoDown(Down),
    GoUp(Res),
    Subst(Expr),
    Early(Res),
}

pub trait Stepper<Expr> {
    type Frame;
    type StepRes;
    fn zip_step(&mut self, expr: Expr) -> ZipDo<(Expr, Self::Frame), Expr, Self::StepRes>;
}
