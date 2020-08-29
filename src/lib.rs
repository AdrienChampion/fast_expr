//! Fast expr.

#[cfg(any(test, feature = "examples"))]
pub mod examples;

pub use fast_expr_proc::expr as fast_expr;

/// An empty enumeration.
pub enum Empty {}
/// Some phantom data, this type has no values.
pub type Sink<T> = (std::marker::PhantomData<T>, Empty);

/// Stores an accumulator and an iterator, used when zipping over a collection.
pub struct CollDer<Acc, Iter> {
    pub acc: Acc,
    pub iter: Iter,
}
impl<Acc, Iter> CollDer<Acc, Iter> {
    pub fn new(acc: Acc, iter: Iter) -> Self {
        Self { acc, iter }
    }
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
impl<Down, Expr, Res> ZipDo<Down, Expr, Res> {
    pub fn map_down<NewDown>(
        self,
        action: impl FnOnce(Down) -> NewDown,
    ) -> ZipDo<NewDown, Expr, Res> {
        match self {
            Self::GoDown(down) => ZipDo::GoDown(action(down)),
            Self::GoUp(up) => ZipDo::GoUp(up),
            Self::Subst(subst) => ZipDo::Subst(subst),
            Self::Early(early) => ZipDo::Early(early),
        }
    }

    pub fn down_and_then<NewDown>(
        self,
        action: impl FnOnce(Down) -> ZipDo<NewDown, Expr, Res>,
    ) -> ZipDo<NewDown, Expr, Res> {
        match self {
            Self::GoDown(down) => action(down),
            Self::GoUp(up) => ZipDo::GoUp(up),
            Self::Subst(subst) => ZipDo::Subst(subst),
            Self::Early(early) => ZipDo::Early(early),
        }
    }
}

pub trait Stepper<Expr> {
    type Frame;
    type StepRes;
    fn handle_expr(&mut self, expr: Expr) -> ZipDo<(Expr, Self::Frame), Expr, Self::StepRes>;
    fn handle_frame(
        &mut self,
        frame: Self::Frame,
    ) -> ZipDo<(Expr, Self::Frame), Expr, Self::StepRes>;
}
