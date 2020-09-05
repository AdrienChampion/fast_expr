//! Substitution example, this example is used in the book.

prelude! {}

mod fast_expr {
    pub use crate::*;
}

use crate::fast_expr;

/// Expression specification.
pub trait Spec: Clone {
    /// Values (for constants).
    type Val: Clone + std::fmt::Debug;
    /// Identifiers (for variables).
    type Id: Clone + std::cmp::Ord + std::fmt::Debug;
    /// Operators (for applications)
    type Op: Clone + std::fmt::Debug;
}

fast_expr! {
    #![fast_expr(
        all_pub = true,
    )]

    /// Expression type.
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Expr<S: Spec> {
        /// A constant.
        Cst(S::Val),
        /// A variable.
        Var(S::Id),
        /// An application of some operator to one or more arguments.
        App {
            /// Operator.
            op: S::Op,
            /// First argument.
            head: wrap::Box<Self>,
            /// Argument tail.
            tail: coll::Vec<Self>,
        },
    }
}

impl<S> Expr<S>
where
    S: Spec,
{
    pub fn subst(self, map: &BTreeMap<S::Id, Expr<S>>) -> Self {
        zip_own::ExprZip::new(Subst::from(map)).zip_expr(self)
    }
}

/// Implements variable substitution over `Expr`s.
#[derive(Debug, Clone, Copy)]
pub struct Subst<'map, S>
where
    S: Spec,
{
    map: &'map BTreeMap<S::Id, Expr<S>>,
}

impl<'map, S> From<&'map BTreeMap<S::Id, Expr<S>>> for Subst<'map, S>
where
    S: Spec,
{
    fn from(map: &'map BTreeMap<S::Id, Expr<S>>) -> Self {
        Self { map }
    }
}

// #[track_caller]
// macro_rules! zipper_impl_ref {
//     (@($($expected:ident),*)($($got:ident),*)
//         Expr $(<$($t_params:ty),* $(,)?>)*
//     ) => {
//     };
//     (@($($expected:ident),*)($($got:ident),*)) => {
//         fast_expr::missing_cases! {
//             "expression handler(s)",
//             expected($($expected),*),
//             got($($got),*),
//         }
//     };
//     ($($stuff:tt)*) => {
//         zipper_impl_ref! {
//             @(Expr, BExp, IExpr)()
//             $($stuff)*
//         }
//     };
// }

// pub fn blah() {
//     zipper_impl_ref! {
//         Expr<'a, S>
//     }
// }

impl<'map, S> zip_own::ExprZipper<S> for Subst<'map, S>
where
    S: Spec,
{
    type ExprRes = Expr<S>;
    type ExprAppTailAcc = Vec<Expr<S>>;

    fn go_up_expr_var(&mut self, var: S::Id) -> Expr<S> {
        self.map
            .get(&var)
            .cloned()
            .unwrap_or_else(|| Expr::Var(var))
    }
    fn go_up_expr_cst(&mut self, val: S::Val) -> Expr<S> {
        Expr::Cst(val)
    }
    fn go_up_expr_app(&mut self, op: S::Op, head: Expr<S>, tail: Vec<Expr<S>>) -> Expr<S> {
        Expr::App {
            op,
            head: Box::new(head),
            tail,
        }
    }

    fn coll_init_expr_app_tail(
        &mut self,
        _op: &S::Op,
        _head: &Self::ExprRes,
        tail: &Vec<Expr<S>>,
    ) -> fast_expr::ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes> {
        fast_expr::ZipDo::GoDown(Vec::with_capacity(tail.len()))
    }
    fn coll_fold_expr_app_tail(
        &mut self,
        _op: &S::Op,
        _head: &Self::ExprRes,
        (mut acc, next_res): (Self::ExprAppTailAcc, Self::ExprRes),
    ) -> fast_expr::ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes> {
        acc.push(next_res);
        fast_expr::ZipDo::GoDown(acc)
    }
}
