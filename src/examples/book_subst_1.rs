//! Substitution example, this example is used in the book.

prelude! {}

mod fast_expr {
    pub use crate::*;
}

use crate::fast_expr;
use fast_expr::ZipDo;

/// Expression specification.
pub trait Spec: Clone {
    /// Values (for constants).
    type Val: Clone;
    /// Identifiers (for variables).
    type Id: Clone;
    /// Operators (for applications)
    type Op: Clone;
}

fast_expr! {
    #![fast_expr(
        all_pub = true,
        top = Expr,
        ref_impl_macro = subst_example_own_impl,
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

impl<S: Spec> Expr<S> {
    pub fn cst(val: S::Val) -> Self {
        Self::Cst(val)
    }
    pub fn var(id: S::Id) -> Self {
        Self::Var(id)
    }
    pub fn app(op: S::Op, head: Self, tail: Vec<Self>) -> Self {
        Self::App {
            op,
            head: Box::new(head),
            tail,
        }
    }
}

// ANCHOR: subst_on_expr
impl<S> Expr<S>
where
    S: Spec,
    S::Id: std::cmp::Ord,
{
    pub fn subst(self, map: &BTreeMap<S::Id, Expr<S>>) -> Self {
        use zip_own::ExprZipSpec;
        Subst::from(map).zip_expr(self)
    }
}
// ANCHOR_END: subst_on_expr

// ANCHOR: zip_def
/// Implements variable substitution over `Expr`s.
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
// ANCHOR_END: zip_def

// ANCHOR: zip_impl
impl<'map, S> zip_own::ExprZipSpec<S> for Subst<'map, S>
where
    S: Spec,
    S::Id: std::cmp::Ord,
{
    /// We want to generate expressions.
    type ExprRes = Expr<S>;

    /// Going up a variable.
    ///
    /// If something's in the map for this variable, yield that. Otherwise just yield the same
    /// expression (a variable).
    fn go_up_expr_var(&mut self, var: S::Id) -> fast_expr::ZipUp<Expr<S>, Self::ExprRes> {
        ZipDo::GoUp(
            self.map
                .get(&var)
                .cloned()
                .unwrap_or_else(|| Expr::Var(var)),
        )
    }
    /// Going up a constant, nothing to do.
    fn go_up_expr_cst(&mut self, val: S::Val) -> fast_expr::ZipUp<Expr<S>, Self::ExprRes> {
        ZipDo::GoUp(Expr::Cst(val))
    }

    /// Folding over a `tail` should yield a new tail, *i.e.* a `Vec<Expr<S>>`.
    type ExprAppTailAcc = Vec<Expr<S>>;

    /// Going up an `Expr::App`.
    ///
    /// We get the original `op`, the result for `head`, and the result for `tail` (as the final
    /// value of the accumulator). Yields a new `Expr::App` with the input `op`, `head` and `tail`.
    fn go_up_expr_app(
        &mut self,
        op: S::Op,
        // `Self::ExprRes = Expr<S>`
        head: Self::ExprRes,
        // `Self::ExprAppTailAcc = Vec<Expr<S>>`
        tail: Self::ExprAppTailAcc,
    ) -> fast_expr::ZipUp<Expr<S>, Self::ExprRes> {
        ZipDo::GoUp(Expr::App {
            op,
            head: Box::new(head),
            tail,
        })
    }

    /// Initializes the `tail` accumulator.
    ///
    /// Produces an empty vector with capacity `tail.len()`, that will eventually store the results
    /// of zipping over each element of `tail`.
    fn coll_init_expr_app_tail(
        &mut self,
        // Original `op`.
        _op: &S::Op,
        // Result for `head`.
        _head: &Self::ExprRes,
        // `tail` we're about to go down into.
        tail: &Vec<Expr<S>>,
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes> {
        ZipDo::GoDown(Vec::with_capacity(tail.len()))
    }

    /// Folds over a `tail` accumulator.
    ///
    /// Pushes `next_res` on `acc` and returns it.
    fn coll_fold_expr_app_tail(
        &mut self,
        // Original `op`.
        _op: &S::Op,
        // Result for `head`.
        _head: &Self::ExprRes,
        // Current accumulator, and result for the next element in the tail.
        (mut acc, next_res): (Self::ExprAppTailAcc, Self::ExprRes),
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes> {
        acc.push(next_res);
        ZipDo::GoDown(acc)
    }
}
// ANCHOR_END: zip_impl

// ANCHOR: example_spec
/// Integer operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IOp {
    /// Addition.
    Add,
    /// Subtraction.
    Sub,
}

/// Integer expression specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ISpec;
impl Spec for ISpec {
    type Op = IOp;
    type Id = &'static str;
    type Val = isize;
}

/// Integer expressions.
pub type IExpr = Expr<ISpec>;
// ANCHOR_END: example_spec

// ANCHOR: example
/// Helper macro to build expressions easily.
macro_rules! iexpr {
    // `Add` application.
    ( ( $expr_head:tt $(+ $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Add,
            iexpr!($expr_head),
            vec![
                $(iexpr!($expr_tail), )*
            ],
        )
    };
    // `Sub` application.
    ( ( $expr_head:tt $(- $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Sub,
            iexpr!($expr_head),
            vec![
                $( iexpr!($expr_tail), )*
            ],
        )
    };
    // `Id` as a token, must be given between `[...]`.
    ([$id:tt]) => {
        IExpr::var(stringify!($id))
    };
    // `Cst` as an expression.
    ($val:expr) => {
        IExpr::cst($val)
    };
}

/// Tests `Expr::subst`.
pub fn test_subst_1() {
    // Substitution map.
    let map: BTreeMap<
        &'static str, // ISpec::Id
        IExpr,        // Expr<ISpec>
    > = {
        let mut map = BTreeMap::new();
        let _prev = map.insert("v_1", iexpr!(1));
        assert_eq!(_prev, None);
        let _prev = map.insert("v_2", iexpr!([v_3]));
        assert_eq!(_prev, None);
        map
    };
    println!("subst map {{");
    for (key, value) in map.iter() {
        println!("    {} => {:?},", key, value);
    }
    println!("}}");

    // Test expression we'll `subst(map)` soon.
    let iexpr = iexpr! {
        (3 + [v_2] + (7 - [v_1]))
    };
    println!("iexpr:     {:?}", iexpr);

    // Expected result.
    let expected = iexpr! {
        (3 + [v_3] + (7 - 1))
    };
    println!("expected:  {:?}", expected);

    // Apply the substitution.
    let iexpr = iexpr.subst(&map);
    println!("subst res: {:?}", iexpr);

    // Check the result.
    assert!(iexpr == expected);

    println!("no problem")
}
// ANCHOR_END: example

#[test]
fn subst_1() {
    test_subst_1()
}
