#![deny(warnings)]

use fast_expr::fast_expr;

#[test]
fn renamed() {}
pub mod renamed {
    use super::*;

    pub mod fast_expr {}
    pub mod fastexpr {
        pub use fast_expr::*;
    }

    fast_expr! {
        #![fast_expr(
            name = fastexpr,
        )]

        spec trait ESpec<Var, Cst, Op> {}

        #[allow(dead_code)]
        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        #[allow(dead_code)]
        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}

/// **NB**: this only tests that `zip_ref` is available when `!own_gen` and `ref_gen`.
#[test]
fn no_own_1() {
    #[allow(unused_imports)]
    use no_own_1::zip_ref::*;
}
pub mod no_own_1 {
    use super::*;

    fast_expr! {
        #![fast_expr(
            own_gen = false,
            ref_gen = true,
        )]

        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}

/// **NB**: this only tests that `zip_own` is available when `!ref_gen` and `own_gen`.
#[test]
fn no_ref() {
    pub use no_ref::zip_own::*;
}
pub mod no_ref {
    use super::*;

    fast_expr! {
        #![fast_expr(
            own_gen = true,
            ref_gen = false,
        )]

        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}

/// **NB**: this only tests that `zip_own` and `zip_ref` are available when `ref_gen` and `own_gen`.
#[test]
fn ref_and_own() {
    #[allow(unused_imports)]
    use ref_and_own::{zip_own::*, zip_ref::*};
}
pub mod ref_and_own {
    use super::*;

    fast_expr! {
        #![fast_expr(
            own_gen = true,
            ref_gen = true,
        )]

        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}

// **NB**: this only checks that the top expression's Zip and Zipper are defined.
#[test]
fn top_1() {
    {
        #[allow(unused_imports)]
        use top_1::zip_own::{ExprZip, ExprZipSpec};
    }
    {
        #[allow(unused_imports)]
        use top_1::zip_ref::{ExprZip, ExprZipSpec};
    }
    // // Must not compile.
    // {
    //     #[allow(unused_imports)]
    //     use top_1::zip_own::{BExprZip, BExprZipSpec};
    // }
    // {
    //     #[allow(unused_imports)]
    //     use top_1::zip_ref::{BExprZip, BExprZipSpec};
    // }
}
pub mod top_1 {
    use super::*;

    fast_expr! {
        #![fast_expr(
            top = Expr,
        )]

        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}

// **NB**: this only checks that the top expression's Zip and Zipper are defined.
#[test]
fn top_2() {
    {
        #[allow(unused_imports)]
        use top_2::zip_own::{ExprZip, ExprZipSpec};
    }
    {
        #[allow(unused_imports)]
        use top_2::zip_ref::{ExprZip, ExprZipSpec};
    }
    // // Must not compile.
    // {
    //     #[allow(unused_imports)]
    //     use top_1::zip_own::{BExprZip, BExprZipSpec};
    // }
    // {
    //     #[allow(unused_imports)]
    //     use top_1::zip_ref::{BExprZip, BExprZipSpec};
    // }
}
pub mod top_2 {
    use super::*;

    fast_expr! {
        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        #[fast_expr(
            zip_gen = false,
        )]
        pub enum BExpr<ESpec> {
            Var(Var),
            Cst(Cst),
            App {
                op: Op,
                args: coll::Vec<Self>,
            }
        }
    }
}
