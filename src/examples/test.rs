//! Main fast-expr test example.

pub mod fast_expr {
    pub use crate::*;
}

/// Specification for boolean expressions.
pub trait BoolSpec {
    /// Boolean variables.
    type BVar;
    /// Boolean constants.
    type BCst;
    /// Boolean operators.
    type BOp;
    /// Integer relations.
    type IRel;
}

/// Specification for integer expressions.
pub trait IntSpec {
    /// Type of integer variables.
    type IVar;
    /// Type of integer constants.
    type ICst;
    /// Type of integer operators.
    type IOp;
}

crate::expr! {
    spec trait Defaults<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {}

    /// Top-level expression structure.
    pub enum Expr<Defaults> {
        /// Boolean variant.
        B(BExpr<_>),

        /// Integer variant.
        I(IExpr<_>),
    }

    /// Boolean expressions.
    pub enum BExpr<Defaults> {
        /// A boolean variable.
        Var(BSpec::BVar),
        /// A boolean constant.
        Cst(BSpec::BCst),
        /// Boolean operator application to one or more operands.
        App {
            /// Operator.
            op: BSpec::BOp,
            /// First operand.
            head: wrap::Box<Self>,
            /// Tail of operands.
            tail: coll::Vec<Self>,
        },
        /// A relation between two integer expressions.
        IRel {
            /// Relation.
            irel: BSpec::IRel,
            /// First operand.
            lft: wrap::Box<IExpr<_>>,
            /// Second operand.
            rgt: wrap::Box<IExpr<_>>,
        },
    }

    /// Integer expressions.
    pub enum IExpr<Defaults> {
        /// An integer constant.
        Cst(ISpec::ICst),
        /// An integer variable.
        Var(ISpec::IVar),
        /// Integer operator application to one or more operands.
        App {
            /// Operator.
            op: ISpec::IOp,
            /// First operand.
            head: wrap::Box<Self>,
            /// Second operand.
            tail: coll::Vec<Self>,
        },
        /// An if-then-else.
        Ite {
            /// Condition (boolean).
            cnd: BExpr<_>,
            /// Then-branch.
            thn: wrap::Box<Self>,
            /// Else-branch.
            els: wrap::Box<Self>,
        },
        /// Counts the number of true sub-expressions.
        CountTrue(
            coll::Vec<BExpr<_>>,
        ),
        /// Counts the number of true sub-expressions.
        CountPositive(
            coll::Vec<IExpr<_>>,
        ),
    }
}

pub mod inline {
    use super::{BoolSpec, IntSpec};

    /// Top-level expression structure.
    pub enum Expr<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        /// Boolean variant.
        B(BExpr<BSpec, ISpec>),

        /// Integer variant.
        I(IExpr<BSpec, ISpec>),
    }

    /// Boolean expressions.
    pub enum BExpr<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        /// A boolean variable.
        Var(BSpec::BVar),
        /// A boolean constant.
        Cst(BSpec::BCst),
        /// Boolean operator application to one or more operands.
        App {
            /// Operator.
            op: BSpec::BOp,
            /// First operand.
            head: Box<Self>,
            /// Tail of operands.
            tail: Vec<Self>,
        },
        /// A relation between two integer expressions.
        IRel {
            /// Relation.
            irel: BSpec::IRel,
            /// First operand.
            lft: IExpr<BSpec, ISpec>,
            /// Second operand.
            rgt: IExpr<BSpec, ISpec>,
        },
    }

    /// Integer expressions.
    pub enum IExpr<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        /// An integer constant.
        Cst(ISpec::ICst),
        /// An integer variable.
        Var(ISpec::IVar),
        /// Integer operator application to one or more operands.
        App {
            /// Operator.
            op: ISpec::IOp,
            /// First operand.
            head: Box<Self>,
            /// Second operand.
            tail: Vec<Self>,
        },
        /// An if-then-else.
        Ite {
            /// Condition (boolean).
            cnd: Box<BExpr<BSpec, ISpec>>,
            /// Then-branch.
            thn: Box<Self>,
            /// Else-branch.
            els: Box<Self>,
        },
        /// Counts the number of true sub-expressions.
        CountTrue(Vec<BExpr<BSpec, ISpec>>),
    }

    use crate::Zipper;

    pub enum BExprFrame<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        Whatever(BSpec, ISpec),
    }

    pub struct BExprZip<Zip, BSpec, ISpec>
    where
        Zip: Zipper<BExpr<BSpec, ISpec>>,
        Zip: Zipper<IExpr<BSpec, ISpec>>,
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        #[allow(dead_code)]
        zipper: Zip,
        #[allow(dead_code)]
        b_stack: Vec<BExprFrame<BSpec, ISpec>>,
        #[allow(dead_code)]
        i_stack: Vec<IExprFrame<BSpec, ISpec>>,
    }

    pub trait BExprZipper<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        Self: Zipper<IExpr<BSpec, ISpec>>,
        Self: Sized,
    {
        type BExprRes;
        fn bexpr_zip(&mut self, e: BExpr<BSpec, ISpec>) -> Self::BExprRes;
    }

    impl<T, BSpec, ISpec> Zipper<BExpr<BSpec, ISpec>> for T
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        T: BExprZipper<BSpec, ISpec>,
    {
        type Res = T::BExprRes;
        fn zip(&mut self, e: BExpr<BSpec, ISpec>) -> Self::Res {
            self.bexpr_zip(e)
        }
    }

    pub enum IExprFrame<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
    {
        Whatever(BSpec, ISpec),
    }

    pub trait IExprZipper<BSpec, ISpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        Self: Zipper<BExpr<BSpec, ISpec>>,
        Self: Sized,
    {
        type IExprRes;
        fn iexpr_zip(&mut self, e: IExpr<BSpec, ISpec>) -> Self::IExprRes;
    }

    impl<T, BSpec, ISpec> Zipper<IExpr<BSpec, ISpec>> for T
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        T: IExprZipper<BSpec, ISpec>,
    {
        type Res = T::IExprRes;
        fn zip(&mut self, e: IExpr<BSpec, ISpec>) -> Self::Res {
            self.iexpr_zip(e)
        }
    }
}
