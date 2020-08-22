//! Main fast-expr test example.

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
    }
}
