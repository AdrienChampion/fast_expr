/// Specification for boolean expressions.
pub trait BoolSpec {
    /// Type of boolean variables.
    type BVar;
    /// Type of boolean constants.
    type BCst;
    /// Type of boolean operators.
    type BOp;
    /// Type of integer relations.
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

fast_expr::expr! {
    /// Top-level expression structure.
    enum Expr<BSpec: BoolSpec, ISpec: IntSpec> {
        /// Boolean variant.
        B(BExpr<_>),

        /// Integer variant.
        I(IExpr<_>),
    }

    subs {
        /// Boolean expressions.
        enum BExpr {
            /// A boolean variable.
            Var(BSpec::BVar),
            /// A boolean constant.
            Cst(BSpec::BCst),
            /// Boolean operator application to one or more operands.
            App {
                /// Operator.
                op: BSpec::BOp,
                /// First operand.
                head: Self,
                /// Tail of operands.
                tail: Vec<Self>,
            },
            /// A relation between two integer expressions.
            IRel {
                /// Relation.
                irel: BSpec::IRel,
                /// First operand.
                lft: IExpr,
                /// Second operand.
                rgt: IExpr,
            },
        }

        /// Integer expressions.
        enum IExpr {
            /// An integer constant.
            Cst(ISpec::ICst),
            /// An integer variable.
            Var(ISpec::IVar),
            /// Integer operator application to one or more operands.
            App {
                /// Operator.
                op: ISpec::IOp,
                /// First operand.
                head: Self,
                /// Second operand.
                tail: Vec<Self>,
            },
            /// An if-then-else.
            Ite {
                /// Condition (boolean).
                cnd: BExpr,
                /// Then-branch.
                thn: Self,
                /// Else-branch.
                els: Self,
            },
            /// Counts the number of true sub-expressions.
            CountTrue(
                Vec<BExpr>,
            ),
        }
    }
}

fn main() {}
