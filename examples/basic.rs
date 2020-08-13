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
    /// List relations.
    type ListRel;
    /// Bool list unary operators.
    type ListUnOp;
}

/// Specification for integer expressions.
pub trait IntSpec {
    /// Type of integer variables.
    type IVar;
    /// Type of integer constants.
    type ICst;
    /// Type of integer operators.
    type IOp;
    /// Bool list unary operators.
    type ListUnOp;
}

/// Specification for list expressions.
pub trait ListSpec {
    /// Type of list variables.
    type LVar;
    /// Type of list binary operators.
    type LBinOp;
    /// Type of list unary operators.
    type LUnOp;
}

fast_expr::expr! {
    /// Top-level expression structure.
    enum Expr<BSpec: BoolSpec, ISpec: IntSpec, LSpec: ListSpec> {
        /// Boolean variant.
        B(BExpr),

        /// Integer variant.
        I(IExpr),

        /// Bool list variant.
        ListB(List<BExpr>),

        /// Integer list variant.
        ListI(List<IExpr>),
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
            /// A unary predicate over lists of booleans.
            BListUnApp {
                /// Operator.
                op: BSpec::ListUnOp,
                /// Operand.
                list: List<Self>,
            }
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
            /// A unary predicate over lists of booleans.
            IListUnApp {
                /// Operator.
                op: ISpec::ListUnOp,
                /// Operand.
                list: List<Self>,
            }
        }

        /// List expressions.
        enum List<BSpec: BoolSpec, ISpec: IntSpec, LSpec: ListSpec, Expr> {
            /// A list constant.
            Cst(Vec<Expr>),
            /// A list variable.
            Var(LSpec::ListVar),
            /// A list binary application.
            BinApp {
                /// Operator.
                op: LSpec::BinOp,
                /// First operand.
                fst: Self,
                /// Second operand.
                snd: Self,
            },
            /// A list unary application.
            UnApp {
                /// Operator.
                op: LSpec::UnOp,
                /// Operand.
                list: Self,
            },
        }
    }
}

fn main() {}
