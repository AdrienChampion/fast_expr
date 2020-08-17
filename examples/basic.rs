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
    spec trait Defaults<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
    {}

    /// Top-level expression structure.
    enum Expr<Defaults> {
        /// Boolean variant.
        B(BExpr<_>),

        /// Integer variant.
        I(IExpr<_>),

        /// Bool list variant.
        ListB(List<BSpec, ISpec, LSpec, BExpr>),

        /// Integer list variant.
        ListI(List<BSpec, ISpec, LSpec, IExpr>),
    }

    /// Boolean expressions.
    enum BExpr<Defaults> {
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
            tail: coll::Vec<Self>,
        },
        /// A relation between two integer expressions.
        IRel {
            /// Relation.
            irel: BSpec::IRel,
            /// First operand.
            lft: IExpr<_>,
            /// Second operand.
            rgt: IExpr<_>,
        },
        /// A unary predicate over lists of booleans.
        BListUnApp {
            /// Operator.
            op: BSpec::ListUnOp,
            /// Operand.
            list: List<LSpec, Self>,
        }
    }

    /// Integer expressions.
    enum IExpr<Defaults> {
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
            tail: coll::Vec<Self>,
        },
        /// An if-then-else.
        Ite {
            /// Condition (boolean).
            cnd: BExpr<_>,
            /// Then-branch.
            thn: Self,
            /// Else-branch.
            els: Self,
        },
        /// Counts the number of true sub-expressions.
        CountTrue(
            coll::Vec<BExpr<_>>,
        ),
        /// A unary predicate over lists of booleans.
        IListUnApp {
            /// Operator.
            op: ISpec::ListUnOp,
            /// Operand.
            list: List<LSpec, Self>,
        }
    }

    spec trait LDefaults<LSpec, Expr>
    where
        LSpec: ListSpec,
    {}

    /// List expressions.
    enum List<LDefaults> {
        /// A list *"constant"*.
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

fn main() {}

pub mod test {
    use super::{BoolSpec, IntSpec, ListSpec};

    /// Top-level expression structure.
    pub enum Expr<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
    {
        /// Boolean variant.
        B(BExpr<BSpec, ISpec, LSpec>),

        /// Integer variant.
        I(IExpr<BSpec, ISpec, LSpec>),

        /// Bool list variant.
        ListB(List<LSpec, BExpr<BSpec, ISpec, LSpec>>),

        /// Integer list variant.
        ListI(List<LSpec, IExpr<BSpec, ISpec, LSpec>>),
    }

    /// Boolean expressions.
    pub enum BExpr<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
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
            lft: IExpr<BSpec, ISpec, LSpec>,
            /// Second operand.
            rgt: IExpr<BSpec, ISpec, LSpec>,
        },
        /// A unary predicate over lists of booleans.
        BListUnApp {
            /// Operator.
            op: BSpec::ListUnOp,
            /// Operand.
            list: List<LSpec, Self>,
        },
    }

    /// Integer expressions.
    pub enum IExpr<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
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
            cnd: Box<BExpr<BSpec, ISpec, LSpec>>,
            /// Then-branch.
            thn: Box<Self>,
            /// Else-branch.
            els: Box<Self>,
        },
        /// Counts the number of true sub-expressions.
        CountTrue(Vec<BExpr<BSpec, ISpec, LSpec>>),
        /// A unary predicate over lists of booleans.
        IListUnApp {
            /// Operator.
            op: ISpec::ListUnOp,
            /// Operand.
            list: List<LSpec, Self>,
        },
    }

    /// List expressions.
    pub enum List<LSpec: ListSpec, Expr> {
        /// A list *"constant"*.
        Cst(Vec<Expr>),
        /// A list variable.
        Var(LSpec::LVar),
        /// A list binary application.
        BinApp {
            /// Operator.
            op: LSpec::LBinOp,
            /// First operand.
            fst: Box<Self>,
            /// Second operand.
            snd: Box<Self>,
        },
        /// A list unary application.
        UnApp {
            /// Operator.
            op: LSpec::LUnOp,
            /// Operand.
            list: Box<Self>,
        },
    }

    pub trait Zipper<Expr> {
        type Res;
        // fn zip(&mut self, e: Expr) -> Self::Res;
    }

    pub enum BExprFrame<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
    {
        Whatever(BSpec, ISpec, LSpec),
    }

    pub struct BExprZip<Zip, BSpec, ISpec, LSpec>
    where
        Zip: Zipper<BExpr<BSpec, ISpec, LSpec>>,
        Zip: Zipper<IExpr<BSpec, ISpec, LSpec>>,
        Zip: Zipper<List<LSpec, BExpr<BSpec, ISpec, LSpec>>>,
        Zip: Zipper<List<LSpec, IExpr<BSpec, ISpec, LSpec>>>,
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
    {
        #[allow(dead_code)]
        zipper: Zip,
        #[allow(dead_code)]
        b_stack: Vec<BExprFrame<BSpec, ISpec, LSpec>>,
        #[allow(dead_code)]
        i_stack: Vec<IExprFrame<BSpec, ISpec, LSpec>>,
        #[allow(dead_code)]
        l1_stack: Vec<ListFrame<LSpec, BExpr<BSpec, ISpec, LSpec>>>,
        #[allow(dead_code)]
        l2_stack: Vec<ListFrame<LSpec, IExpr<BSpec, ISpec, LSpec>>>,
    }

    pub trait BExprZipper<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
        Self: Zipper<IExpr<BSpec, ISpec, LSpec>>,
        Self: Zipper<List<LSpec, Self>>,
        Self: Sized,
    {
        type BExprRes;

        fn bexpr_zip(&mut self, e: BExpr<BSpec, ISpec, LSpec>) -> Self::BExprRes;
    }

    impl<T, BSpec, ISpec, LSpec> Zipper<BExpr<BSpec, ISpec, LSpec>> for T
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
        T: BExprZipper<BSpec, ISpec, LSpec>,
    {
        type Res = T::BExprRes;
        // fn zip(&mut self, e: BExpr<BSpec, ISpec, LSpec>) -> Self::Res {
        //     self.bexpr_zip(e)
        // }
    }

    pub enum IExprFrame<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
    {
        Whatever(BSpec, ISpec, LSpec),
    }

    pub trait IExprZipper<BSpec, ISpec, LSpec>
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
        Self: Zipper<BExpr<BSpec, ISpec, LSpec>>,
        Self: Zipper<List<LSpec, Self>>,
        Self: Sized,
    {
        type IExprRes;
        fn iexpr_zip(&mut self, e: IExpr<BSpec, ISpec, LSpec>) -> Self::IExprRes;
    }

    impl<T, BSpec, ISpec, LSpec> Zipper<IExpr<BSpec, ISpec, LSpec>> for T
    where
        BSpec: BoolSpec,
        ISpec: IntSpec,
        LSpec: ListSpec,
        T: IExprZipper<BSpec, ISpec, LSpec>,
    {
        type Res = T::IExprRes;
        // fn zip(&mut self, e: IExpr<BSpec, ISpec, LSpec>) -> Self::Res {
        //     self.iexpr_zip(e)
        // }
    }

    pub enum ListFrame<LSpec, Expr>
    where
        LSpec: ListSpec,
    {
        Whatever(LSpec, Expr),
    }

    pub trait ListZipper<LSpec, Expr>
    where
        LSpec: ListSpec,
        Self: Zipper<Expr>,
        Self: Sized,
    {
        type ListRes;
        fn list_zip(&mut self, e: List<LSpec, Expr>) -> Self::ListRes;
    }

    impl<T, LSpec, Expr> Zipper<List<LSpec, Expr>> for T
    where
        LSpec: ListSpec,
        T: ListZipper<LSpec, Expr>,
    {
        type Res = T::ListRes;
        // fn zip(&mut self, e: List<LSpec, Expr>) -> Self::Res {
        //     self.list_zip(e)
        // }
    }
}
