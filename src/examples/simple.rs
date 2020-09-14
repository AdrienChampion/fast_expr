//! Simple example: single expression type representing untyped expressions.

mod fast_expr {
    pub use crate::*;
}

use crate::fast_expr;

prelude! {}

fast_expr! {
    /// A monolithic expression type representing untyped expressions.
    pub enum Expr {
        /// A variable.
        Var(id::Id),
        /// A constant.
        Cst(cst::Cst),
        /// An application of an operator.
        App {
            op: op::Op,
            args: coll::Vec<Self>,
        },
    }
}

pub struct Eval<'model> {
    model: &'model id::Model,
}
impl<'model> Eval<'model> {
    pub fn new(model: &'model id::Model) -> Self {
        Self { model }
    }
}

impl<'expr, 'model> zip_ref::ExprZipSpec<'expr> for Eval<'model> {
    type ExprRes = Res<cst::Cst>;
    type ExprAppArgsAcc = Vec<Res<cst::Cst>>;

    fn go_up_expr_var(
        &mut self,
        var: &'expr id::Id,
    ) -> fast_expr::ZipUp<&'expr Expr, Self::ExprRes> {
        fast_expr::zip_do!(up:
            self.model
                .get(var)
                .ok_or_else(|| format!("no value available for variable `{}`", var.id()))
        )
    }
    fn go_up_expr_cst(
        &mut self,
        cst: &'expr cst::Cst,
    ) -> fast_expr::ZipUp<&'expr Expr, Self::ExprRes> {
        fast_expr::zip_do!(up:
            Ok(*cst)
        )
    }
    fn go_up_expr_app(
        &mut self,
        op: &'expr op::Op,
        args: Self::ExprAppArgsAcc,
    ) -> fast_expr::ZipUp<&'expr Expr, Self::ExprRes> {
        fast_expr::zip_do!(up:
            op.res_eval(args)
        )
    }
    fn coll_init_expr_app_args(
        &mut self,
        _op: &&'expr op::Op,
        args: &&'expr Vec<Expr>,
    ) -> fast_expr::ZipDo<Self::ExprAppArgsAcc, &'expr Expr, Self::ExprRes> {
        fast_expr::zip_do!(down: Vec::with_capacity(args.len()))
    }
    fn coll_fold_expr_app_args(
        &mut self,
        _op: &&'expr op::Op,
        (mut vec, next_res): (Self::ExprAppArgsAcc, Self::ExprRes),
    ) -> fast_expr::ZipDo<Self::ExprAppArgsAcc, &'expr Expr, Self::ExprRes> {
        vec.push(next_res);
        fast_expr::zip_do!(down: vec)
    }
}

impl Expr {
    pub fn eval(&self, model: &id::Model) -> Res<cst::Cst> {
        let eval = Eval::new(model);
        let mut zip = zip_ref::ExprZip::new(eval);

        zip.zip_expr(self)
    }
}

#[test]
pub fn test() {
    let v = "v".to_string();

    let model_with = |val: cst::ICst| {
        let mut i_model = BTreeMap::new();
        let _prev = i_model.insert(v.clone(), val);
        debug_assert_eq!(_prev, None);
        id::Model {
            i_model,
            b_model: BTreeMap::new(),
        }
    };

    let v = Expr::Var(id::Id::i_id(v.clone()));
    let seven = Expr::Cst(7.into());
    let three = Expr::Cst(3.into());

    // v + 7
    let add = Expr::App {
        op: op::IOp::TwoN(op::IOp2N::Add).into(),
        args: vec![v, seven],
    };

    // (v + 7) - 3
    let expr = Expr::App {
        op: op::IOp::TwoN(op::IOp2N::Sub).into(),
        args: vec![add, three],
    };

    // (2 + 7) - 3 = 6
    assert_eq!(expr.eval(&model_with(2)), Ok(6.into()),);
    // (10 + 7) - 3 = 14
    assert_eq!(expr.eval(&model_with(10)), Ok(14.into()),);
    // (0 + 7) - 3 = 4
    assert_eq!(expr.eval(&model_with(0)), Ok(4.into()),);
}
