use std::collections::HashMap as HMap;

use fast_expr::fast_expr;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Conj,
    Disj,
    Not,
}

pub type Cst = bool;
pub type Var = String;

fast_expr! {
    #![fast_expr(
        top = Expr,
        ref_impl_macro = impl_macro,
    )]
    pub enum Expr {
        Cst(Cst),
        Var(Var),
        App {
            op: Op, args: coll::Vec<Expr>,
        }
    }
}

pub struct Evaluator {
    model: HMap<Var, Cst>,
}
impl Evaluator {
    pub fn new() -> Self {
        Self { model: HMap::new() }
    }
    pub fn insert_new(&mut self, var: Var, cst: Cst) {
        let _prev = self.model.insert(var, cst);
        assert_eq!(_prev, None)
    }
}

impl_macro! {
    zip(expr: &'expr Expr => Option<Cst>) {
        Cst(cst) => up!(Some(cst)),
        Var(var) => up!(self.model.get(var).to_owned()),
        App { op, args } => zip {
            go_up => up!(args.and_then(|inner| inner)),
            fold(args => Option<Option<Cst>>) {
                init => down!(None),
                step(acc, next) => match (op, acc, next) {
                    (Op::Not, None, next) => proceed!(Some(next)),
                    (Op::Not, Some(_), next) => panic!(
                        "illegal application of `¬` to more than one argument"
                    ),

                    (Op::Disj, None, next)
                    | (Op::Conj, None, next) => proceed!(next),

                    (Op::Disj, Some(Some(true)), _)
                    | (Op::Disj, Some(_), Some(true)) => up!(Some(true)),
                    (Op::Conj, Some(Some(false)), _)
                    | (Op::Conj, Some(_), Some(false)) => up!(Some(false)),

                    (Op::Disj, Some(Some(false)), Some(false)) => proceed!(Some(false)),
                    (Op::Conj, Some(Some(true)), Some(true)) => proceed!(Some(true)),

                    (Op::Disj, Some(None), _)
                    | (Op::Disj, Some(_), None)
                    | (Op::Conj, Some(None), _)
                    | (Op::Conj, Some(_), None) => proceed!(None),
                },
            },
        }
    }
}

fn main() {
    println!("output:");
    for line in OUTPUT.lines() {
        println!("| {}", line)
    }
}