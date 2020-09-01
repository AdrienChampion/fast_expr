//! An explicit zipper.

prelude! {}

/// Expression specification.
pub trait Spec {
    type Val;
    type Id;
    type Op;
}

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
        head: Box<Self>,
        /// Argument tail.
        tail: Vec<Self>,
    },
}

/// Frames for [`Expr`](./struct.Expr.html).
pub enum Frame<S: Spec, AppTailAcc, Res> {
    /// Information we must remember when going down the `head` of an `Expr::App`.
    AppHead {
        /// Operator of the `Expr::App`.
        op: S::Op,
        /// Tail of the `Expr::App`.
        tail: Vec<Expr<S>>,
    },
    /// Information we must remember when going down elements of the `tail` of an `Expr::App`.
    AppTail {
        /// Operator of the `Expr::App`.
        op: S::Op,
        /// Result yielded by zipping the head.
        head: Res,
        /// Current accumulator value and an iterator over the remaining elements of the tail.
        tail: (AppTailAcc, std::vec::IntoIter<Expr<S>>),
    },
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

    pub fn subst(self, map: &BTreeMap<S::Id, Self>) -> Self
    where
        S::Id: PartialEq + Eq + PartialOrd + Ord + Clone,
        Self: Clone,
    {
        let mut stack: Vec<Frame<S, Vec<Self>, Self>> = vec![];
        let mut expr = self;

        'go_down: loop {
            let mut res: Self = match expr {
                Self::Var(id) => {
                    if let Some(expr) = map.get(&id) {
                        expr.clone()
                    } else {
                        Self::Var(id)
                    }
                }

                cst @ Self::Cst(_) => cst,

                Self::App { op, head, tail } => {
                    let frame = Frame::AppHead { op, tail };
                    stack.push(frame);
                    expr = *head;
                    continue 'go_down;
                }
            };

            'go_up: while let Some(frame) = stack.pop() {
                match frame {
                    Frame::AppHead { op, tail } => {
                        let head = res;
                        let acc = Vec::with_capacity(tail.len());
                        let mut tail = tail.into_iter();

                        if let Some(next) = tail.next() {
                            let tail = (acc, tail);
                            let frame = Frame::AppTail { op, head, tail };
                            stack.push(frame);
                            expr = next;
                            continue 'go_down;
                        } else {
                            res = Expr::app(op, head, acc);
                            continue 'go_up;
                        }
                    }

                    Frame::AppTail {
                        op,
                        head,
                        tail: (mut acc, mut tail),
                    } => {
                        let curr_elem = res;
                        acc.push(curr_elem);

                        if let Some(next) = tail.next() {
                            let frame = Frame::AppTail {
                                op,
                                head,
                                tail: (acc, tail),
                            };
                            stack.push(frame);
                            expr = next;
                            continue 'go_down;
                        } else {
                            res = Expr::app(op, head, acc);
                            continue 'go_up;
                        }
                    }
                }
            }

            assert!(stack.is_empty());
            return res;
        }
    }
}

/// Integer operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IOp {
    Add,
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

macro_rules! iexpr {
    ( ( $expr_head:tt $(+ $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Add,
            iexpr!($expr_head),
            vec![
                $(iexpr!($expr_tail), )*
            ],
        )
    };
    ( ( $expr_head:tt $(- $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Sub,
            iexpr!($expr_head),
            vec![
                $( iexpr!($expr_tail), )*
            ],
        )
    };
    ([$id:tt]) => {
        IExpr::var(stringify!($id))
    };
    ($val:expr) => {
        IExpr::cst($val)
    };
}

#[test]
fn subst_1() {
    test_subst_1()
}
pub fn test_subst_1() {
    let map = {
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

    let iexpr = iexpr! {
        (3 + [v_2] + (7 - [v_1]))
    };
    println!("iexpr:     {:?}", iexpr);

    let expected = iexpr! {
        (3 + [v_3] + (7 - 1))
    };
    println!("expected:  {:?}", expected);

    let iexpr = iexpr.subst(&map);
    println!("subst res: {:?}", iexpr);

    assert!(iexpr == expected);

    println!("no problem")
}

pub fn main() {
    test_subst_1()
}
