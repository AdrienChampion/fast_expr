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
        // Stack of frames.
        let mut stack: Vec<Frame<S, Vec<Self>, Self>> = vec![];
        // Current expression.
        let mut expr = self;

        // Goes down `expr`.
        'go_down: loop {
            // Try to build a result, which only happens on `Expr`'s leaves: `Var` and `Cst`.
            let mut res: Self = match expr {
                // Variable, attempt the substitution and yield the result.
                Self::Var(id) => {
                    if let Some(expr) = map.get(&id) {
                        expr.clone()
                    } else {
                        Self::Var(id)
                    }
                }

                // Constant, just yield it.
                cst @ Self::Cst(_) => cst,

                // Application, we need to go down `head`.
                Self::App { op, head, tail } => {
                    // Build the frame to remember `op` and `tail`.
                    let frame = Frame::AppHead { op, tail };
                    // Push the frame on the stack.
                    stack.push(frame);
                    // Update current expression.
                    expr = *head;
                    // Go down `expr`.
                    continue 'go_down;
                }
            };

            // At this point we have a result, time to propagate it upwards thanks to the stack.
            'go_up: while let Some(frame) = stack.pop() {
                // We have a frame, let's handle it.
                match frame {
                    // We just went up from the `head` of an `App`, the frame gives us `op` and
                    // `tail` back.
                    Frame::AppHead { op, tail } => {
                        // Current result is for `head`.
                        let head = res;
                        // Initial value of the accumulator over `tail`.
                        let acc = Vec::with_capacity(tail.len());
                        // Iterator over `tail`.
                        let mut tail = tail.into_iter();

                        // Do we have a next element in `tail` to work with?
                        if let Some(next) = tail.next() {
                            // Yeah, gather the accumulator and the tail iterator.
                            let tail = (acc, tail);
                            // Build the frame remembering everything we need:
                            // - operator
                            // - head *result*
                            // - accumulator and iterator over the rest of the tail.
                            let frame = Frame::AppTail { op, head, tail };
                            // Push the frame on the stack.
                            stack.push(frame);
                            // Update current expression.
                            expr = next;
                            // Go down `expr`.
                            continue 'go_down;
                        } else {
                            // Nothing in the tail to handle, we're "going up" this application.
                            // Update the current result.
                            res = Expr::app(op, head, acc);
                            // Go up the stack with that result.
                            continue 'go_up;
                        }
                    }

                    // We just went up from an element of an application tail, the frame gives us
                    // `op`, the `head` *result*, the current accumulator value and the iterator
                    // over the rest of the tail.
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
