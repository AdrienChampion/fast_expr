//! An explicit substitution example.

use std::collections::BTreeMap;

// ANCHOR: expr_def
/// Expression specification.
pub trait Spec {
    /// Values (for constants).
    type Val;
    /// Identifiers (for variables).
    type Id;
    /// Operators (for applications)
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
// ANCHOR_END: expr_def

// ANCHOR: frame_def
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
// ANCHOR_END: frame_def

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
}

// ANCHOR: subst_def
impl<S: Spec> Expr<S> {
    /// Substitution over the variables of an expression.
    pub fn subst(self, map: &BTreeMap<S::Id, Self>) -> Self
    where
        S::Id: PartialEq + Eq + PartialOrd + Ord,
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

                    // We just went up from an element of an application tail, the frame gives us...
                    Frame::AppTail {
                        // ...the operator of the application...
                        op,
                        // ...the `head` *result*...
                        head,
                        // ...the current accumulator value and the iterator over the rest of the
                        // tail.
                        tail: (mut acc, mut tail),
                    } => {
                        // `res` contains the result for the element of the tail we going up from.
                        let curr_elem = res;
                        // Add it to the accumulator.
                        acc.push(curr_elem);

                        // Do we have a next element?
                        if let Some(next) = tail.next() {
                            // Yes, build the new frame.
                            let frame = Frame::AppTail {
                                op,
                                head,
                                tail: (acc, tail),
                            };
                            // Push the frame on the stack.
                            stack.push(frame);
                            // Update the current expression.
                            expr = next;
                            // Go down `expr`.
                            continue 'go_down;
                        } else {
                            // We're done with the element of this application, build its result.
                            res = Expr::app(op, head, acc);
                            // Go up with that result.
                            continue 'go_up;
                        }
                    }
                }
            }

            // This point is only reachable if the stack is empty.
            assert!(stack.is_empty());
            // `res` thus contains the result for going up from the top-most expression, which is
            // the result of the whole substitution process.
            return res;
        }
    }
}
// ANCHOR_END: subst_def

// ANCHOR: example_spec
/// Integer operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum IOp {
    /// Addition.
    Add,
    /// Subtraction.
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
// ANCHOR_END: example_spec

// ANCHOR: example
/// Helper macro to build expressions easily.
macro_rules! iexpr {
    // `Add` application.
    ( ( $expr_head:tt $(+ $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Add,
            iexpr!($expr_head),
            vec![
                $(iexpr!($expr_tail), )*
            ],
        )
    };
    // `Sub` application.
    ( ( $expr_head:tt $(- $expr_tail:tt)* ) ) => {
        IExpr::app(
            IOp::Sub,
            iexpr!($expr_head),
            vec![
                $( iexpr!($expr_tail), )*
            ],
        )
    };
    // `Id` as a token, must be given between `[...]`.
    ([$id:tt]) => {
        IExpr::var(stringify!($id))
    };
    // `Cst` as an expression.
    ($val:expr) => {
        IExpr::cst($val)
    };
}

/// Tests `Expr::subst`.
pub fn test_subst_1() {
    // Substitution map.
    let map: BTreeMap<
        &'static str, // ISpec::Id
        IExpr,        // Expr<ISpec>
    > = {
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

    // Test expression we'll `subst(map)` soon.
    let iexpr = iexpr! {
        (3 + [v_2] + (7 - [v_1]))
    };
    println!("iexpr:     {:?}", iexpr);

    // Expected result.
    let expected = iexpr! {
        (3 + [v_3] + (7 - 1))
    };
    println!("expected:  {:?}", expected);

    // Apply the substitution.
    let iexpr = iexpr.subst(&map);
    println!("subst res: {:?}", iexpr);

    // Check the result.
    assert!(iexpr == expected);

    println!("no problem")
}
// ANCHOR_END: example

#[test]
fn subst_1() {
    test_subst_1()
}

pub fn main() {
    test_subst_1()
}
