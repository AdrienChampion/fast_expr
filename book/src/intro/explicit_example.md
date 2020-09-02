# Explicit Example

This section goes through an actual-code example of writing a slight variation of the substitution
from the [Motivation Section]. You can skip this section if you're familiar fast-expr's notion of
expression types and operations over them involving zipper-like mechanisms. This book still assumes
you read it though.

> **NB**: the full code for the example is available [below](#explicit-example-full-code). You can
> run it there, and even edit it if you want.









## Expression Type

Let's look at the expression type first. Since we're here to have fun, `Expr` is now polymorphic: it
takes a specification describing the type of its values, identifiers, and operators.

```rust
{{#include ../../fast_expr_demo/src/explicit.rs:expr_def}}
```

Nice.








## Frames

Now, we need to have some notion of *frame*. Remember that frames are created when we go down a
recursive expression variant: it stores the information needed to *i)* continue processing other
sub-expressions of that variant, and *ii)* going up said variant once we're done processing its
sub-expressions.

Do note that this example implements an **owned** substitution. That is, it will destroy (transform)
its input expression. It's important because if it only borrowed its input expression, frames would
not be defined in exactly the same way.

Anyways, the frame definition is below. It will probably differ from what a reasonable reader would expect because it is not tailored specifically for the substitution we want to perform. It is more generic than that, and can handle any fold-like `Expr` crawl.

```rust,no_run,compile_fail
{{#include ../../fast_expr_demo/src/explicit.rs:frame_def}}
```

Let's briefly discuss the first variant, `AppHead`, right away. It stores the `op` and `tail` of an
`Expr::App`, which is exactly the information we need to remember when going down its `head`.

Next is `AppTail`, which also stores the `op` of an `Expr::App`. Since `AppTail` stores information
we need to remember when going down the `tail`, we already computed the result for the `head`. So,
we need to remember it, which is what the `head` field is for. It has type `Res` which is the last
type parameter of `Frame`. `Res` is the result of the operation `Frame` will be used for. In the
context of our substitution, `Res = Expr<S>`.

Next up in `AppTail`, `tail`: the information about the `App`'s `tail` we must remember when going
down one of its elements. It's a tuple aggregating an `AppTailAcc` and an owned `Vec` iterator. The
latter should not be surprising: it contains the elements of the `Expr::App`'s `tail` we haven't
processed yet. The former is a type parameter of `Frame` that encodes information we will maintain
as we generate results for sub-expressions of the tail. In the context of our substitution, we want
`AppTailAcc = Vec<Res> = Vec<Expr<S>>` because it really represents the `tail` of the application we
will build once all elements of the original tail have been processed.

Locally, think of handling the `tail` of an `Expr::App` as a fold. For our substitution, before we
start working on the tail we will create an empty vector as the *initial* accumulator (since
`AppTailAcc = Vec<Expr<S>>`). Then, we go down elements of `tail`, and when we get a result back we
just push it on our accumulator. Once we're done, the accumulator contains the result of the
substitution of each element of the original `tail`. Since `AppTail` also stores the original `op`
and the result of the substitution of the original `head`, we have everything we need to construct a
new `Expr::App` representing the result of the substitution on the original `Expr::App`.

> Say we in fact wanted to write an iterator over the variables of an expression. It would allow,
> for instance, to put the variables in a set by side-effect `expr.iter_vars(|var| {
> my_var_set.insert(var); })`. Then, we would have `Res = ()` and `AppTailAcc = ()`, and
> monomorphization would just compile `Frame::AppTail::head` and `Frame::AppTail::tail.0` away.
> Neat!







## Substitution Function

Now that we have an expression type and a notion of frame, it's time to write the substitution
function. Readers that followed the substitution example in the [Motivation Section] should have no
problem understanding the following function. Basically, it maintains a stack of `Frame`s:

```rust,no_run,compile_fail
let mut stack: Vec<Frame<
    S,         // Expr's type parameter.
    Vec<Self>, // Tail accumulator.
    Self,      // Result of the substitution.
>> = vec![];
```

It also has a notion of current expression `expr`, which is originally the input expression (`self`,
here). Then, a `'go_down` loop goes down `expr` and either

- produces a result `res` if `expr` is a `Expr`-leaf (`Expr::Cst` or `Expr::Var` here), or
- in the case a recursive `Expr` variant
    - extracts the next sub-expression to go down into,
    - updates `expr` to be that sub-expression,
    - builds the frame remembering everything needed for later,
    - pushes that frame on the `stack`, and
    - `continue`s the `'go_down` loop.

If a result `res` was produced, the function enters a `'go_up` loop that drains the stack until

- the stack is empty, in which case `res` is the top-level result and it's done, or
- it finds a frame that still has a next sub-expression to go down into, in which case it
    - updates `expr` to be that sub-expression,
    - builds the frame remembering everything needed for later,
    - pushes that frame on the `stack`, and
    - `continue`s the `'go_down` loop.
- it finds a frame that does not have a next sub-expression to go down into, in which case it
    - builds the result for the frame (more precisely, for the `Expr` variant the frame was
      generated for),
    - updates `res` to be that result,
    - `continue`s the `'go_up` loop.

```rust,no_run,compile_fail
{{#include ../../fast_expr_demo/src/explicit.rs:subst_def}}
```

And that's it. Note that substitution does not care about what the specification is except for a few
constraints on their types. Let's end this example testing our `subst` function. To do that, we need
to have concrete specification for `Expr`. Here's one:

```rust,no_run,compile_fail
{{#include ../../fast_expr_demo/src/explicit.rs:example_spec}}
```

Nice. Last, we define a tiny DSL-macro so that we can write expressions easier, and finally define a
substitution test.

```rust,no_run,compile_fail
{{#include ../../fast_expr_demo/src/explicit.rs:example}}
```

Let's run this thing!

```bash
subst map {
    v_1 => Cst(1),
    v_2 => Var("v_3"),
}
iexpr:     App { op: Add, head: Cst(3), tail: [Var("v_2"), App { op: Sub, head: Cst(7), tail: [Var("v_1")] }] }
expected:  App { op: Add, head: Cst(3), tail: [Var("v_3"), App { op: Sub, head: Cst(7), tail: [Cst(1)] }] }
subst res: App { op: Add, head: Cst(3), tail: [Var("v_3"), App { op: Sub, head: Cst(7), tail: [Cst(1)] }] }
no problem
```

Awesome.

Remember that the full code of this example is available [just below](#explicit-example-full-code),
and that you can run it by clicking the top-right *play* button and even edit the code directly.







## Explicit Example: Full Code

```rust
{{#include ../../fast_expr_demo/src/explicit.rs}}
```

[Motivation Section]: ./motivation.html (Motivation Section)