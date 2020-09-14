# Fast-Expr Example





The whole point of fast-expr is to automate as much as possible of expression-traversal-like
operations over expressions. These operations include *evaluation*, *iteration*, *substitution* and
so on. As illustrated in the previous example, expression traversal requires

- a notion of frame, so that the traversal can remember intermediary results and the sub-expressions
  left to process;
- a function doing the actual traversal: *going down* sub-expression, storing frames, and *going up*
  to produce results.



Logically, the only part of the traversal that really changes between two operations is what *going
up* means. Evaluation produces some kind of *result value*, iteration produces nothing, and
substitution (and more generally *rewriting*) produces new expressions.


Let's re-write the previous example using fast-expr. The full code is available on the [github
repository][code], although some code snippets might differ slightly for the sake of readability. In
fact, this example is part of the fast-expr example and its documentation is on the [official crate
documentation][crate doc].

The endgame of using fast-expr is that we only need to write code that is specific to the feature
being implemented. For instance, here, the substitution is really defined by

- what to do with variables and constants
- how to handle a `tail` in an `Expr::App`:
    - init: create an empty vector of expressions, the *accumulator*
    - fold: each time a result for an element of the tail is produced, push it on the accumulator
- how to *go up* an `Expr::App` given the `op`, the result for `head`, and the final value of the
  tail accumulator (a vector of `Expr`)

> **NB**: this section focuses on a specific kind of fold/zip-like operation: substitutions.
> Fast-expr is more generic than this, and thus there are aspects that might seem useless or
> cumbersome given the specific purpose we have here. Make sure you read the [Remarks](#remarks) at
> the end of this section for a short discussion on aspects of fast-expr we mentioned but did not
> use, or omitted entirely.






## Fast-Expr Macro Call

Let's dive right in and take a look at the fast-expr way of writing the example from the previous
section. Note that we added some `derive`s on the expression type 


```rust,compile_fail,no_run
/// Expression specification.
pub trait Spec: Clone {
    /// Values (for constants).
    type Val: Clone;
    /// Identifiers (for variables).
    type Id: Clone;
    /// Operators (for applications)
    type Op: Clone;
}

fast_expr::fast_expr! {
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
            head: wrap::Box<Self>,
            /// Argument tail.
            tail: coll::Vec<Self>,
        },
    }
}
```

It is hopefully visible how little has changed compared to the [original definition][expr def].
There are exactly two changes, both in the `App` variant: the type of `head` (`tail`) has a
mysterious `wrap` (`coll`) in front of it. This design choice forces users to make explicit the
recursive parts of the expression type definition; it also makes sure users agree with fast-expr
about what `Vec` and `Box` (here) mean, in case they were re-defined in the current scope.

Refer to the official fast-expr documentation for a list of the `wrap`pers and `coll`ections
supported.

> Identifying the points of recursion in the expression type definition is crucial as fast-expr has
> to generate frames for each variant, and needs to know how to *compute the derivative* of
> recursive cases (meaning roughly *how to access and/or iterate over them*). Any misunderstanding
> between fast-expr and its users would create many barely readable type-errors. This slightly
> contrived `wrap`/`coll` syntax lets fast-expr produce clean error messages when used improperly.
>
> Eventually this kind of annotation may not be needed anymore, instead relying on `IntoIterator`
> and such.




## What You Get

The call to `fast_expr` above generates two modules (here): `zip_own` and `zip_ref`. Both are very
similar, they contain similar types/traits with the same names. The former is used to zip over
*owned* expressions, while the latter allows zipping over *borrowed* expressions. Let's focus on
`zip_own` for now as we are re-writing the substitution over owned expressions from the previous
section.

Inside `zip_own` we find `ExprFrame` (`enum`),  `ExprZipSpec` (`trait`), and `ExprZip` (`struct`):

- `ExprFrame` is pretty much the same `enum` as `Frame` from our last example, except we did not
  have to write it and won't have to maintain it if `Expr` changes 😺.
- `ExprZipSpec` is the trait we implement to specify what our zipper does, typically what *going up*
  means for the operation we are implementing. The type implementing it would also maintain whatever
  state the operation needs, such as the *substitution map* in our running example.
- `ExprZip` is in charge of maintaining the full *state* of the zipper: the stack of frames, but
  also a value of a type implementing `ExprZipSpec`. It features a `zip_expr` function which a much
  more generic version of the substitution `go_down`/`go_up` nested loops from the previous section.

This might be confusing at this point, but everything will become clear as we discuss and use
`ExprZipSpec` and `ExprZip`. Hopefully.

> Do note that `ExprFrame`, `ExprZipSpec` and `ExprZip` are tailored to the expression-type
> definition given as input of the fast-expr macro. Fast-expr generates them for you, easing the
> burden of extending/maintaining the expression type.





## Understanding `zip_own::ExprZipSpec`

Now, let's start by looking at trait `zip_own::ExprZipSpec` since that's how we specify whatever
operation we want to do. Remember that, for this substitution, we need to specify the following:

- what to do with variables and constants.
- how to handle a `tail` in an `Expr::App`:
    - *init*: create an empty vector of expressions, the *accumulator*;
    - *fold*: each time a result for an element of the tail is produced, push it on the accumulator.
- how to *go up* an `Expr::App` given the `op`, the result for `head`, and the final value of the
  tail accumulator (a vector of `Expr`).

Here is what `zip_own::ExprZipSpec` looks like after some editing for readability, followed by a
discussion point by point.

```rust,no_run,compile_fail
pub trait ExprZipSpec<S: Spec> {
    type ExprRes;

    fn go_up_expr_cst(
        &mut self, 
        elem_0: S::Val
    ) -> ZipDo<Empty, Expr<S>, Self::ExprRes>;
    fn go_up_expr_var(
        &mut self, 
        elem_0: S::Id
    ) -> ZipDo<Empty, Expr<S>, Self::ExprRes>;

    type ExprAppTailAcc;

    fn go_up_expr_app(
        &mut self, 
        op: S::Op, 
        head: Self::ExprRes, 
        tail: Self::ExprAppTailAcc
    ) -> ZipDo<Empty, Expr<S>, Self::ExprRes>;
    fn coll_init_expr_app_tail(
        &mut self, 
        op: &S::Op, 
        head: &Self::ExprRes, 
        tail: &Vec<Expr<S>>
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes>;
    fn coll_fold_expr_app_tail(
        &mut self, 
        op: &S::Op, 
        head: &Self::ExprRes, 
        tail: (Self::ExprAppTailAcc, Self::ExprRes)
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes>;
}
```

First, it has an expression specification type parameter just like our expression type. Then, we
find the `ExprRes` associated type. This is the result type of the zipper: `Expr<S>` for a
substitution, `()` for an iteration, *etc.*

> We are omitting a concrete function defined in `ExprZipSpec` as it is not needed for this example.
> Go to [the Remarks](#inspect_-functions) at the end to learn more.



### Quick Digression

We need to talk a little bit about `ZipDo`, since all abstract functions in `ExprZipSpec` mention it.
At this point, remember that a *zipper* is kind of a *fold* on steroids, in that it provides more
control on how the traversal of the expression works. `ZipDo` is the type providing this control.
`ZipDo` is (almost) defined as

```rust
pub enum ZipDo<Down, Expr, Res> {
    GoDown(Down),
    GoUp(Res),
    Subst(Expr),
    // other variant(s) omitted for readability
}
```

Since all abstract functions in `ExprZipSpec` return a `ZipDo`, these functions can control the
traversal by telling it to

- keep going down *something*: `ZipDo::GoDown`,
- going up with some result: `ZipDo::GoUp`, or
- replace the current expression with another one: `ZipDo::Subst`. We won't use this for now, but go
  to [the Remarks](#zipdosubst) at the end for a discussion on what it does.

Last, notice that all `go_up_...` functions return a `ZipDo<Empty, Expr<S>, Self::ExprRes>`. `Empty`
is defined in fast-expr; users do not manipulate values of type `Empty`, mostly because there are
none. It is an *inhabited* type:

```rust
pub enum Empty {}

// Cannot build `Empty` values, this function can never be called because we cannot construct an
// argument for it.
fn produce_anything<T>(empty: Empty) -> T {
    match empty {}
}
```

So, abstract functions that return a `ZipDo<Empty, Expr<S>, Self::ExprRes>` cannot actually
*go down*. All they can do is `GoUp` given a result (omitting `ZipDo::Subst` for now).

> **NB**: fast-expr has a `type ZipUp<E, R>` type alias for `ZipDo<Empty, E, R>`.




### Back To Understanding `ExprZipSpec` Proper

The next part of `ExprZipSpec` we need to address is this:

```rust,no_run,compile_fail
    type ExprAppTailAcc;

    fn go_up_expr_app(
        &mut self,
        op: S::Op,
        head: Self::ExprRes,
        tail: Self::ExprAppTailAcc,
    ) -> ZipDo<Empty, Expr<S>, Self::ExprRes>;
    fn coll_init_expr_app_tail(
        &mut self,
        op: &S::Op,
        head: &Self::ExprRes,
        tail: &Vec<Expr<S>>,
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes>;
    fn coll_fold_expr_app_tail(
        &mut self,
        op: &S::Op,
        head: &Self::ExprRes,
        tail: (Self::ExprAppTailAcc, Self::ExprRes),
    ) -> ZipDo<Self::ExprAppTailAcc, Expr<S>, Self::ExprRes>;
```

One might guess what associated type `ExprAppTailAcc` represents based on its name. It is a notion
of accumulator that we want to maintain as the zipper handles the `tail` part of an `Expr::App`.

One way to look at it is as the result of handling a `tail`. You can see this in the signature of
`go_up_expr_app`, which is in charge of producing a result when going up an `Expr::App`. It takes
the `op` of the `Expr::App`, the `ExprRes` for `head`, and the `ExprAppTailAcc` value for the `tail`
which is the final value of the `tail` accumulator.

So, `ExprAppTailAcc` should be whatever we need from the `tail` to *go up* an `Expr::App`:

- `Vec<Expr<S>>` for a *substitution*, since we want to construct a new tail.
- `()` for an *iteration*, since we are not producing any result.
- a (collection of) value(s) for an *evaluator*.

In practice, when the zipper first reaches a `tail`, it will call `coll_init_expr_app_tail` to
produce the initial value of the accumulator. For our substitution, we want that value to be an
empty vector of expressions: `Vec::with_capacity(tail.len())`.

The zipper then goes through each element of the tail, which yields a `Self::ExprRes`. Each time, it
will call `coll_fold_expr_app_tail` with its `tail` argument containing the current value of the
accumulator, and the result for the next element of the `tail`. So `ExprAppTailAcc`,
`coll_init_expr_app_tail` and `coll_fold_expr_app_tail` quite literally encode *fold* over the
results yielded by the elements of the tail.

> If this is confusing, go back to the previous example where we wrote the `go_down`/`go_up` loop
> and look at how `tail` is handled when going down, and up. You will find the same `init` (down)
> and `fold` (up, when the `tail` is not empty) behaviors.



## (Finally) Writing The Substitution

Now that we discussed `ExprZipSpec`, let's use it. We're writing a substitution, so let's store the
map somewhere:

```rust,no_run,compile_fail
{{#include ../../../src/examples/book_subst_1.rs:zip_def}}
```

That was the easy part. Now it is time to implement `ExprTrait` to specify what our zipper does.

> The code below features quite a lot of comments, which along with the (long) explanation above
> should be enough to understand what's going on. If not, compare the implementation with the
> `go_down`/`go_up` loop from the previous section.

```rust,no_run,compile_fail
{{#include ../../../src/examples/book_subst_1.rs:zip_impl}}
```




## Applying The Substitution

`Subst`, which implements `ExprZipSpec`, cannot zip over `Expr`essions on its own. It is merely a
specification of how the substitution operation works that we will give to an `ExprZip` so that it
can perform the actual substitution.

This is quite easy to do, given a substitution map `map` and an expression `expr`, we just have to

```rust,no_run,compile_fail
let mut zipper = ExprZip::new(Subst::from(map));
let new_expr = zipper.zip_expr(new_expr);
```

Better yet, let's write a method for `Expr` to make it nicer.

```rust,no_run,compile_fail
{{#include ../../../src/examples/book_subst_1.rs:subst_on_expr}}
```




## Remarks

### About Having a Separate `ExprZip` Type

A legitimate question is *"why have `ExprZip` at all?"*. Fast-expr could just define a `zip_expr`
function in `ExprZipSpec` that creates a stack of frames, and zips over `Expr`s right away.

Well, actually, `ExprZipSpec` has a `zip_expr` function that does just that. The main point of
`ExprZip` is to avoid re-allocating and re-growing the stack(s) of frames in high performance
contexts that work on expressions intensively.

### `ZipDo::Subst`

We did not use `ZipDo::Subst(expr)` here, which might seem odd as we are actually doing a
`Subst`itution. The purpose of `ZipDo::Subst` is to pretend to perform a substitution without
actually doing it, *i.e.* without constructing the result of the substitution. This is useful in
particular when evaluating an expression (taken by reference). Sometimes, a variable `v` in an
expression represents another sub-expression, which we need to evaluate to know what value `v`
yields.

Here are two (bad) ways to do this. One is to *inline* all these variables by their definition, at
the cost of constructing a new (bigger) expression solely for the purpose of the evaluation. Another
way is, when we reach one such variable, to start a new evaluation process from scratch on the
expression it represents; this means we can have an arbitrary number of nested evaluation-zippers
traversals.

A better way to handle the problem is to yield a `ZipDo::Subst(expr)` when we reach one such
variable; assuming `expr` is what the variable represents. This tells the zipper to replace whatever
expression it's handling (the variable, here), by `expr` and resume the zipping process. This is
exactly what we want here, without the drawbacks of the two approaches above.

### `inspect_...` Functions

This section completely ignored the concrete functions of `ExprZipSpec` called that *inspect*
expressions. In this example, there is only one, called `inspect_expr`:

```rust,no_run,compile_fail
fn inspect_expr(
    &mut self,
    expr: Expr<S>
) -> ZipDo<Expr<S>, Expr<S>, Self::ExprRes> {
    ZipDo::GoDown(expr)
}
```

Whenever the zipper is about to pattern-match an `Expr` to decide what to do with it, it first calls
`inspect_expr`. Since this function returns a `ZipDo`, it provides an opportunity when implementing
`ExprZipSpec` to short-circuit the treatment of an expression early-on, before the zipper even looks
at it.

Fast-expr generates a *default* definition that tells the zipper to keep going down the current
expression because it expects that most operations will traverse the whole expression. Any
implementation of `ExprZipSpec` can override this default definition.

One would typically do exactly that when specifying a *cached* operation, for instance cached
evaluation. Assume we are implementing `ExprZipSpec` for some type featuring a `try_get_res(&expr)`
method that looks in the cache and returns an optional result value; then, we would re-define
`inspect_expr` as

```rust,no_run,compile_fail
fn inspect_expr(
    &mut self,
    expr: Expr<S>,
) -> ZipDo<Expr<S>, Expr<S>, Self::ExprRes> {
    self.try_get_res(&expr).map(ZipDo::GoUp).unwrap_or_else(|| ZipDo::GoDown(expr))
}
```

> **NB**: our evaluator would probably take expressions as references. Although it does not make a
> lot of sense, we stuck with the owned version to preserve consistency across the section.



[code]: https://github.com/AdrienChampion/fast_expr/blob/master/src/examples/book_subst_1.rs (Code for this example on github.com)
[expr def]: explicit_example.html#expression-type (Original expression type definition)
[crate doc]: TODO
[example doc]: TODO