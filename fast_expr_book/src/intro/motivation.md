# Motivation

The problem fast-expr solves (hopefully) is that expression types require a lot of boilerplate code
to make working with them comfortable. Operations like iteration, substitution, evaluation... have
to *go down* the tree structure of an expression (an expression-type value), then *go up* looking
for elements left to process, go down again... until the operation has been applied to the whole
expression.

As an example, let's define an expression type.


```rust,no_run
/// Integer operator.
pub enum IOp {
    /// Addition.
    Add,
    /// Subtraction.
    Sub,
}
/// Integer expression.
pub enum Expr {
    Cst(isize),
    Var(&'static str),
    App {
        op: IOp,
        head: Box<Self>,
        tail: Vec<Self>,
    },
}
```

Say now we want to define *substitution* over expressions given a map from variables to expressions.
The next section will do precisely that with actual code, but for now let's just see how it would
work informally, on this example:

```rust,no_run,compile_fail
// 7 + v_1 + (v_2 - v_3)
App {
    op: Add,
    head: Cst(7),
    tail: vec![
        Var("v_1"),
        App {
            op: Sub
            head: Var("v_2")
            tail: vec![
                Var("v_3")
            ],
        }
    ],
}
```

First, we need to *go down* the `App`, and

- *go down* the `head`, while remembering `op` and the `tail`. Let's call this information a *frame*
  (for `App`);
    - *apply* the substitution over `head` (a constant, nothing happens);

- *go back up* to the `App` frame, see that there arguments in the tail, and *go down* the first one
  while remembering `op`, the rest of `tail`, **and** the *result value* yielded by *going up* from
  the `head`;
    - *apply* the substitution over that second argument (a variable, so we get a new expression)

- *go back up* to the `App` frame and repeat the process for the last argument, while remembering
  `op`, the (empty) rest of `tail`, and **both** result values yielded by the `head` and the first
  `tail` element;
    - ...

- *go back up* to the `App` frame, see we processed all elements of the `tail`, and build the new
  `App` with the original `op`, the `head` result, and the vector of `tail` results;

- and, finally, we're done.

This kind of structure crawling is very similar to a *fold*/*iter*/*map* operation over linear
structures like collections, *e.g.* lists. For more complex operations however, one might want to
skip sub-expressions, *back-jump* to a higher point in the crawling/fold process, or perform
substitutions on the fly. This kind of operation is significantly more complex and flexible than a
traditional fold/iter/map and is usually done with a *zipper*, or something very close to a
*zipper*.

If you think of expressions as trees, then a *zipper* maintains all the information needed to crawl
in that tree with the possibility of going

- down a sub-expression,
- up a super-expression, and
- left and right, to siblings of a node (such as `App` in our example).

A zipper is a bit like a joystick allowing to move around a structure arbitrarily.

> **NB**: the notion of [zipper][zipper] was originally introduced to realize a (variation of a)
> map-like operation. Readers familiar with the original notion should expect fast-expr to take some
> liberty with it, although knowledge of [`xmonad`]-style zippers will probably ease the experience
> of learning about fast-expr. It would be probably better to say that fast-expr generates code for
> *semi-zippers*, meaning fast-expr provides much more control than one gets with a regular
> structural fold but not technically as powerful as zippers.

The point here is that, if you are interested in complex and flexible operations over
(mutually-)recursive expression types, then the amount of boilerplate code quickly becomes very
large. Since this is a Rust library, consider also that you cannot really use the same notion of
frame or the same zip/fold functions if you own the expression or if you are borrowing it.
Evaluation would definitely borrow expressions, but substitution might not. Also consider that
expression-types can be polymorphic. In the example above, `IOp` could be a type parameter of `Expr`
for instance. Spoiler alert, this will result in more tedious boilerplate.

Writing the boilerplate code is tedious and repetitive (and thus error-prone), but it's not that
difficult to do once you get the hang of it. Much more difficult however is the process of adding
variants to expression types, as this will impact most of the boilerplate code. This is the main
reason fast-expr exists: it takes care of generating and maintaining all that for ~~me~~ you.

[zipper]: https://en.wikipedia.org/wiki/Zipper_(data_structure) (Zipper on Wikipedia)
[`xmonad`]: https://en.wikipedia.org/wiki/Xmonad (xmonad on Wikipedia)