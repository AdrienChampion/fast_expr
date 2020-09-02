# Introduction and Motivation

This book is intended as a user-manual for `fast_expr` (also written *fast-expr*) a library
providing a procedural macro generating code helping with the construction of custom *fold*/*zip*
functions over *expression types*. In fast-expr, *expression types* are (mutually-)recursive
`enum`s.

For instance,

```rust
// Defining some types to use in the expression types.
pub enum BOp { And, Or }
pub enum IOp { Add, Sub }
pub enum IRel { Ge, Gt, Le, Lt }
pub type Id = &'static str;

// Actual expression types start here.

/// Top-level expression, a boolean or integer expression.
pub enum Expr {
    Bool(BExpr),
    Int(IExpr),
}

/// Boolean expression.
pub enum BExpr {
    BCst(bool),
    BVar(Id),
    BIApp {
        op: BOp,
        args: Vec<Self>,
    },
    IRel {
        rel: IRel,
        fst: Box<IRel>,
        snd: Box<IRel>,
    },
    BIte {
        cnd: Box<Self>,
        thn: Box<Self>,
        els: Box<Self>,
    }
}

/// Integer expression.
pub enum IExpr {
    ICst(isize),
    IVar(Id),
    IApp {
        op: IOp,
        args: Vec<Self>,
    },
    IIte {
        cnd: Box<BExpr>,
        thn: Box<Self>,
        els: Box<Self>,
    }
}
```

The fast-expr macro takes such (slightly altered) definitions and generates code so that you can
easily write your own map/fold/zip-like operations over your expression types. Such operations
include (cached) evaluation, sub-expression substitution, folding, iterating, rewriting...

The rest of this introduction motivates fast-expr by going in the details of writing a fold-like
operation over expression types. It is recommended to read it, even if you are familiar with the
topic, as the discussion also lays some foundation for the rest of the book.

> Readers familiar with the notion of [derivative of algebraic data types][type der] will probably
> have an easier time understanding what fast-expr does and how it does it. Readers not familiar
> with this notion have hopefully clicked the link above and are now familiar with it.

[type der]: https://en.wikipedia.org/wiki/Generalizations_of_the_derivative#Type_theory (Derivative of a Type)