//! `fast_expr` generates (semi-)zippers over your expression structures automatically.
//!
//! If you are looking for simple examples, go to the [`examples`][examples] module.
//!
//! [examples]: ./examples/index.html

#[cfg(any(test, feature = "examples"))]
pub mod examples;

/// Compile the documentation with `--features examples` to see the examples.
#[cfg(not(any(test, feature = "examples")))]
pub mod examples {}

pub use fast_expr_proc::fast_expr;

/// Internal types: not dangerous, but not meant for users.
pub mod internal {
    /// An empty enumeration, *i.e.* an inhabited type.
    ///
    /// Used by fast_expr to rule out some variants of an enum.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use fast_expr::internal::Empty;
    /// let opt: Option<Empty>;
    /// // `Empty` is inhabited, so the only value `opt` can have is `None`.
    /// let opt: Option<Empty> = None;
    ///
    /// match opt {
    ///     None => (),
    ///     Some(inhabited) => match inhabited {
    ///         // `inhabited` is inhabited (!), this match has no branches and thus is
    ///         // - compatible with any type
    ///         // - will never run
    ///     },
    /// }
    /// ```
    pub enum Empty {}

    /// Some phantom data along with an [`Empty`]; this type is inhabited.
    ///
    /// Used when an enum has type parameters `'lt_1, 'lt_2, ..., T1, T2, ...` not actually used by
    /// its variants. By adding a new variant containing a `Sink<(&'lt_1 (), &'lt_2 (), ..., T1, T2,
    /// ...)>` the *unused type parameters* problem goes away, while not polluting the enum too much
    /// since `Sink` is inhabited.
    ///
    /// Internally, this is used in the frame enums generated for each expression structure.
    ///
    /// [`Empty`]: ./enum.Empty.html
    pub type Sink<T> = (std::marker::PhantomData<T>, Empty);
}

/// Stores an accumulator and an iterator, used when zipping over a collection.
///
/// Locally, zipping over a collection is akin to a `fold`. Before starting to actually zip over the
/// collection, the zipper will make users produce an initial value for the accumulator (of type
/// `Acc`, here). Then, the zipper will compute a result for each element of the collection.
///
/// Everytime it does that, it will give users the current value of the accumulator (a `CollDer<Acc,
/// Iter>`) and the result that was just computed (*i.e.* the result for the next element in the
/// collection), along with some additional information (not relevant here). Users then give back
/// the new value of the accumulator, and the zipper keeps going.
///
/// So, one needs to manipulate values of this type when implementing `fold`-like functions in
/// zipper trait implementations.
pub struct CollDer<Acc, Iter> {
    /// Accumulator of the fold over the collection.
    pub acc: Acc,
    /// Iterator over the tail of the collection.
    pub iter: Iter,
}
impl<Acc, Iter> CollDer<Acc, Iter> {
    /// Constructor.
    pub fn new(acc: Acc, iter: Iter) -> Self {
        Self { acc, iter }
    }
}

/// An order for an expression zipper.
///
/// When implementing the `Zipper` trait for a custom zipper over an expression structure, some of
/// the functions to implement will return a `ZipDo`. This means these functions have to tell the
/// zipper what to do next.
pub enum ZipDo<Down, Expr, Res> {
    /// The zipper will keep going through the structure normally by going down the inner value.
    GoDown(Down),
    /// Discard the current expression and its sub-expressions, and consider the inner value to be
    /// the result yielded by the current expression.
    GoUp(Res),
    /// Discard the current expression and replace it with the inner value.
    Subst(Expr),
    /// Abandon the whole zip process by early-returning the inner value.
    Early(Res),
}
impl<Down, Expr, Res> ZipDo<Down, Expr, Res> {
    /// Map over the `GoDown` variant.
    pub fn map_down<NewDown>(
        self,
        action: impl FnOnce(Down) -> NewDown,
    ) -> ZipDo<NewDown, Expr, Res> {
        match self {
            Self::GoDown(down) => ZipDo::GoDown(action(down)),
            Self::GoUp(up) => ZipDo::GoUp(up),
            Self::Subst(subst) => ZipDo::Subst(subst),
            Self::Early(early) => ZipDo::Early(early),
        }
    }

    /// If `GoDown`, return the result of the action; else maintain the variant and inner value.
    pub fn down_and_then<NewDown>(
        self,
        action: impl FnOnce(Down) -> ZipDo<NewDown, Expr, Res>,
    ) -> ZipDo<NewDown, Expr, Res> {
        match self {
            Self::GoDown(down) => action(down),
            Self::GoUp(up) => ZipDo::GoUp(up),
            Self::Subst(subst) => ZipDo::Subst(subst),
            Self::Early(early) => ZipDo::Early(early),
        }
    }
}
