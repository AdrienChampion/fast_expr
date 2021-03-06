//! Code generation crate for fast-expr.

pub extern crate quote;
pub extern crate syn;

#[macro_use]
pub mod macros;

pub mod check;
pub mod conf;
pub mod cxt;
pub mod doc;
pub mod err;
pub mod expr;
pub mod front;
pub mod gen;
pub mod prelude;
pub mod rust;

prelude! {}

/// Procedural macro entry-point, yields a top context for zipper generation.
pub fn generate_context(top: front::Top) -> Res<cxt::ZipTop> {
    cxt::Top::new(top).map(|top| {
        top.dbg_log_to_file();
        top
    })
}

/// Parses a stream of tokens.
///
/// This is used in tests to check the result of a `quote!` call.
pub fn from_stream(stream: TokenStream) -> Res<TokenStream> {
    let top = syn::parse_quote!(#stream);
    generate_context(top).map(|top| top.to_token_stream())
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

pub type ZipUp<Expr, Res> = ZipDo<internal::Empty, Expr, Res>;

/// An order for an expression zipper.
///
/// When implementing the `Zipper` trait for a custom zipper over an expression structure, some of
/// the functions to implement will return a `ZipDo`. This means these functions have to tell the
/// zipper what to do next.
pub enum ZipDo<Down, Expr, Res> {
    /// The zipper will keep going through the structure following the normal flow.
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
    pub fn map_go_down<NewDown>(
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
    pub fn go_down_and_then<NewDown>(
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
impl<Expr, Res> ZipDo<internal::Empty, Expr, Res> {
    /// Converts the `Down` type parameter from `Empty` to anything.
    pub fn empty_convert<Down>(self) -> ZipDo<Down, Expr, Res> {
        self.map_go_down(|empty| match empty {})
    }
}

/// Internal types: not dangerous, but not meant for users.
pub mod internal {
    /// An empty enumeration, *i.e.* an inhabited type.
    ///
    /// Used by fast_expr to rule out some variants of an enum.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use fast_expr_gen::internal::Empty;
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
