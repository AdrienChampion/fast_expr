//! Fast expr derive.

extern crate proc_macro;

fast_expr_gen::prelude! {}

use fast_expr_gen::syn;

/// The whole point.
///
/// Roughly-speaking, this macro takes one or more (mutually-)recursive enum definitions. In the
/// following we will call these types *expression types*, and their values *expressions*. The macro
/// generates the expression type definitions (almost untouched) and, by default, two modules:
///
/// - `zip_ref`, for zipping over expression references, and
/// - `zip_own`, for zipping over owned expression.
///
/// For each (by default) expression type `Expr`, both of these modules will contain
///
/// - an `ExprZipSpec` trait, used to write zippers over your expressions, and
/// - an `ExprZip` struct that leverages the zipper traits to actually zip over an owned expression
///   (`zip_own`) or an expression reference (`zip_ref`).
///
///
/// Below is discussed how to
///
/// - write [expression type definitions](#expression-type-definitions), and in particular
///   [recursive variants](#recursive-variants) and [specification traits (type
///   parameters)](#specification-traits-expression-type-parameters);
/// - [customize the macro's behavior](#configuration), by acting on the [global
///   configuration](#global-configuration) or on [expression-specific
///   configurations](#local-configuration).
///
///
///
///
///
///
///
///
///
///
///
/// # Expression Type Definitions
///
/// This macro has several deviations compared to pure-Rust enum definition.
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
/// ## Recursive Variants
///
/// The whole point of `fast_expr` is to work over (mutually-)recursive expression types. In Rust,
/// and assuming we have some types `Op`, `UnOp`, `Id` and `Val` in scope, a simple example of
/// such a type could be:
///
/// ```rust
/// # pub type Op = ();
/// # pub type UnOp = ();
/// # pub type Id = ();
/// # pub type Val = ();
/// pub enum Expr {
///     Cst(Val),
///     Id(Id),
///     UnApp { op: UnOp, arg: Box<Self> },
///     App { op: Op, args: Vec<Self> },
/// }
/// # fn main() {}
/// ```
///
/// Both appearances of `Self` have to be *wrapped* in some way, otherwise `Expr` would not be
/// well-formed. We do this here with
///
/// - a `Box` in the `UnApp` variant (a pointer ot the inner `Self`, stored on the heap);
/// - a `Vec` in the `App` variant, which stores its element on the heap.
///
/// `Expr` does not directly stores value of itself, meaning it is well-formed and its size is known
/// at compile-time.
///
/// Now, as a zipper-generation library, `fast_expr` needs to understand the wrappers (*e.g.* `Box`)
/// and collections (*e.g.* `Vec`) used in the expression types. *Currently*, this means that
/// `fast_expr` requires you to prefix wrappers and collections with a special prefix. Here are the
/// wrappers/collections supported and how to write them, with `E` some expression type.
///
/// | type | `fast_expr` syntax | notes |
/// |:----:|:------------------:|:------|
/// | `E` | `E` | beware that your types must be well-formed |
/// | `std::box::Box<E>` | `wrap::Box<E>` |
/// | `std::vec::Vec<E>` | `coll::Vec<E>` |
/// | `std::collections::HashSet<E>` | `coll::HashSet<E>` |
/// | `std::collections::BTreeSet<E>` | `coll::BTreeSet<E>` |
///
/// As an example, `Expr` in the previous example would be given to `fast_expr` as
///
/// ```rust
/// # use fast_expr_proc::fast_expr;
/// # pub type Op = ();
/// # pub type UnOp = ();
/// # pub type Id = ();
/// # pub type Val = ();
/// fast_expr! {
/// # #![fast_expr(name = fast_expr_gen)]
///     pub enum Expr {
///         Cst(Val),
///         Id(Id),
///         UnApp { op: UnOp, arg: wrap::Box<Self> },
///         App { op: Op, args: coll::Vec<Self> },
///     }
/// }
/// # fn main() {}
/// ```
///
/// > **NB**: note that nested wrappers and/or collections are currently not supported.
///
///
///
///
///
///
///
///
/// ## Specification Traits (Expression Type Parameters)
///
/// Expression types can have type parameters, but they are quite constrained. Crucially, a group of
/// *mutually-recursive* expressions **must all have the same type parameters**. Note in particular
/// that you will not be able (currently) to define a polymorphic expression type, and instantiate
/// it with different type arguments in different expression-type-variants. Handling this kind of
/// use-case is possible but extremely tricky, and is not implemented at the moment.
///
/// In `fast_expr`, we call the type parameters of an expression type its *specification*.
///
/// Now, since mutually-recursive expressions must all have the same specification, there is really
/// no point in manually passing them every time. This is why `fast_expr` lets you write `Expr<_>`
/// in expression-type-variants.
///
/// Let's define two mutually recursive expression type: one represents boolean expression, the
/// other integer expressions. Also, let's have the operators as type parameters:
///
/// - `BOp`: operators over some boolean arguments, *e.g.* `and`, `or`, `not`;
/// - `IRel`: relations over some integer arguments, *e.g.* `>`, `≤`;
/// - `IOp`: operators over some integer arguments, *e.g.* `+`, `-`.
///
/// ```rust
/// # use fast_expr_proc::fast_expr;
/// fast_expr! {
/// # #![fast_expr(name = fast_expr_gen)]
///     pub enum Expr<BOp, IRel, IOp> {
///         B(BExpr<_>),
///         I(IExpr<_>),
///     }
///     pub enum BExpr<BOp, IRel, IOp> {
///         Cst(bool),
///         Id(String),
///         App { op: BOp, args: coll::Vec<Self> },
///         Ite { cnd: wrap::Box<Self>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///         IRel { rel: IRel, args: coll::Vec<IExpr<_>> },
///     }
///     pub enum IExpr<BOp, IRel, IOp> {
///         Cst(isize),
///         Id(String),
///         App { op: IOp, args: coll::Vec<Self> },
///         Ite { cnd: BExpr<_>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///     }
/// }
/// # fn main() {}
/// ```
///
/// > Note that in `Expr` and in `IExpr::Ite`, some expression types appear without a
/// > wrapper/collection. This is supported by `fast_expr`, but you need to make sure the types are
/// > well-formed since Rust still needs to compile them. Which is the case here.
///
/// Now, if you have many expression types this can become tedious fast. Also, you may want to have
/// constraints over these type parameters, further increasing the verbosity. As as example, let's
/// have some traits that gather the three type parameters above.
///
/// ```rust
/// # use fast_expr_proc::fast_expr;
/// pub trait BoolSpec {
///     type BOp;
///     type IRel;
/// }
/// pub trait IntSpec {
///     type IOp;
/// }
/// fast_expr! {
/// # #![fast_expr(name = fast_expr_gen)]
///     pub enum Expr<BSpec, ISpec>
///     where
///         BSpec: BoolSpec,
///         ISpec: IntSpec,
///     {
///         B(BExpr<_>),
///         I(IExpr<_>),
///     }
///     pub enum BExpr<BSpec, ISpec>
///     where
///         BSpec: BoolSpec,
///         ISpec: IntSpec,
///     {
///         Cst(bool),
///         Id(String),
///         App { op: BSpec::BOp, args: coll::Vec<Self> },
///         Ite { cnd: wrap::Box<Self>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///         IRel { rel: BSpec::IRel, args: coll::Vec<IExpr<_>> },
///     }
///     pub enum IExpr<BSpec, ISpec>
///     where
///         BSpec: BoolSpec,
///         ISpec: IntSpec,
///     {
///         Cst(isize),
///         Id(String),
///         App { op: ISpec::IOp, args: coll::Vec<Self> },
///         Ite { cnd: BExpr<_>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///     }
/// }
/// # fn main() {}
/// ```
///
/// We can make this process less redundant (and thus more maintainable) by declaring a *spec
/// trait*. A spec trait is a special `fast_expr` construct that factors out expression type
/// parameters. The syntax of a spec trait is:
///
/// ```rust,compile_fail
/// spec trait SpecId<...>
/// where
///     ...
/// {}
/// ```
///
/// This simply aggregates some type parameters and a where-clause, and allows refering to them as
/// `SpecId` (here, the name can be anything you want) in expression type declarations. More
/// precisely, writing an expression type as `pub enum Expr<SpecId> { ... }` means that `Expr` has
/// the same type parameters and where-clause as `SpecId`.
///
/// Let's rewrite our example using a specification trait.
///
/// ```rust
/// # use fast_expr_proc::fast_expr;
/// pub trait BoolSpec {
///     type BOp;
///     type IRel;
/// }
/// pub trait IntSpec {
///     type IOp;
/// }
/// fast_expr! {
/// # #![fast_expr(name = fast_expr_gen)]
///     spec trait ESpec<BSpec, ISpec>
///     where
///         BSpec: BoolSpec,
///         ISpec: IntSpec,
///     {}
///
///     pub enum Expr<ESpec> {
///         B(BExpr<_>),
///         I(IExpr<_>),
///     }
///     pub enum BExpr<ESpec> {
///         Cst(bool),
///         Id(String),
///         App { op: BSpec::BOp, args: coll::Vec<Self> },
///         Ite { cnd: wrap::Box<Self>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///         IRel { rel: BSpec::IRel, args: coll::Vec<IExpr<_>> },
///     }
///     pub enum IExpr<ESpec> {
///         Cst(isize),
///         Id(String),
///         App { op: ISpec::IOp, args: coll::Vec<Self> },
///         Ite { cnd: BExpr<_>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///     }
/// }
/// # fn main() {}
/// ```
///
/// Thanks to specification traits, modifying the type parameters / constraints of any group of
/// mutually-recursive expression types consists in modifying one list of type parameters /
/// constraints (that of the specification trait), and possibly the variants impacted by that
/// change (which is not avoidable anyway).
///
///
///
///
///
///
///
///
///
///
///
/// # Configuration
///
/// This macro's behavior can be configured in two ways.
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
/// ## Global Configuration
///
/// > Examples available [below](#examples-configuration).
///
/// To act on the global behavior, start the macro's input with **inner** attributes of the form
/// `#![fast_expr(<options>)]`, where `<options>` is a comma-separated list (trailing comma allowed)
/// of
///
/// - `name = <ident>`: changes the name of the `fast_expr` crate (default `fast_expr`);
/// - `top = <ExprIdent>`: specifies `<ExprIdent>` as the *top expression type*, meaning no other
///   expression type will a zipper-trait or a zipper-struct; `<ExprIdent>` must be one of the
///   expression types;
///   
///   (default none, can be overridden by local configurations)
/// - `all_pub = <bool>`: if true, forces the macro to expose types and functions normally hidden;
///   there is no reason to do this except for debug/pedagogy purposes;
///   
///   (default `false`)
///   
///   (`all_pub` is equivalent to `all_pub = true`)
/// - `ref_gen = <bool>`: (de)activates code-generation of the `zip_ref` module;
///
///   (default `true`)
///   
///   (`ref_gen` is equivalent to `ref_gen = true`)
/// - `own_gen = <bool>`: (de)activates code-generation of the `zip_own` module;
///
///   (default `true`)
///   
///   (`own_gen` is equivalent to `own_gen = true`)
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
///
/// ## Local Configuration
///
/// > Examples available [below](#examples-configuration).
///
/// The second kind of configuration is local to a single expression type, written as **outer**
/// attributes for the expression type definition. These have form `#[fast_expr(<options>)]`, where
/// `<options>` is a comma-separated list (trailing comma allowed) of
///
/// - `zip_gen = <bool>`: (de)activates code-generation of the zipper-trait and zipper-struct;
///
///   (default `false` if the global configuration has a top expression type different from `Self`)
///   
///   (default `true` if the global configuration has Self or nothing as the top expression type)
///   
///   (`zip_gen` is equivalent to `zip_gen = true`)
///
///
///
///
///
///
///
///
///
/// ## Examples (Configuration)
///
/// ```rust
/// # use fast_expr_proc::fast_expr;
/// pub trait BoolSpec {
///     type BOp;
///     type IRel;
/// }
/// pub trait IntSpec {
///     type IOp;
/// }
///
/// # pub mod fast_expr {
/// #     pub use fast_expr_gen::*;
/// # }
/// # pub mod fast_expr_gen {}
/// /// Pretending we renamed `fast_expr` to `fexpr`.
/// pub mod fexpr {
///     pub use crate::fast_expr::*;
/// }
///
/// fast_expr! {
///     #![fast_expr(
///         // use `fexpr` to refer to fast_expr instead of `fast_expr`
///         name = fexpr,
///         // only generate zip-struct and zipper-trait for `Expr`
///         top = Expr,
///         // do not generate the code for zipping over owned expressions
///         own_gen = false,
///     )]
///     spec trait ESpec<BSpec, ISpec>
///     where
///         BSpec: BoolSpec,
///         ISpec: IntSpec,
///     {}
///
///     pub enum Expr<ESpec> {
///         B(BExpr<_>),
///         I(IExpr<_>),
///     }
///
///     #[fast_expr(
///         // override `top` and do generate zip-code for this expression
///         zip_gen,
///         // equivalent to `zip_gen = true`
///     )]
///     pub enum BExpr<ESpec> {
///         Cst(bool),
///         Id(String),
///         App { op: BSpec::BOp, args: coll::Vec<Self> },
///         Ite { cnd: wrap::Box<Self>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///         IRel { rel: BSpec::IRel, args: coll::Vec<IExpr<_>> },
///     }
///
///     pub enum IExpr<ESpec> {
///         Cst(isize),
///         Id(String),
///         App { op: ISpec::IOp, args: coll::Vec<Self> },
///         Ite { cnd: BExpr<_>, thn: wrap::Box<Self>, els: wrap::Box<Self> },
///     }
/// }
/// fn main() {
/// #     #[allow(dead_code)]
///     use zip_ref::{
///         ExprZip, ExprZipSpec,
///         // Generated because of the override for zip-generation on `BExpr`.
///         BExprZip, BExprZipSpec,
///         // // Not generated because of global `top = Expr`, does not exist.
///         // IExprZip, IExprZipSpec,
///     };
///
///     // // Not generated because of global `own_gen = false`, does not exist.
///     // use zip_own::*;
///
///     // ...
/// }
/// ```
#[proc_macro]
pub fn fast_expr(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let top = syn::parse_macro_input!(stream as front::Top);

    let res = fast_expr_gen::generate_context(top)
        .map(|top| proc_macro::TokenStream::from(top.to_token_stream()));

    match res {
        Ok(res) => res,
        Err(e) => proc_macro::TokenStream::from(e.to_compile_error()),
    }
}
