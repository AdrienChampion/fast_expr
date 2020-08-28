use fast_expr_gen::{from_stream, quote::quote};

fn check_error<T>(res: Result<T, impl std::fmt::Display>, msg: impl AsRef<str>) {
    if let Err(e) = res {
        assert_eq!(e.to_string(), msg.as_ref())
    } else {
        panic!("expected error, got okay result")
    }
}

#[test]
fn wrong_spec_trait() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, UOp> {}

        pub enum Expr<Spec> {
            Var(Spec::Var),
            Cst(Spec::Cst),
            UApp {
                op: Spec::UOp,
                arg: wrap::Box<Self>,
            },
        }
    };

    check_error(
        from_stream(tokens),
        "unknown specification trait; available specification traits: `ESpec`",
    )
}

#[test]
fn wrong_too_many_params() {
    let tokens = quote! {
        spec trait ESpec<Var, UOp> {}

        pub enum Expr<ESpec, Cst> {
            Var(Spec::Var),
            Cst(Cst),
            UApp {
                op: Spec::UOp,
                arg: wrap::Box<Self>,
            },
        }
    };

    check_error(
        from_stream(tokens),
        "expression types can only have one specification-trait type parameter",
    )
}

#[test]
fn no_sub_expr_param() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, UOp> {}

        pub enum Expr<ESpec> {
            B(BExpr),
        }

        pub enum BExpr<ESpec> {
            Var(ESpec::Var),
            Cst(ESpec::Cst),
            UApp {
                op: Spec::UOp,
                arg: wrap::Box<Self>,
            }
        }
    };

    check_error(
        from_stream(tokens),
        "this expression type takes a specification trait, expected inference parameter `_`",
    );
}

#[test]
fn wrong_sub_expr_param() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, UOp> {}

        pub enum Expr<ESpec> {
            B(BExpr<ESpec::Cst>),
        }

        pub enum BExpr<ESpec> {
            Var(ESpec::Var),
            Cst(ESpec::Cst),
            UApp {
                op: Spec::UOp,
                arg: wrap::Box<Self>,
            }
        }
    };

    check_error(from_stream(tokens), "expected inference parameter `_`");
}

#[test]
fn unknown_one_wrapper() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(ESpec::Var),
            Cst(ESpec::Cst),
            App {
                op: Spec::Op,
                arg: wrap::Bo<Self>,
            }
        }
    };

    check_error(from_stream(tokens), "unknown wrapper type");
}

#[test]
fn unknown_coll_wrapper() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(ESpec::Var),
            Cst(ESpec::Cst),
            App {
                op: Spec::Op,
                args: coll::Ve<Self>,
            }
        }
    };

    check_error(from_stream(tokens), "unknown collection type");
}

#[test]
fn nested_wrappers() {
    let tokens = quote! {
        spec trait ESpec<Var, Cst, Op> {}

        pub enum Expr<ESpec> {
            B(BExpr<_>),
        }

        pub enum BExpr<ESpec> {
            Var(ESpec::Var),
            Cst(ESpec::Cst),
            App {
                op: Spec::Op,
                args: coll::Vec<wrap::Box<Self>>,
            }
        }
    };

    check_error(
        from_stream(tokens),
        "illegal nesting of expression type wrappers",
    );
}
