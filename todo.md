- the `syn` dependency is build with the `full` feature; once the code is stable, try to trim it
  down

- add a `switch_step` function to `<Expr>Zip`

- have `wrap` and `coll` be actual modules in `fast_expr`

- generate a macro for each trait that eases the process of implementing them

    ```rust
    impl<...> zip_own::ExprZipper<...> for MyOperation<...> {

        zipper_impl_ref! {
            Expr<S> where Res = <type> {

                variants: |&mut self| {
                    Var(var: S::Var) => {
                        go_up => <expr>,
                    },
                    Cst(cst: S::Cst) => {
                        go_up => <expr>,
                    },
                    App { op: S::Op, head: Expr<S>, tail: Vec<Expr<S>> } => {
                        go_up => <expr>,
                        coll(tail: std::vec::IntoIter<S>) where Acc = <type> => {
                            init => <expr>,
                            fold => <expr>,
                        },
                    },
                },

                // Optional
                inspect: |&mut self, expr| {
                    <expr>
                },
                // or
                inspect: |&mut self, expr| <expr>,
            }
        }

    }
    ```