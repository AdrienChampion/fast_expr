- the `syn` dependency is build with the `full` feature; once the code is stable, try to trim it
  down

- add a `switch_step` function to `<Expr>Zip`

- user-side conf: global conf as inner attributes, expression-specific as outter attributes
    - let users specify a top expression
    - allow overriding the `fast_expr` path to the library