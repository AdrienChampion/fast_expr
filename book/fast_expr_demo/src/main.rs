fn main() {
    use clap::clap_app;

    let matches = clap_app! {
        fast_expr_demo =>
            (version: clap::crate_version!())
            (author: clap::crate_authors!())
            (@subcommand explicit =>
                (about: "runs `explicit::main()`")
            )
    }
    .get_matches();

    if matches.subcommand_matches("explicit").is_some() {
        fast_expr_demo_lib::explicit::main()
    }
}
