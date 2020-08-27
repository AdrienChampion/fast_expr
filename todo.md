- the `syn` dependency is build with the `full` feature; once the code is stable, try to trim it
  down

- `cxt::zip::ZipStruct` and `cxt::zip::ZipperTrait` contain many things (idents and other things)
  that are characteristic of other expression types; have them retrieve these things from the
  relevant expression contexts instead