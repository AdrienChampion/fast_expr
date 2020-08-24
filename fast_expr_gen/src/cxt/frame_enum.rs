prelude! {}

#[derive(Debug, Clone)]
pub struct FrameEnum {
    e_idx: idx::Expr,
    id: rust::Id,

    own_generics: rust::Generics,
    ref_generics: rust::Generics,
}
