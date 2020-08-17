//! Extracts and checks stuff for the context.

prelude! {}

/// Checks a specification trait.
pub fn spec_trait(spec: &rust::Trait) -> Res<()> {
    if let Some(token) = &spec.unsafety {
        bail!(on(token, "unexpected token"))
    }
    if let Some(token) = &spec.auto_token {
        bail!(on(token, "unexpected token"))
    }
    if !spec.supertraits.is_empty() {
        bail!(on(
            &spec.ident,
            "specification trait do not take supertrait constraints"
        ))
    }
    if !spec.items.is_empty() {
        bail!(on(
            &spec.ident,
            "specification traits cannot have items in their definition"
        ))
    }

    Ok(())
}

/// Checks an expression variant.
pub fn expr_variant(variant: &rust::Variant) -> Res<()> {
    if variant.discriminant.is_some() {
        bail!(on(
            &variant.ident,
            "expression variants cannot have discriminants"
        ))
    }
    Ok(())
}
