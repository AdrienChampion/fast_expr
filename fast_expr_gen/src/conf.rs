//! Configuration info given as attributes by users.
//!
//! There are two kind of configuration information:
//!
//! - top-level: [`Conf`][conf]
//! - expression-level: [`EConf`][econf]
//!
//! Users write their top-level configuration, if any, as inner attributes at the start of the
//! proc-macro call. An expression-level configuration must be written as (an) outer attribute(s)
//! for each expression type.
//!
//! Please refer to the examples in the official documentation for details as to how this works.
//!
//! [conf]: ./struct.Conf.html
//! [econf]: ./struct.EConf.html

prelude! {}

/// A configuration value, which tracks if it's a default or provided by the user.
///
/// Stores the actual value, and an optional `Span` corresponding to where the user gave us that
/// value. If there is no `Span`, it means the value does not come from the user, *i.e.* it is the
/// default value we generated.
///
/// The span info is mostly there to produce better error messages.
///
/// **NB**: this type `Deref`s to `T` for convenience.
#[derive(Debug, Clone)]
pub struct Val<T> {
    /// Actual value.
    val: T,
    /// Location of origin of the value, if any. `None` indicates the value is a default generated
    /// by fast_expr.
    from_user_span: Option<rust::Span>,
}
impl<T> Val<T> {
    /// Creates a new default value.
    fn new_default(val: impl Into<T>) -> Self {
        Self {
            val: val.into(),
            from_user_span: None,
        }
    }
    /// Sets the value, and remembers `src` as the location responsible for this set.
    pub fn set(&mut self, val: impl Into<T>, src: rust::Span) {
        self.val = val.into();
        self.from_user_span = Some(src);
    }

    /// True if the value comes from the user.
    pub fn is_from_user(&self) -> bool {
        self.from_user_span.is_some()
    }
    /// The location where this value comes from, none if fast_expr generated the value.
    pub fn from_user_span(&self) -> Option<rust::Span> {
        self.from_user_span.clone()
    }

    /// Maps over a reference to the actual value.
    pub fn ref_map<Out>(&self, action: impl FnOnce(&T) -> Out) -> Val<Out> {
        Val {
            val: action(&self.val),
            from_user_span: self.from_user_span.clone(),
        }
    }
}
implement! {
    impl(T) Val<T> {
        Deref<T> {
            |self| &self.val
        }
    }
}

/// Fast_expr configuration for a specific expression.
#[derive(Debug, Clone)]
pub struct EConf {
    /// Controls whether to actually generate the expression's `Zip` struct and `Zipper` trait.
    pub zip_gen: Val<bool>,
}

impl EConf {
    /// Key for the `zip_gen` field.
    const KEY_ZIP_GEN: &'static str = "zip_gen";
    /// All keys accepted by `Self`.
    const KEYS: [&'static str; 1] = [Self::KEY_ZIP_GEN];
    /// String containing a comma-separated enumeration of the legal keys for `Self`.
    fn key_list() -> String {
        let mut s = String::new();
        let mut pref = "";
        for key in &Self::KEYS {
            s.push_str(pref);
            s.push('`');
            s.push_str(key);
            s.push('`');
            pref = ", ";
        }
        s
    }

    /// Constructor.
    ///
    /// Requires mutable access to the expression to remove fast_expr-specific attributes.
    pub fn new(conf: &Conf, expr: &mut rust::Enum) -> Res<Self> {
        // Build the default expr-conf induced by `conf`.
        let mut slf = {
            let zip_gen = conf.top_expr.ref_map(|top_opt| {
                top_opt
                    .as_ref()
                    .map(|top| &expr.ident == top)
                    .unwrap_or(true)
            });

            Self { zip_gen }
        };

        // Modify `slf` based on the attributes.
        let attributes = std::mem::replace(&mut expr.attrs, vec![]);

        for attr in attributes {
            let front_conf_opt: Result<front::Conf, _> = syn::parse(attr.tokens.clone().into());

            if let Ok(front_conf) = front_conf_opt {
                slf.handle_front_conf(front_conf)?
            } else {
                expr.attrs.push(attr)
            }
        }

        Ok(slf)
    }

    /// Applies the changes specified by `front_conf`.
    fn handle_front_conf(&mut self, front_conf: front::Conf) -> Res<()> {
        for field in front_conf.fields {
            self.handle_front_conf_field(field)?
        }
        Ok(())
    }

    /// Applies the change specified by `field`.
    fn handle_front_conf_field(&mut self, field: front::ConfField) -> Res<()> {
        if field.id == Self::KEY_ZIP_GEN {
            let (id_span, b) = field.into_bool()?;
            self.zip_gen.set(b, id_span)
        } else {
            bail!(on(
                field.id,
                "unknown configuration identifier, expected one of {}",
                Self::key_list()
            ))
        }

        Ok(())
    }
}

/// Top-level fast_expr configuration.
#[derive(Debug, Clone)]
pub struct Conf {
    /// Name of the `fast_expr` crate.
    pub fast_expr_name: Val<rust::Id>,

    /// Name of the single expression type to consider as top.
    ///
    /// This deactivates `zip_gen` for all expression types but this one. Note that `EConf` can
    /// override this.
    pub top_expr: Val<Option<rust::Id>>,

    /// Forces to reveal all types and functions that would normally be secret.
    pub all_pub: Val<bool>,

    /// Controls whether we should generate the reference-zipper(s).
    pub ref_gen: Val<bool>,
    /// Controls whether we should generate the owned-zipper(s).
    pub own_gen: Val<bool>,
}
impl Conf {
    /// Key for the `fast_expr_name` field.
    const KEY_FAST_EXPR_NAME: &'static str = "name";
    /// Key for the `top_expr` field.
    const KEY_TOP_EXPR: &'static str = "top";
    /// Key for the `all_pub` field.
    const KEY_ALL_PUB: &'static str = "all_pub";
    /// Key for the `ref_gen` field.
    const KEY_REF_GEN: &'static str = "ref_gen";
    /// Key for the `own_gen` field.
    const KEY_OWN_GEN: &'static str = "own_gen";
    /// All keys accepted by `Self`.
    const KEYS: [&'static str; 5] = [
        Self::KEY_FAST_EXPR_NAME,
        Self::KEY_TOP_EXPR,
        Self::KEY_ALL_PUB,
        Self::KEY_REF_GEN,
        Self::KEY_OWN_GEN,
    ];
    /// String containing a comma-separated enumeration of the legal keys for `Self`.
    fn key_list() -> String {
        let mut s = String::new();
        let mut pref = "";
        for key in &Self::KEYS {
            s.push_str(pref);
            s.push('`');
            s.push_str(key);
            s.push('`');
            pref = ", ";
        }
        s
    }

    /// Checks whether the configuration is legal.
    pub fn check(&self, exprs: &expr::Exprs) -> Res<()> {
        // If user specified a top expression, it must exist.
        if let Some(top) = self.top_expr.as_ref() {
            let top_id = top.deref();
            if exprs.iter().all(|expr| expr.id() != top_id) {
                bail!(on(top, "unknown expression type `{}`", top_id))
            }
        }
        Ok(())
    }

    /// Constructor.
    ///
    /// # Panics
    ///
    /// if one or more of the attributes
    ///
    /// - is not an inner attribute,
    /// - is not a legal general config attribute.
    pub fn new(attrs: Vec<rust::Attribute>) -> Res<Self> {
        assert!(attrs.iter().all(|attr| match attr.style {
            syn::AttrStyle::Inner(_) => true,
            syn::AttrStyle::Outer => false,
        }));

        // Build the default expr-conf induced by `conf`.
        let mut slf = Self::default();

        let fast_expr_attr_key = front::Conf::fast_expr_attr_key();

        for attr in attrs {
            if attr.path != fast_expr_attr_key {
                bail!(on(
                    &attr.path,
                    "expected a fast_expr configuration attribute `fast_expr(...)`"
                ))
            }
            let front_conf = syn::parse(attr.tokens.clone().into())?;
            slf.handle_front_conf(front_conf)?
        }

        Ok(slf)
    }

    /// Visibility of the codegen items supposed to be secret.
    pub fn secret_item_vis(&self) -> rust::Visibility {
        if *self.all_pub {
            let span = self
                .all_pub
                .from_user_span
                .clone()
                .unwrap_or_else(gen::span);
            let pub_token = rust::token::Pub { span };
            syn::parse_quote!(#pub_token)
        } else {
            rust::Visibility::Inherited
        }
    }

    /// Applies the changes specified in `front_conf`.
    fn handle_front_conf(&mut self, front_conf: front::Conf) -> Res<()> {
        for field in front_conf.fields {
            self.handle_front_conf_field(field)?
        }
        Ok(())
    }
    /// Applies th echange specified in `field`.
    fn handle_front_conf_field(&mut self, field: front::ConfField) -> Res<()> {
        if field.id == Self::KEY_FAST_EXPR_NAME {
            let (id_span, path_id) = field.into_id()?;
            self.fast_expr_name.set(path_id, id_span)
        } else if field.id == Self::KEY_TOP_EXPR {
            let (id_span, top_id) = field.into_id()?;
            self.top_expr.set(top_id, id_span)
        } else if field.id == Self::KEY_ALL_PUB {
            let (id_span, top_id) = field.into_bool()?;
            self.all_pub.set(top_id, id_span)
        } else if field.id == Self::KEY_REF_GEN {
            let (id_span, b) = field.into_bool()?;
            self.ref_gen.set(b, id_span)
        } else if field.id == Self::KEY_OWN_GEN {
            let (id_span, b) = field.into_bool()?;
            self.own_gen.set(b, id_span)
        } else {
            bail!(on(
                field.id,
                "unknown configuration identifier, expected one of {}",
                Self::key_list()
            ))
        }

        Ok(())
    }
}
impl Default for Conf {
    fn default() -> Self {
        Self {
            fast_expr_name: Val::new_default(rust::Id::new("fast_expr", gen::span())),

            top_expr: Val::new_default(None),

            all_pub: Val::new_default(false),

            ref_gen: Val::new_default(true),
            own_gen: Val::new_default(true),
        }
    }
}
