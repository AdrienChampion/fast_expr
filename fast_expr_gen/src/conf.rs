//! Fast_expr configuration.

prelude! {}

/// A configuration value, which tracks if it's a default or provided by the user.
#[derive(Debug, Clone)]
pub struct Val<T> {
    val: T,
    from_user_span: Option<rust::Span>,
}
impl<T> Val<T> {
    pub fn new_default(val: impl Into<T>) -> Self {
        Self {
            val: val.into(),
            from_user_span: None,
        }
    }
    pub fn set(&mut self, val: impl Into<T>, src: rust::Span) {
        self.val = val.into();
        self.from_user_span = Some(src);
    }

    pub fn is_from_user(&self) -> bool {
        self.from_user_span.is_some()
    }
    pub fn from_user_span(&self) -> Option<rust::Span> {
        self.from_user_span.clone()
    }

    pub fn map<Out>(&self, action: impl FnOnce(&T) -> Out) -> Val<Out> {
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
    pub is_top_expr: Val<bool>,

    pub ref_gen: Val<bool>,
    pub own_gen: Val<bool>,
}

impl EConf {
    const KEY_IS_TOP_EXPR: &'static str = "top";
    const KEY_REF_GEN: &'static str = "ref_gen";
    const KEY_OWN_GEN: &'static str = "own_gen";
    const KEYS: [&'static str; 3] = [Self::KEY_IS_TOP_EXPR, Self::KEY_REF_GEN, Self::KEY_OWN_GEN];
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
            let is_top_expr = conf.top_expr.map(|top_opt| {
                top_opt
                    .as_ref()
                    .map(|top| &expr.ident == top)
                    .unwrap_or(false)
            });
            let (ref_gen, own_gen) = (conf.ref_gen.clone(), conf.own_gen.clone());

            Self {
                is_top_expr,

                ref_gen,
                own_gen,
            }
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

    fn handle_front_conf(&mut self, front_conf: front::Conf) -> Res<()> {
        for field in front_conf.fields {
            self.handle_front_conf_field(field)?
        }
        Ok(())
    }
    fn handle_front_conf_field(&mut self, field: front::ConfField) -> Res<()> {
        if field.id == Self::KEY_IS_TOP_EXPR {
            let (id_span, bool_opt) = field.into_bool_opt()?;
            self.is_top_expr.set(bool_opt.unwrap_or(true), id_span)
        } else if field.id == Self::KEY_REF_GEN {
            let (id_span, bool_opt) = field.into_bool_opt()?;
            self.ref_gen.set(bool_opt.unwrap_or(true), id_span)
        } else if field.id == Self::KEY_OWN_GEN {
            let (id_span, bool_opt) = field.into_bool_opt()?;
            self.ref_gen.set(bool_opt.unwrap_or(true), id_span)
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
    pub fast_expr_name: Val<rust::Id>,

    pub top_expr: Val<Option<rust::Id>>,

    pub ref_gen: Val<bool>,
    pub own_gen: Val<bool>,
}
impl Conf {
    const KEY_FAST_EXPR_NAME: &'static str = "fast_expr_name";
    const KEY_TOP_EXPR: &'static str = "top";
    const KEY_REF_GEN: &'static str = "ref_gen";
    const KEY_OWN_GEN: &'static str = "own_gen";
    const KEYS: [&'static str; 4] = [
        Self::KEY_FAST_EXPR_NAME,
        Self::KEY_TOP_EXPR,
        Self::KEY_REF_GEN,
        Self::KEY_OWN_GEN,
    ];
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

        for attr in attrs {
            let front_conf = syn::parse(attr.tokens.clone().into())?;
            slf.handle_front_conf(front_conf)?
        }

        Ok(slf)
    }

    fn handle_front_conf(&mut self, front_conf: front::Conf) -> Res<()> {
        for field in front_conf.fields {
            self.handle_front_conf_field(field)?
        }
        Ok(())
    }
    fn handle_front_conf_field(&mut self, field: front::ConfField) -> Res<()> {
        if field.id == Self::KEY_FAST_EXPR_NAME {
            let (id_span, path_id) = field.into_id()?;
            self.fast_expr_name.set(path_id, id_span)
        } else if field.id == Self::KEY_TOP_EXPR {
            let (id_span, top_id) = field.into_id()?;
            self.top_expr.set(top_id, id_span)
        } else if field.id == Self::KEY_REF_GEN {
            let (id_span, bool_opt) = field.into_bool_opt()?;
            self.ref_gen.set(bool_opt.unwrap_or(true), id_span)
        } else if field.id == Self::KEY_OWN_GEN {
            let (id_span, bool_opt) = field.into_bool_opt()?;
            self.ref_gen.set(bool_opt.unwrap_or(true), id_span)
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

            ref_gen: Val::new_default(true),
            own_gen: Val::new_default(true),
        }
    }
}
