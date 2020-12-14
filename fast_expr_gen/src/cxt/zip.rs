//! Top context level.
//!
//! Augments frame contexts with the data necessary to generate the zip struct and zip spec trait
//! for each expression.

use super::*;

pub mod zip_struct;
pub mod zipper_trait;

pub use self::{
    zip_struct::ZipStruct,
    zipper_trait::{CollHandlers, Helpers, Inspecter, VariantHandlers, ZipperTrait},
};

/// A list of context building info.
pub type Infos = idx::ExprMap<Info>;
/// Information needed to construct zip expression contexts.
pub struct Info {
    zip_struct: ZipStruct,
    zipper_trait: ZipperTrait,
    variant_handlers: VariantHandlers,
    coll_handlers: CollHandlers,
    inspecter: Inspecter,
}
impl Info {
    /// Constructor.
    pub fn new(cxt: &cxt::FrameCxt, e_idx: idx::Expr) -> Self {
        let zip_struct = ZipStruct::new(cxt, e_idx);
        let zipper_trait = ZipperTrait::new(cxt, e_idx);
        let variant_handlers = VariantHandlers::from(cxt, e_idx);
        let coll_handlers = CollHandlers::from(cxt, e_idx);
        let inspecter = Inspecter::new(cxt, e_idx);
        Self {
            zip_struct,
            zipper_trait,
            variant_handlers,
            coll_handlers,
            inspecter,
        }
    }
}

/// An expression context.
#[derive(Debug, Clone)]
pub struct ECxt {
    /// Underlying frame context.
    cxt: cxt::frame::ECxt,

    /// Zip struct, in charge of actually doing the zipping.
    zip_struct: ZipStruct,
    /// Zipper spec trait, implemented by users to write zippers.
    zipper_trait: ZipperTrait,

    /// Variant handlers.
    variant_handlers: VariantHandlers,
    /// Collection handlers.
    coll_handlers: CollHandlers,
    /// Inspecter.
    inspecter: Inspecter,

    /// Helpers.
    helpers: Helpers,
}
implement! {
    impl ECxt {
        Deref<cxt::frame::ECxt>, DerefMut {
            field: cxt
        }
    }
}

impl ECxt {
    /// Constructor.
    pub fn new(
        cxt: cxt::frame::ECxt,
        Info {
            zip_struct,
            zipper_trait,
            variant_handlers,
            coll_handlers,
            inspecter,
        }: Info,
    ) -> Self {
        let helpers = Helpers::new(&cxt, &variant_handlers, &zipper_trait);
        Self {
            cxt,
            zip_struct,
            zipper_trait,

            variant_handlers,
            coll_handlers,
            inspecter,

            helpers,
        }
    }

    /// Zip struct accessor.
    pub fn zip_struct(&self) -> &ZipStruct {
        &self.zip_struct
    }
    /// Zipper spec accessor.
    pub fn zipper_trait(&self) -> &ZipperTrait {
        &self.zipper_trait
    }

    /// Variant handlers.
    pub fn variant_handlers(&self) -> &VariantHandlers {
        &self.variant_handlers
    }
    /// Collection handlers.
    pub fn coll_handlers(&self) -> &CollHandlers {
        &self.coll_handlers
    }
    /// Inspecter.
    pub fn inspecter(&self) -> &Inspecter {
        &self.inspecter
    }

    /// Helper traits.
    pub fn helpers(&self) -> &Helpers {
        &self.helpers
    }
}

impl ECxt {
    /// Codegen for a zip module.
    pub fn to_zip_mod_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let mut tokens = self.frame_enum_tokens(cxt, is_own);
        if *self.e_conf().zip_gen {
            tokens.extend(self.to_zip_trait_tokens(cxt, is_own));
            tokens.extend(self.to_zip_struct_tokens(cxt, is_own));
            tokens.extend(self.impl_macro_tokens(cxt, is_own));
        }
        tokens
    }

    /// Codegen for this expression's zip struct.
    pub fn to_zip_struct_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zip_struct.to_tokens(cxt, is_own)
    }

    /// Generates the tokens for the zipper spec trait.
    pub fn to_zip_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zipper_trait.to_tokens(cxt, is_own)
    }

    /// Generates the zipper trait items for this expression type.
    ///
    /// Used when generating a zipper trait for an expression type that mentions this expression
    /// type.
    pub fn to_self_zip_trait_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        for_expr: idx::Expr,
        is_own: IsOwn,
    ) -> TokenStream {
        let variants = self.variant_handlers.to_zipper_trait_tokens(cxt, is_own);
        let colls = self.coll_handlers.to_zipper_trait_tokens(cxt, is_own);
        let inspecter = self.inspecter.to_zipper_trait_tokens(cxt, for_expr, is_own);

        let zip_fun_id = &self.self_ids().zip_fun;
        let e_typ = self.plain_typ_for(is_own);
        let out_typ = self.res_typ();
        let zip_struct = cxt[for_expr].zip_struct().id();
        let zip_fun_doc = doc::zip_struct::zip_fun(cxt, self.e_idx());

        quote! {
            #variants
            #colls
            #inspecter

            #zip_fun_doc
            fn #zip_fun_id(&mut self, expr: #e_typ) -> Self::#out_typ
            where Self: std::marker::Sized {
                #zip_struct::new(self).#zip_fun_id(expr)
            }
        }
    }

    /// Auto implementations.
    pub fn to_self_auto_impl_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        from: &impl ToTokens,
    ) -> TokenStream {
        let variants = self.variant_handlers.to_auto_impl_tokens(cxt, is_own, from);
        let colls = self.coll_handlers.to_auto_impl_tokens(cxt, is_own, from);
        let inspecter = self.inspecter.to_auto_impl_tokens(cxt, is_own, from);

        quote! {
            #variants
            #colls
            #inspecter
        }
    }
}

impl ECxt {
    /// Output type for a handler for this expression type.
    pub fn zip_variant_handler_out_typ(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let expr = self.plain_typ_for(is_own);
        let res = self.res_typ_id();

        let down = if let Some(frames) = self.frames() {
            let frame_typ = frames.plain_typ(is_own);
            quote!( ( #frame_typ, #expr ) )
        } else {
            cxt.lib_gen().empty_instantiate()
        };
        cxt.lib_gen().zip_do_instantiate(&down, &expr, &res)
    }

    /// Codegen for this expression type's handlers.
    pub fn to_zip_handlers_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let fn_id = &self.self_ids().handle_expr_fun;
        let expr_var = &cxt.zip_ids().expr_var;
        let expr_typ = self.plain_typ_for(is_own);
        let out_typ = self.zip_variant_handler_out_typ(cxt, is_own);

        let match_cases =
            self.expr()
                .to_variant_constructors_tokens()
                .map(|(v_idx, constructor)| {
                    let variant_handler =
                        gen::fun::variant_handler(self.e_id(), self.expr()[v_idx].v_id());
                    let params = self.expr().variants()[v_idx]
                        .data()
                        .iter()
                        .map(|data| data.param_id());
                    quote! {
                        #constructor => self.#variant_handler( #(#params, )* )
                    }
                });

        let vis = cxt.conf().secret_item_vis();

        quote! {
            #vis fn #fn_id(&mut self, #expr_var: #expr_typ) -> #out_typ {
                match #expr_var {
                    #(#match_cases ,)*
                }
            }
        }
    }
}

/// # Codegen for the actual `zip` function for this expression type
impl ECxt {
    /// Codegen for the zipper specific to this expression type.
    pub fn self_zip_fun_def_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let zip_fun_id = &self.self_ids().zip_fun;

        let expr = &cxt.zip_ids().expr_var;
        let e_typ = self.plain_typ_for(is_own);

        let out_typ = self.res_typ();

        let body = self.zip_fun_def_body_tokens(cxt);

        let doc = doc::zip_struct::zip_fun(cxt, self.e_idx());

        quote! {
            #doc
            pub fn #zip_fun_id(
                &mut self,
                // Input expression is mutable, because it represents the current expression.
                // Meaning it will be changed as we go down the expression.
                #[allow(unused_mut)]
                mut #expr: #e_typ
            ) -> #out_typ {
                #body
            }
        }
    }

    /// Removes all elements from the stack that are deeper than the depth variable.
    ///
    /// Makes no sense if this expression type has no frames, so it takes the name of the
    /// expression's stack as parameter.
    fn zip_fun_drain_stack(&self, cxt: &cxt::ZipCxt, stack_field: &Ident) -> TokenStream {
        let depth = &cxt.zip_ids().depth_var;
        quote!(self.#stack_field.drain(#depth ..))
    }

    /// Generates the whole body of the zip function for this expression type.
    fn zip_fun_def_body_tokens(&self, cxt: &cxt::ZipCxt) -> TokenStream {
        if let Some(stack_field) = &self.self_ids().stack_field {
            let go_down_label = quote!('go_down);

            let depth = &cxt.zip_ids().depth_var;

            let step_field = &cxt.zip_ids().step_field;

            let go_down_loop = |go_down_body: TokenStream| {
                quote! {
                    let #depth = self.#stack_field.len();

                    #go_down_label: loop {
                        debug_assert!(#depth <= self.#stack_field.len());

                        #go_down_body
                    }
                }
            };

            let expr = &cxt.zip_ids().expr_var;
            let zip_do = &cxt.zip_ids().zip_do_var;

            // This part produces a command for the zipper, *i.e.* a `fast_expr::ZipDo`. The user's
            // stepper first inspects the expression, and then might be asked to do stuff as we handle
            // the expression variant in practice.
            let get_user_command = {
                let inspect = &&self.self_ids().inspect_fun;
                let go_down_and_then = cxt.lib_gen().zip_do_go_down_and_then();
                let handle_expr = &self.self_ids().handle_expr_fun;

                quote! {
                    let #zip_do = self.#step_field.#inspect(#expr).#go_down_and_then(
                        |expr| self.#handle_expr(expr)
                    );
                }
            };

            let res = &cxt.zip_ids().res_var;

            // Here we handle the command, look at what it is (down, up, subst, early) and act
            // accordingly.
            //
            // Basically, either we're given a frame and an expression, in which case we continue
            // `go_up_label`, or we're given a result and it's time to go up. "Going up" here means
            // we don't continue `go_up_label`, the go-up-loop is just after this.
            let produce_res = {
                let new_expr = &cxt.zip_ids().new_expr_var;
                let drain_stack = self.zip_fun_drain_stack(cxt, stack_field);

                let zip_do_cases = cxt.lib_gen().zip_do_match_cases(
                    // Down, get a new frame and an expression to go down into.
                    quote! { (frame, expr) },
                    quote! {{
                        // Update current expression.
                        #expr = expr;
                        // Remember frame.
                        self.#stack_field.push(frame);
                        // Go down in current expression.
                        continue #go_down_label;
                    }},
                    // Up, get a return, yield it.
                    quote! { res },
                    quote! { res },
                    // Subst, replace current expression and go down.
                    quote! { #new_expr },
                    quote! {{
                        #expr = #new_expr;
                        continue #go_down_label;
                    }},
                    // Early, early-return with a result.
                    quote! { res },
                    quote! {{
                        // Drain everything that was pushed since **this zip call** started.
                        #drain_stack;
                        // Return the result.
                        return res;
                    }},
                );

                quote! {
                    // Mutable result because the go-up-loop can update it as it goes up.
                    let mut #res = match #zip_do {
                        #zip_do_cases
                    };
                }
            };

            // Generate the code handling going up the result.
            let go_up = self.zip_fun_def_go_up_tokens(cxt, stack_field, &go_down_label);

            // The go-up-loop keeps going up until it finds an expression to go down into, or there is
            // no more frames to handle. If it's the latter, we're done zipping over the input
            // expression and just return the result.

            let return_result = quote! {
                debug_assert!(#depth == self.#stack_field.len());
                return #res;
            };

            go_down_loop(quote! {
                // Get a `ZipDo` from the user.
                #get_user_command
                // Handle the result, might continue `go_down_label`.
                #produce_res
                // If a result was produced, go up.
                #go_up
                // If there are no more frames, return the result.
                #return_result
            })
        } else {
            // Not much can happen here, the expression is not self-recursive. Still need a loop as
            // we can be asked to substitute stuff.
            let subst_label = quote!('subst);
            let subst_loop = |handle_expr: TokenStream| {
                quote! {
                    #subst_label: loop {
                        #handle_expr
                    }
                }
            };

            let expr = &cxt.zip_ids().expr_var;
            let new_expr = &cxt.zip_ids().new_expr_var;

            // Handle the expression, and then handle the result. Expression has no frames, the down
            // variant is inhabited.
            let zip_do_cases = cxt.lib_gen().zip_do_match_cases(
                // Down is inhabited.
                quote! { empty },
                quote! {{
                    match empty {}
                }},
                // Up, got a result, done.
                quote! { res },
                quote! {{
                    return res;
                }},
                // Subst, continue `subst_label`.
                quote! { #new_expr },
                quote! {{
                    #expr = #new_expr;
                    continue 'subst;
                }},
                // Early, same as up in this case (non-self-rec expression).
                quote! { res },
                quote! {{
                    return res;
                }},
            );

            let handle_expr = &self.self_ids().handle_expr_fun;

            subst_loop(quote! {
                match self.#handle_expr(#expr) {
                    #zip_do_cases
                }
            })
        }
    }

    /// Codegen for the `go_up` part of the zip function.
    fn zip_fun_def_go_up_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        stack_field: &Ident,
        go_down_label: impl ToTokens,
    ) -> TokenStream {
        let go_up_label = quote!('go_up);

        let depth = &cxt.zip_ids().depth_var;
        let frame = &cxt.zip_ids().frame_var;

        let go_up_loop = |handle_frame: TokenStream| {
            quote! {
                #go_up_label: loop {
                    debug_assert!(#depth <= self.#stack_field.len());

                    if #depth == self.#stack_field.len() {
                        break #go_up_label;
                    }

                    let #frame = if let Some(frame) = self.#stack_field.pop() {
                        frame
                    } else {
                        unreachable!(
                            "[fast_expr] depth on entry is {}, \
                            stack length is {}, \
                            we should have `stack length > depth on entry`, \
                            but stack is empty",
                            #depth, self.#stack_field.len(),
                        )
                    };

                    #handle_frame
                }
            }
        };

        let (expr, new_expr) = (&cxt.zip_ids().expr_var, &cxt.zip_ids().new_expr_var);
        let (res, new_res) = (&cxt.zip_ids().res_var, &cxt.zip_ids().new_res_var);
        let drain_stack = self.zip_fun_drain_stack(cxt, stack_field);

        let frame_zip_do_cases = cxt.lib_gen().zip_do_match_cases(
            // Down, get a new frame and a new expression.
            quote! { (#frame, #new_expr) },
            quote! {{
                // Update current expression.
                #expr = #new_expr;
                // Remember the frame.
                self.#stack_field.push(#frame);
                // Go down in the current expression.
                continue #go_down_label;
            }},
            // Up, get a result to propagate upwards.
            quote! { #new_res },
            quote! {{
                // Update current result.
                #res = #new_res;
                // Propagate upwards.
                continue #go_up_label;
            }},
            // Subs, get a new expression.
            quote! { #new_expr },
            quote! {{
                // Update current expression.
                #expr = #new_expr;
                // Go down in the current expression.
                continue #go_down_label;
            }},
            // Early, early-exit with a result.
            quote! { #res },
            quote! {{
                // Drain everything that was pushed since **this zip call** started.
                #drain_stack;
                // Return the result.
                return #res;
            }},
        );

        let handle_frame = &self.self_ids().handle_frame_fun;

        go_up_loop(quote! {
            match self.#handle_frame(#res, #frame) {
                #frame_zip_do_cases
            }
        })
    }
}

/// # Helper impl-Macro Codegen
impl ECxt {
    /// Generates a dummy implementation for the zip spec trait of this expression type.
    pub fn impl_macro_dummy_impl_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        expr_lt: Option<&TokenStream>,
    ) -> TokenStream {
        let expr_tokens = self.fp_e_deps().iter().cloned().map(|e_idx| {
            let e_cxt = &cxt[e_idx];
            // let expr = e_cxt.expr();

            let res_typ = e_cxt.res_typ();
            let acc_typs = e_cxt.colls().iter().map(|coll| coll.acc_t_param_id());

            let variant_handlers = e_cxt.variant_handlers().iter().map(|handler| {
                handler.custom_to_go_up_tokens(
                    cxt,
                    is_own,
                    expr_lt,
                    Some(quote! {
                        todo!()
                    }),
                )
            });

            let coll_handlers = e_cxt.coll_handlers().iter().map(|handler| {
                let init = handler.initializer().custom_to_tokens(
                    cxt,
                    is_own,
                    expr_lt,
                    Some(quote!(todo!())),
                );
                let fold =
                    handler
                        .folder()
                        .custom_to_tokens(cxt, is_own, expr_lt, Some(quote!(todo!())));
                quote! {
                    #init
                    #fold
                }
            });

            quote! {
                type #res_typ = ();
                #(
                    type #acc_typs = ();
                )*

                #(#variant_handlers)*
                #(#coll_handlers)*
            }
        });

        quote! {
            #(#expr_tokens)*
        }
    }

    /// Generates the macro-cases for this expression.
    pub fn impl_macro_cases_tokens(
        &self,
        _cxt: &cxt::ZipCxt,
        is_own: IsOwn,
        macro_ident: &rust::Ident,
    ) -> TokenStream {
        let expr = self.expr();
        let e_id = self.e_id();

        let macro_res_ty = self.res_typ();
        let lt_ident = quote! { expr_lt };
        let lt_tokens = if is_own {
            quote! {}
        } else {
            quote! { $#lt_ident }
        };
        let lt_match = if is_own {
            quote! {}
        } else {
            quote! { #lt_tokens:lifetime }
        };
        let pref_match = quote! { @(#lt_match) };
        let pref_tokens = quote! { @(#lt_tokens) };

        let variant_case_pref = quote! {
            #pref_match #e_id ($#macro_res_ty : ty)
        };

        let variant_cases = expr.variants().iter().map(|variant| {
            let v_id = variant.v_id();

            let lhs = {
                let mut lhs = quote! { #variant_case_pref #v_id };
                let data_list = variant.data().iter().map(|data| {
                    let d_id = data.param_id();
                    quote! {
                        $#d_id : pat
                    }
                });
                match variant.is_struct_like() {
                    Some(true) => lhs.extend(quote! {
                        { #(#data_list),* $(,)? }
                    }),
                    Some(false) => lhs.extend(quote! {
                        ( #(#data_list),* $(,)? )
                    }),
                    None => (),
                }

                lhs.extend(quote! {
                    => $def:expr
                    $(, $($tail:tt)* )?
                });

                lhs
            };

            let rhs = quote! {
                #macro_ident! {
                    #pref_tokens #e_id ($#macro_res_ty) $( $($tail)* )?
                }
            };

            quote! {
                { #lhs } => { #rhs };
            }
        });

        let res_typ = self.res_typ();

        quote! {
            {
                #pref_match zip(#e_id to $#macro_res_ty : ty) {
                    $($cases:tt)*
                } $(,)?
                $($tail:tt)*
            } => {
                type #res_typ = $#macro_res_ty;
                #macro_ident ! {
                    #pref_tokens #e_id ($#macro_res_ty)
                    $($cases)*
                }
                #macro_ident! {
                    #pref_tokens
                    $($tail)*
                }
            };

            { #variant_case_pref } => {};

            #(#variant_cases)*
        }
    }

    /// Generates the impl-macro for this expression.
    pub fn impl_macro_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let macro_ident = cxt.lib_gen().impl_macro_name(self.e_id(), is_own);

        let cases = self
            .fp_e_deps()
            .iter()
            .cloned()
            .map(|e_idx| cxt[e_idx].impl_macro_cases_tokens(cxt, is_own, &macro_ident));

        let legal_e_ids = self
            .fp_e_deps()
            .iter()
            .cloned()
            .enumerate()
            .map(|(idx, e_idx)| {
                if idx > 0 {
                    format!(", `{}`", cxt[e_idx].e_id())
                } else {
                    format!("`{}`", cxt[e_idx].e_id())
                }
            });

        let expr_lt = if is_own {
            None
        } else {
            Some(quote! { $ expr_lt })
        };

        let dummy_token_args = expr_lt.as_ref().map(|lt| quote! { #lt : lifetime });
        let impl_dummy_tokens = self.impl_macro_dummy_impl_tokens(cxt, is_own, expr_lt.as_ref());

        let catch_lifetime = if let Some(expr_lt) = &expr_lt {
            quote! {
                { lifetime: #expr_lt :lifetime , $($stuff:tt)* } => {
                    #macro_ident! { @(#expr_lt) $($stuff)* }
                };
            }
        } else {
            quote! {
                { $($stuff:tt)* } => {
                    #macro_ident! { @() $($stuff)* }
                };
            }
        };

        let top_level_error = "\
            a list of zip specification items of form \
            `zip(<expr_type_ident> to <expr_type_result>) { ... }`\
        ";
        let top_level_error = if is_own {
            format!("expected {}", top_level_error)
        } else {
            format!(
                "expected expression lifetime info of form `lifetime: <lifetime> ,` \
                followed by {}",
                top_level_error,
            )
        };

        quote! {
            macro_rules! #macro_ident {
                #catch_lifetime

                #(#cases)*

                { @ dummy tokens ( #dummy_token_args ) } => {
                    #impl_dummy_tokens
                };

                {
                    @ ($($args:tt)*) zip($unknown_e_id:ident $($tail_1:tt)*)
                    $($tail_2:tt)*
                } => {
                    #macro_ident! { @ dummy tokens ($($args)*) }
                    compile_error! { concat!(
                        "unexpected expression-type identifier `", stringify!($unknown_e_id),
                        "`, expected one of ",
                        #(#legal_e_ids,)*
                    ) }
                };
                { @ ($($ignore:tt)*) } => {};
                { @ () $($pebcak:tt)* } => {
                    compile_error! {
                        concat!( "1", #top_level_error)
                    }
                };
                { $($pebcak:tt)* } => {
                    compile_error! {
                        concat!( "2", #top_level_error, "   " $(, " ", stringify!($pebcak))*)
                    }
                };
            }
        }
    }
}
