use super::*;

pub mod zip_struct;
pub mod zipper_trait;

use self::{
    zip_struct::ZipStruct,
    zipper_trait::{CollHandlers, Inspecter, VariantHandlers, ZipperTrait},
};

pub type Infos = idx::ExprMap<Info>;
pub struct Info {
    zip_struct: ZipStruct,
    zipper_trait: ZipperTrait,
    variant_handlers: VariantHandlers,
    coll_handlers: CollHandlers,
    inspecter: Inspecter,
}
impl Info {
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

#[derive(Debug, Clone)]
pub struct ECxt {
    cxt: cxt::frame::ECxt,

    zip_struct: ZipStruct,
    zipper_trait: ZipperTrait,

    variant_handlers: VariantHandlers,
    coll_handlers: CollHandlers,
    inspecter: Inspecter,
}
implement! {
    impl ECxt {
        Deref<cxt::frame::ECxt>, DerefMut {
            field: cxt
        }
    }
}

impl ECxt {
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
        Self {
            cxt,
            zip_struct,
            zipper_trait,

            variant_handlers,
            coll_handlers,
            inspecter,
        }
    }

    pub fn zip_struct(&self) -> &ZipStruct {
        &self.zip_struct
    }
    pub fn zipper_trait(&self) -> &ZipperTrait {
        &self.zipper_trait
    }

    pub fn variant_handlers(&self) -> &VariantHandlers {
        &self.variant_handlers
    }
    pub fn coll_handlers(&self) -> &CollHandlers {
        &self.coll_handlers
    }
    pub fn inspecter(&self) -> &Inspecter {
        &self.inspecter
    }
}

impl ECxt {
    pub fn zip_mod_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let mut tokens = self.frame_enum_tokens(cxt, is_own);
        if *self.e_conf().zip_gen {
            tokens.extend(self.zip_trait_tokens(cxt, is_own));
            tokens.extend(self.zip_struct_tokens(cxt, is_own));
        }
        tokens
    }

    pub fn zip_struct_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zip_struct.to_tokens(cxt, is_own)
    }

    /// Generates the tokens for the trait definition for this expression type.
    pub fn zip_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        self.zipper_trait.to_tokens(cxt, is_own)
    }

    /// Generates the zipper trait items for this expression type.
    ///
    /// Used when generating a zipper trait for an expression type that mentions this expression
    /// type.
    pub fn self_zip_trait_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let variants = self.variant_handlers.to_zipper_trait_tokens(cxt, is_own);
        let colls = self.coll_handlers.to_zipper_trait_tokens(cxt, is_own);
        let inspecter = self.inspecter.to_zipper_trait_tokens(cxt, is_own);
        quote! {
            #variants
            #colls
            #inspecter
        }
    }
}

impl ECxt {
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

    pub fn to_zip_handler_fn_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
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
    pub fn self_zip_fun_def_tokens(&self, cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        let zip_fun_id = &self.self_ids().zip_fun;

        let expr = &cxt.zip_ids().expr_var;
        let e_typ = self.plain_typ_for(is_own);

        let out_typ = self.res_typ();

        let body = self.zip_fun_def_body_tokens(cxt);

        quote! {
            pub fn #zip_fun_id(
                &mut self,
                // Input expression is mutable, because it represents the current expression. Meaning it will be changed as we go down the expression.
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
    fn zip_fun_drain_stack(&self, cxt: &cxt::ZipCxt, stack_field: &rust::Id) -> TokenStream {
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
                let down_and_then = cxt.lib_gen().zip_do_down_and_then();
                let handle_expr = &self.self_ids().handle_expr_fun;

                quote! {
                    let #zip_do = self.#step_field.#inspect(#expr).#down_and_then(
                        |expr| self.#handle_expr(expr)
                    );
                }
            };

            let res = &cxt.zip_ids().res_var;

            // Here we handle the command, look at what it is (down, up, subst, early) and act
            // accordingly.
            //
            // Basically, either we're given a frame and an expression, in which case we continue
            // `go_up_label`, or we're given a result and it's time to go up. "Going up" here means we
            // don't continue `go_up_label`, the go-up-loop is just after this.
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

    fn zip_fun_def_go_up_tokens(
        &self,
        cxt: &cxt::ZipCxt,
        stack_field: &rust::Id,
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
