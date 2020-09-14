//! Handles documentation generation.

prelude! {}

macro_rules! doc {
    (@($is_top:tt, $s:expr) newline $($tail:tt)*) => {{
        $s.push_str("\n");
        doc!(@($is_top, $s) $($tail)*)
    }};

    (@($is_top:tt, $s:expr) if ($cnd:expr) {$($thn:tt)*} $(else {$($els:tt)*})? $($tail:tt)* ) => {{
        if $cnd {
            doc!(@(false, $s) $($thn)*)
        } $(else {
            doc!(@(false, $s) $($els)*)
        })?
        doc!(@($is_top, $s) $($tail)*)
    }};
    (@($is_top:tt, $s:expr)
        if (let $pat:pat = $cnd:expr) {$($thn:tt)*} $(else {$($els:tt)*})? $($tail:tt)*
    ) => {{
        if let $pat = $cnd {
            doc!(@(false, $s) $($thn)*)
        } $(else {
            doc!(@(false, $s) $($els)*)
        })?
        doc!(@($is_top, $s) $($tail)*)
    }};
    (@($is_top:tt, $s:expr) for ($for_pat:pat in $iter:expr) { $($body:tt)* } $($tail:tt)* ) => {{
        for $for_pat in $iter {
            doc!(@(false, $s) $($body)*)
        }
        doc!(@($is_top, $s) $($tail)*)
    }};

    (@($is_top:tt, $s:expr) $str:expr ; $($tail:tt)* ) => {{
        $s.push_str($str);
        $s.push_str("\n");
        doc!(@($is_top, $s) $($tail)*)
    }};
    (@($is_top:tt, $s:expr) $($args:expr),* ; $($tail:tt)* ) => {{
        $s.push_str(&format!($($args),*));
        $s.push_str("\n");
        doc!(@($is_top, $s) $($tail)*)
    }};
    (@($is_top:tt, $s:expr) ; $($tail:tt)*) => (doc!(@($is_top, $s), $($tail)*));
    (@(true, $s:expr)) => (into_doc($s));
    (@(false, $s:expr)) => (());

    (@($s:expr) $($stuff:tt)*) => {
        compile_error!(
            concat!("unexpected tokens: " $(, stringify!($stuff))*)
        )
    };

    ($($stuff:tt)*) => {{
        let mut s = String::new();
        doc!(@(true, s) $($stuff)*)
    }};
}

fn into_doc(doc: String) -> TokenStream {
    quote!(#[doc = #doc])
}

pub mod zip_mod {
    use super::*;

    pub fn doc(cxt: &cxt::ZipCxt, is_own: IsOwn) -> TokenStream {
        doc! {
            "Zipper(s) over {} expressions.", if is_own { "owned" } else { "borrowed" };
            newline

            "The first step to build your own zipper is to implement the `...{}` for some type. \
            Typically, this type is a `struct` containing whatever state your zipper needs to \
            maintain. For an evaluator-like zipper, that would be the *model* mapping variables to \
            values.",
            gen::trai::ZIPPER_SUFF;
            "These traits have one or more `...{}` associated type(s). They correspond to the \
            kind of result you want your zipper to produce.",
            gen::typ::RES_SUFF;
            newline

            "Implementing such a trait *basically* amounts to specifying how to produce a `...{}` \
            result for each variant of your expression type(s).",
            gen::typ::RES_SUFF;
            "This is what all the `{}...` abstract functions of a `...{}` trait represent.",
            gen::fun::GO_UP_PREF, gen::trai::ZIPPER_SUFF;
            "There's one such function for each variant of your expression type(s). Each of them \
            takes arguments named after the fields of the corresponding variant. *Leaf* fields \
            (fields that do not mention an expression type) are passed as {}, while *recursive \
            fields* are first zipped over, thus yielding a result, which is then passed to the \
            `{}...` function.",
            if is_own { "is" } else { "references" }, gen::fun::GO_UP_PREF;
            newline

            "# Expression Types";
            newline

            for (e_cxt in cxt.e_cxts()) {
                if (*e_cxt.e_conf().zip_gen) {
                    "- [`{}`]: see [`{}`] and [`{}`].",
                    e_cxt.e_id(), e_cxt.zipper_trait().id(), e_cxt.zip_struct().id();
                }
            }
            newline

            for (e_cxt in cxt.e_cxts()) {
                if (*e_cxt.e_conf().zip_gen) {
                    "[`{0}`]: ../struct.{0}.html ({0} enum)", e_cxt.e_id();
                    "[`{0}`]: trait.{0}.html ({0} trait)", e_cxt.zipper_trait().id();
                    "[`{0}`]: struct.{0}.html ({0} struct)", e_cxt.zip_struct().id();
                }
            }
        }
    }
}

pub mod zip_struct {
    use super::*;

    pub fn doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let e_id = e_cxt.e_id();
        let trait_id = e_cxt.zipper_trait().id();
        doc! {
            "Zipper structure for [`{}`].", e_id;
            newline

            "The main zip functions is [`{}`].", e_cxt.self_ids().zip_fun;
            if (e_cxt.fp_e_deps().len() > 1) {
                "Note that [`{}`] (recursively) depends on the following expression type(s):", e_id;
                for (dep_e_idx in
                    e_cxt.fp_e_deps().iter().cloned().filter(|dep_e_idx| *dep_e_idx != e_idx)
                ) {
                    "- [`{}`], zipped by [`{}`]",
                    cxt[dep_e_idx].e_id(),
                    cxt[dep_e_idx].self_ids().zip_fun;
                }
            }
            newline

            "This type provides all `zip_...` functions and is in charge of maintaining the \
            stack(s) of frames used when zipping over expressions. On creation, requires something \
            implementing the [`{0}`] trait.",
            trait_id;
            newline

            for (dep_e_idx in e_cxt.fp_e_deps().iter().cloned()) {
                "[`{0}`]: ../enum.{0}.html ({0} enum)", cxt[dep_e_idx].e_id();
                "[`{0}`]: #method.{0} ({0} zip function for {1})",
                cxt[dep_e_idx].self_ids().zip_fun,
                cxt[dep_e_idx].e_id();
            }
            "[`{0}`]: trait.{0}.html ({0} trait)", trait_id;
        }
    }

    fn constructor_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, capa: bool) -> TokenStream {
        doc! {
            "Constructor{}, takes a zipper spec' that implements [`{}`].",
            if capa { " with an initial stack capacity" } else { "" },
            cxt[e_idx].zipper_trait().id();

            newline
            "[`{0}`]: trait.{0}.html ({0} trait)", cxt[e_idx].zipper_trait().id();
        }
    }

    pub fn new_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr) -> TokenStream {
        constructor_doc(cxt, e_idx, false)
    }
    pub fn with_capacity_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr) -> TokenStream {
        constructor_doc(cxt, e_idx, true)
    }

    pub fn zip_fun(cxt: &cxt::ZipCxt, e_idx: idx::Expr) -> TokenStream {
        let e_id = cxt[e_idx].e_id();
        doc! {
            "Zips over [`{}`] expressions.", e_id;

            newline
            "[`{0}`]: ../enum.{0}.html ({0} enum)", e_id;
        }
    }
}

pub mod zipper_trait {
    use super::*;

    pub fn doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let e_id = e_cxt.e_id();
        let zip_id = e_cxt.zip_struct().id();
        doc! {
            "Zipper specification for [`{}`].", e_id;
            newline

            "This trait specifies an operation over [`{}`] expressions by describing what to do \
            with its leaves and sub-expressions (if any).", e_id;
            if (e_cxt.fp_e_deps().len() > 1) {
                "Note that [`{}`] (recursively) depends on the following expression type(s):", e_id;
                for (dep_e_idx in
                    e_cxt.fp_e_deps().iter().cloned().filter(|dep_e_idx| *dep_e_idx != e_idx)
                ) {
                    "- [`{}`]", cxt[dep_e_idx].e_id();
                }
                newline

                "Hence, how to handle these expressions is also part of this zipper specification \
                trait.";
            }
            newline

            "Values of a type implementing this trait can be used to construct a [`{0}`], which \
            performs the actual operation using [`{0}::{1}`]",
            zip_id, e_cxt.self_ids().zip_fun;
            newline

            for (dep_e_idx in e_cxt.fp_e_deps().iter().cloned()) {
                "[`{0}`]: ../enum.{0}.html ({0} enum)", cxt[dep_e_idx].e_id();
                "[`{0}`]: #method.{0} ({0} zip function for {1})",
                cxt[dep_e_idx].self_ids().zip_fun,
                cxt[dep_e_idx].e_id();
            }
            "[`{0}`]: struct.{0}.html ({0} struct)", zip_id;
            "[`{0}::{1}`]: struct.{0}.html#method.{1} ({1} method on {0})",
            zip_id,
            e_cxt.self_ids().zip_fun;
        }
    }

    pub fn res_typ_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, for_expr: idx::Expr) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let e_id = e_cxt.e_id();
        let zip_id = cxt[for_expr].zip_struct().id();

        doc! {
            "Type of the result of the operation over [`{}`] expressions.", e_id;
            newline

            "Most notably, if this is what [`{}::{}`] produces.",
            zip_id, e_cxt.self_ids().zip_fun;
            newline

            "[`{0}`]: ../enum.{0}.html ({0} enum)", e_id;
            "[`{0}::{1}`]: struct.{0}.html#method.{1} ({1} method on {0})",
            zip_id, e_cxt.self_ids().zip_fun;
        }
    }

    pub fn init_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, c_idx: idx::Coll) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let c_cxt = &e_cxt.colls()[c_idx];
        let variant = &e_cxt.expr()[c_cxt.v_idx()];
        let data = &variant[c_cxt.d_idx()];

        let (data_name, data_link) = if let Some(id) = data.d_id() {
            (
                format!("[`{}`]", id),
                Some(format!(
                    "[`{2}`]: ../enum.{0}.html#variant.{1}.field.{2}",
                    e_cxt.e_id(),
                    variant.v_id(),
                    id
                )),
            )
        } else {
            (format!("field number {}", data.d_idx()), None)
        };
        let fold_id = e_cxt.coll_handlers()[c_idx].folder().id();

        doc! {
            "Initializes the accumulator used to fold over [`{}::{}`]'s {}.",
            e_cxt.e_id(), variant.v_id(), data_name;
            newline

            "This initializer is called whenever the zipper reaches a [`{}::{}`]'s {} in its \
            traversal of an expression.",
            e_cxt.e_id(), variant.v_id(), data_name;
            "This sets up the accumulator so that it can be used by [`{}`], which performs the \
            actual folding steps.",
            fold_id;
            newline

            "[`{0}::{1}`]: ../enum.{0}.html#variant.{1} ({0}'s {1} variant)",
            e_cxt.e_id(), variant.v_id();
            "[`{0}`]: #tymethod.{0} (method {0})", fold_id;
            if (let Some(data_link) = data_link) {
                &data_link;
            }
        }
    }

    pub fn fold_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, c_idx: idx::Coll) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let c_cxt = &e_cxt.colls()[c_idx];
        let variant = &e_cxt.expr()[c_cxt.v_idx()];
        let data = &variant[c_cxt.d_idx()];

        let (data_name, data_link) = if let Some(id) = data.d_id() {
            (
                format!("[`{}`]", id),
                Some(format!(
                    "[`{2}`]: ../enum.{0}.html#variant.{1}.field.{2}",
                    e_cxt.e_id(),
                    variant.v_id(),
                    id
                )),
            )
        } else {
            (format!("field number {}", data.d_idx()), None)
        };
        let init_id = e_cxt.coll_handlers()[c_idx].initializer().id();

        doc! {
            "Folding step over elements of a [`{}::{}`]'s {}.",
            e_cxt.e_id(), variant.v_id(), data_name;
            newline

            "This fold-step is called when the zipper just computed a result for the *next \
            element* of a [`{}::{}`]'s {}. The arguments include the current value of the \
            accumulator, and the result for the next element.",
            e_cxt.e_id(), variant.v_id(), data_name;
            newline

            "See also [`{}`], which takes care of initializing the accumulator prior to the \
            folding process.",
            init_id;
            newline

            "[`{0}::{1}`]: ../enum.{0}.html#variant.{1} ({0}'s {1} variant)",
            e_cxt.e_id(), variant.v_id();
            "[`{0}`]: #tymethod.{0} (method {0})", init_id;
            if (let Some(data_link) = data_link) {
                &data_link;
            }
        }
    }

    pub fn assoc_typ_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, c_idx: idx::Coll) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let c_cxt = &e_cxt.colls()[c_idx];
        let variant = &e_cxt.expr()[c_cxt.v_idx()];
        let data = &variant[c_cxt.d_idx()];

        let (data_name, data_link) = if let Some(id) = data.d_id() {
            (
                format!("[`{}`]", id),
                Some(format!(
                    "[`{2}`]: ../enum.{0}.html#variant.{1}.field.{2}",
                    e_cxt.e_id(),
                    variant.v_id(),
                    id
                )),
            )
        } else {
            (format!("field number {}", data.d_idx()), None)
        };

        let (init_id, fold_id) = (
            e_cxt.coll_handlers()[c_idx].initializer().id(),
            e_cxt.coll_handlers()[c_idx].folder().id(),
        );

        doc! {
            "Type of the accumulator used to fold over [`{}::{}`]'s {}.",
            e_cxt.e_id(), variant.v_id(), data_name;
            newline

            "There are two methods tied to this accumulator type:";
            "- [`{}`] produces the initial value of the accumulator, and",
            init_id;
            "- [`{}`] produces the new value of the accumulator given the old one and the result \
            for the next element of the collection.",
            fold_id;
            newline

            "[`{0}::{1}`]: ../enum.{0}.html#variant.{1} ({0}'s {1} variant)",
            e_cxt.e_id(), variant.v_id();
            "[`{0}`]: #tymethod.{0} (method {0})", init_id;
            "[`{0}`]: #tymethod.{0} (method {0})", fold_id;
            if (let Some(data_link) = data_link) {
                &data_link;
            }
        }
    }

    pub fn go_up_doc(cxt: &cxt::ZipCxt, e_idx: idx::Expr, v_idx: idx::Variant) -> TokenStream {
        let e_cxt = &cxt[e_idx];
        let variant = &e_cxt.expr()[v_idx];

        doc! {
            "Describes how to produce a result from the data stored in a [`{}::{}`]{}.",
            e_cxt.e_id(), variant.v_id(), if variant.is_leaf() {
                ""
            } else {
                " and its sub-expression(s) zip results"
            };
            newline

            "[`{0}::{1}`]: ../enum.{0}.html#variant.{1} ({0}'s {1} variant)",
            e_cxt.e_id(), variant.v_id();
        }
    }
}
