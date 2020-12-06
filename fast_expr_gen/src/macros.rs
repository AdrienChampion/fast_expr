//! Macros.

/// Imports the fast expr prelude.
#[macro_export]
macro_rules! prelude {
    () => {
        use $crate::prelude::*;
    };
}

/// Builds an error.
#[macro_export]
macro_rules! error {
    (on($tokens:expr, $($msg:tt)*)) => {
        crate::err::Error::new_spanned($tokens, format_args!($($msg)*))
    };
    (@($span:expr, $($msg:tt)*)) => {
        crate::err::Error::new($span, format_args!($($msg)*))
    };

    ($unexpected:tt $def:tt) => {
        compile_error!(concat!("expected `for` or `@`, found `", stringify!($unexpected), "`"))
    };

    ($kind:tt $def:tt $(, $tail_kind:tt $tail_def:tt)* $(,)?) => {{
        let mut err = $crate::error!($kind $def);
        $(
            err.combine($crate::error!($tail_kind $tail_def));
        )*
        err
    }};
}
/// Returns an error.
#[macro_export]
macro_rules! bail {
    ($($stuff:tt)*) => {
        return Err($crate::error!($($stuff)*))
    };
}

/// Outputs something to `stdout` if the `log` feature is active
#[cfg(any(test, feature = "dbg_log"))]
#[macro_export]
macro_rules! log {
    ({$($stuff:tt)*}) => {
        $($stuff)*
    };
}
/// Outputs something to `stdout` if the `log` feature is active
#[cfg(not(any(test, feature = "dbg_log")))]
#[macro_export]
macro_rules! log {
    ({$($stuff:tt)*}) => {
        debug_assert!({
            let _ = {
                $($stuff)*
            };
            true
        })
    };
}

/// Outputs something to `stdout` if the `log` feature is active
#[cfg(any(test, feature = "dbg_log"))]
#[macro_export]
macro_rules! logln {
    () => {
        println!()
    };
    ($($stuff:tt)*) => {
        println!("{}", format_args!($($stuff)*))
    };
}
/// Outputs something to `stdout` if the `log` feature is active
#[cfg(not(any(test, feature = "dbg_log")))]
#[macro_export]
macro_rules! logln {
    () => {};
    ($($stuff:tt)*) => {
        debug_assert!({
            let _ = format!($($stuff)*);
            true
        })
    };
}

/// Creates a [`StaticTypPath`].
///
/// [`StaticTypPath`]: prelude/struct.StaticTypPath.html
#[macro_export]
macro_rules! static_typ_path {
    (
        $($field:ident : $val:expr),* $(,)?
    ) => {
        $crate::prelude::StaticTypPath {
            $( $field: $val ),*
        }
    }
}

/// Convenience macro for implementing various traits.
#[macro_export]
macro_rules! implement {
    () => {};
    (
        impl ($($t_params:tt)*) $slf_ty:ty {
            $($impls:tt)*
        }
        $($tail:tt)*
    ) => {
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($impls)*
        }
        $crate::implement! { $($tail)* }
    };
    (
        impl $slf_ty:ty {
            $($impls:tt)*
        }
        $($tail:tt)*
    ) => {
        $crate::implement! {
            @impl($slf_ty,)
            $($impls)*
        }
        $crate::implement! { $($tail)* }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Display $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident, $fmt:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::fmt::Display for $slf_ty
        $(where $($t_constraints)*)? {
            fn fmt(&$slf, $fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Debug $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident, $fmt:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::fmt::Debug for $slf_ty
        $(where $($t_constraints)*)? {
            fn fmt(&$slf, $fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        From<$src_ty:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$src:pat| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> From<$src_ty> for $slf_ty
        $(where $($t_constraints)*)? {
            fn from($src: $src_ty) -> Self {
                $def
            }
        }
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Into<$tgt_ty:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> Into<$tgt_ty> for $slf_ty
        $(where $($t_constraints)*)? {
            fn into($slf) -> $tgt_ty {
                $def
            }
        }
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        TryFrom<$src_ty:ty, $err_ty:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$src:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::convert::TryFrom<$src_ty> for $slf_ty
        $(where $($t_constraints)*)? {
            type Error = $err_ty;
            fn try_from($src: $src_ty) -> Res<Self> {
                $def
            }
        }
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        AsRef<Self> $(
            where ( $($t_constraints:tt)* )
        )?
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::convert::AsRef<Self> for $slf_ty
        $(where $($t_constraints)*)? {
            fn as_ref(&self) -> &Self {
                self
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        AsRef<$target:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::convert::AsRef<$target> for $slf_ty
        $(where $($t_constraints)*)? {
            fn as_ref(&$slf) -> &$target {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Deref<$target:ty>, DerefMut $(
            where ( $($t_constraints:tt)* )
        )? {
            field: $field:ident
        }
        $($tail:tt)*
    ) => {
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            Deref<$target> $(where ($($t_constraints)*))? {
                |self| &self.$field
            }
            DerefMut<$target> $(where ($($t_constraints)*))? {
                |self| &mut self.$field
            }
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Deref<$target:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::ops::Deref for $slf_ty
        $(where $($t_constraints)*)? {
            type Target = $target;
            fn deref(&$slf) -> &$target {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        DerefMut<$target:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::ops::DerefMut for $slf_ty
        $(where $($t_constraints)*)? {
            fn deref_mut(&mut $slf) -> &mut $target {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        Index<$idx_ty:ty, $out_ty:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident, $idx:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::ops::Index<$idx_ty> for $slf_ty
        $(where $($t_constraints)*)? {
            type Output = $out_ty;
            fn index(&$slf, $idx: $idx_ty) -> &$out_ty {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        IndexMut<$idx_ty:ty, $out_ty:ty> $(
            where ( $($t_constraints:tt)* )
        )? {
            |$slf:ident, $idx:ident| $def:expr
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> std::ops::IndexMut<$idx_ty> for $slf_ty
        $(where $($t_constraints)*)? {
            fn index_mut(&mut $slf, $idx: $idx_ty) -> &mut $out_ty {
                $def
            }
        }

        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
        $trait:ty $(
            where ( $($t_constraints:tt)* )
        )? {
            $($stuff:tt)*
        }
        $($tail:tt)*
    ) => {
        impl<$($t_params)*> $trait for $slf_ty
        $(where $($t_constraints)*)? {
            $($stuff)*
        }
        $crate::implement! {
            @impl($slf_ty, $($t_params)*)
            $($tail)*
        }
    };

    (
        @impl($slf_ty:ty, $($t_params:tt)*)
    ) => {};
}
