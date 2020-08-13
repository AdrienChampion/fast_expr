//! Macros.

/// Outputs something to `stdout` if the `log` feature is active
#[cfg(feature = "dbg_log")]
#[macro_export]
macro_rules! log {
    ({$($stuff:tt)*}) => {
        $($stuff)*
    };
}
/// Outputs something to `stdout` if the `log` feature is active
#[cfg(not(feature = "dbg_log"))]
#[macro_export]
macro_rules! log {
    ($($stuff:tt)*) => {};
}

/// Outputs something to `stdout` if the `log` feature is active
#[cfg(feature = "dbg_log")]
#[macro_export]
macro_rules! logln {
    ($($stuff:tt)*) => {
        println!("[{}:{}] {}", file!(), line!(), format_args!($($stuff)*))
    };
}
/// Outputs something to `stdout` if the `log` feature is active
#[cfg(not(feature = "dbg_log"))]
#[macro_export]
macro_rules! logln {
    ($($stuff:tt)*) => {
        debug_assert!({
            let _ = format!($($stuff)*);
            true
        })
    };
}
