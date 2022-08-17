/// Instead of propogating this error, just log it as an error.
/// Useful for non-essential failures like the file watcher
pub macro catch_and_log($e:expr, $msg:literal $(, $args:expr)*) {
    match $e {
        Ok(v) => Some(v),
        Err(e) => {
            log::error!(concat!($msg, ": {}") $(, $args)*, e);
            None
        }
    }
}

/// Instead of propogating this error, just log it as a warning.
/// Useful for non-essential failures like the file watcher
pub macro catch_and_warn($e:expr, $msg:literal $(, $args:expr)*) {
match $e {
        Ok(v) => Some(v),
        Err(e) => {
            log::warn!(concat!($msg, ": {}") $(, $args)*, e);
            None
        }
    }
}