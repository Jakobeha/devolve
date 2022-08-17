/// Like a limited form of [matches],
/// but returns [Some] with with a tuple of identifiers, or [None] if didn't match.
pub macro extract{
    ($expression:expr, $pat_path1:ident :: $pat_path2:ident $( ( $( $pat_expr:expr ),* $(,)? ) )? $( if $guard:expr )? $(,)?) => {
        match $expression {
            $pat_path1 :: $pat_path2 $( ( $( $pat_expr ),* ) )? $( if $guard )? => Some($( __extract_return!($( $pat_expr ),*) )?),
            _ => None
        }
    },
    ($expression:expr, $pat_path1_1:ident :: $pat_path2_1:ident $( ( $( $pat_expr_1:expr ),* $(,)? ) )? | $pat_path1_2:ident :: $pat_path2_2:ident $( ( $( $pat_expr_2:expr ),* $(,)? ) )? $( if $guard:expr )? $(,)?) => {
        match $expression {
            $pat_path1_1 :: $pat_path2_1 $( ( $( $pat_expr_1 ),* ) )? $( if $guard )? => Some($( __extract_return!($( $pat_expr_1 ),*) )?),
            $pat_path1_2 :: $pat_path2_2 $( ( $( $pat_expr_2 ),* ) )? $( if $guard )? => Some($( __extract_return!($( $pat_expr_2 ),*) )?),
            _ => None
        }
    }
}

#[doc(hidden)]
macro __extract_return {
    () => { () },
    ($first:ident) => { ($first, ) },
    ($first:expr) => { () },
    ($first:ident, $( $rest:expr ),*) => { ($first, $crate::misc::extract::__extract_return!($($rest),*)) },
    ($first:expr, $( $rest:expr ),*) => { $crate::misc::extract::__extract_return!($($rest),*) }
}