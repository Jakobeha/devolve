/// Like a limited form of [matches],
/// but returns [Some] with with a tuple of identifiers, or [None] if didn't match.
pub macro extract {
    ($expression:expr, $pat_path1:ident :: $pat_path2:ident $( ( $( $pat_expr:tt ),* $(,)? ) )? $( if $guard:expr )? $(,)?) => {
        match $expression {
            $pat_path1 :: $pat_path2 $( ( $( $pat_expr ),* ) )? $( if $guard )? => Some(__extract_return!($( $( $pat_expr ),* )?)),
            _ => None
        }
    },
    ($expression:expr, $pat_path1_1:ident :: $pat_path2_1:ident $( ( $( $pat_expr_1:expr ),* $(,)? ) )? | $pat_path1_2:ident :: $pat_path2_2:ident $( ( $( $pat_expr_2:expr ),* $(,)? ) )? $( if $guard:expr )? $(,)?) => {
        match $expression {
            $pat_path1_1 :: $pat_path2_1 $( ( $( $pat_expr_1 ),* ) )? $( if $guard )? => Some(__extract_return!($( $( $pat_expr_1 ),* )?)),
            $pat_path1_2 :: $pat_path2_2 $( ( $( $pat_expr_2 ),* ) )? $( if $guard )? => Some(__extract_return!($( $( $pat_expr_2 ),* )?)),
            _ => None
        }
    }
}

#[doc(hidden)]
macro __extract_return {
    ($( $lit0:literal ),*) => { () },
    ($( $lit0:literal, )* $id0:ident $(, $lit1:literal )*) => { $id0 },
    ($( $lit0:literal, )* $id0:ident, $( $lit1:literal, )* $id1:ident $(, $lit2:literal )*) => { ($id0, $id1) },
    ($( $lit0:literal, )* $id0:ident, $( $lit1:literal, )* $id1:ident, $( $lit2:literal, )* $id2:ident $(, $lit3:literal )*) => { ($id0, $id1, $id2) },
    ($( $lit0:literal, )* $id0:ident, $( $lit1:literal, )* $id1:ident, $( $lit2:literal, )* $id2:ident, $( $lit3:literal, )* $id3:ident $(, $lit4:literal)*) => { ($id0, $id1, $id2, $id3) },
    ($( $lit0:literal, )* $id0:ident, $( $lit1:literal, )* $id1:ident, $( $lit2:literal, )* $id2:ident, $( $lit3:literal, )* $id3:ident, $( $lit4:literal, )* $id4:ident $(, $lit5:literal )*) => { ($id0, $id1, $id2, $id3, $id4) }
}