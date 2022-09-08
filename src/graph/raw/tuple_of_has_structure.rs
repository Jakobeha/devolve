use std::iter::{Chain, Empty, empty, Once, once};
use structural_reflection::{HasStructure, RustType};

pub trait TupleOfHasStructure {
    type OfOptionals;
    type IterRustTypes: Iterator<Item=RustType>;
    type IterIsSome: Iterator<Item=bool>;

    fn iter_rust_types() -> Self::IterRustTypes;
    fn iter_is_some(values: Self::OfOptionals) -> Self::IterIsSome;
}

macro_rules! __impl_tuple_of_has_structure__chain_type {
    ($ty:ty | ) => { std::iter::Empty<$ty> }
    ($ty:ty | $x:ident $(, $xs:ident)*) => { std::iter::Chain<std::iter::Once<$ty>, __impl_tuple_of_has_structure__chain_type!($ty | $($xs),*)> }
}

macro_rules! __impl_tuple_of_has_structure__chain_value {
    () => { std::iter::empty() }
    ({ $($x:tt)* } $(, { $($xs:tt)* })*) => { std::iter::once($($x)*).chain(__impl_tuple_of_has_structure__chain_value!($({ $($xs)* }),*)) }
}

macro_rules! impl_tuple_of_has_structure {
    ($($name:ident),*) => {
impl<$($name: HasStructure),*> TupleOfHasStructure for ($($name,)*) {
    type OfOptionals = ($(Option<$name>,)*);
    type IterRustTypes = __impl_tuple_of_has_structure__chain_type!(RustType | $($name),*);
    type IterIsSome = __impl_tuple_of_has_structure__chain_type!(bool | $($name),*);

    fn iter_rust_types() -> Self::IterRustTypes {
        __impl_tuple_of_has_structure__chain_value!($({ RustType::of::<$name>() }),*)
    }

    fn iter_is_some(($($name,)*): Self::OfOptionals) -> Self::IterIsSome {
        __impl_tuple_of_has_structure__chain_value!($({ $name.is_some() }),*)
    }
}
    };
}

impl_tuple_of_has_structure!();
impl_tuple_of_has_structure!(A);
impl_tuple_of_has_structure!(A, B);
impl_tuple_of_has_structure!(A, B, C);
impl_tuple_of_has_structure!(A, B, C, D);
impl_tuple_of_has_structure!(A, B, C, D, E);
impl_tuple_of_has_structure!(A, B, C, D, E, F);
impl_tuple_of_has_structure!(A, B, C, D, E, F, G);