use std::ptr::null;
use structural_reflection::{HasStructure, RustType};

pub trait IODataTypes {
    type OfOptionals;
    type IterRustTypes: Iterator<Item=RustType>;
    type IterIsSome: Iterator<Item=bool>;
    type IterRawData: Iterator<Item=*const ()>;

    fn iter_rust_types() -> Self::IterRustTypes;
    fn iter_is_some(values: &Self::OfOptionals) -> Self::IterIsSome;
    fn iter_raw_data(values: &Self::OfOptionals) -> Self::IterRawData;
}

fn option_as_raw_data<T: Copy + ?Sized>(data: Option<&T>) -> *const () {
    match data {
        None => null(),
        Some(data) => data as *const T as *const ()
    }
}

macro_rules! __impl_io_data_types__chain_type {
    ($ty:ty | ) => { std::iter::Empty<$ty> };
    ($ty:ty | $x:ident $(, $xs:ident)*) => { std::iter::Chain<std::iter::Once<$ty>, __impl_io_data_types__chain_type!($ty | $($xs),*)> };
}

macro_rules! __impl_io_data_types__chain_value {
    () => { std::iter::empty() };
    ({ $($x:tt)* } $(, { $($xs:tt)* })*) => { std::iter::once($($x)*).chain(__impl_io_data_types__chain_value!($({ $($xs)* }),*)) };
}

macro_rules! impl_io_data_types {
    ($($name:ident),*) => {
impl<$($name: HasStructure + Copy),*> IODataTypes for ($($name,)*) where $($name::StaticId: Sized),* {
    type OfOptionals = ($(Option<$name>,)*);
    type IterRustTypes = __impl_io_data_types__chain_type!(RustType | $($name),*);
    type IterIsSome = __impl_io_data_types__chain_type!(bool | $($name),*);
    type IterRawData = __impl_io_data_types__chain_type!(*const () | $($name),*);

    fn iter_rust_types() -> Self::IterRustTypes {
        __impl_io_data_types__chain_value!($({ RustType::of::<$name>() }),*)
    }

    #[allow(non_snake_case)]
    fn iter_is_some(($($name,)*): &Self::OfOptionals) -> Self::IterIsSome {
        __impl_io_data_types__chain_value!($({ $name.is_some() }),*)
    }

    #[allow(non_snake_case)]
    fn iter_raw_data(($($name,)*): &Self::OfOptionals) -> Self::IterRawData {
        __impl_io_data_types__chain_value!($({ option_as_raw_data($name.as_ref()) }),*)
    }
}
    };
}

impl_io_data_types!();
impl_io_data_types!(A);
impl_io_data_types!(A, B);
impl_io_data_types!(A, B, C);
impl_io_data_types!(A, B, C, D);
impl_io_data_types!(A, B, C, D, E);
impl_io_data_types!(A, B, C, D, E, F);
impl_io_data_types!(A, B, C, D, E, F, G);