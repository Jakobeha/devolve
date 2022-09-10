use std::mem::MaybeUninit;
use std::ptr::null;
use structural_reflection::{HasStructure, RustType};
use crate::raw::NullRegion;

pub trait IODataType: Copy where Self::Inner::StaticId: Sized {
    type Inner: HasStructure + Copy;

    fn max_null_region() -> NullRegion;
    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion);
}

pub trait IODataTypes: Copy where Self::Inner::StaticId: Sized {
    type Inner: HasStructure + Copy;
    type IterRustTypes: Iterator<Item=RustType>;
    type IterNullRegions: Iterator<Item=NullRegion>;

    fn iter_rust_types() -> Self::IterRustTypes;
    fn iter_max_null_regions() -> Self::IterNullRegions;
    fn split(self) -> (MaybeUninit<Self::Inner>, Vec<NullRegion>);
}

#[derive(Debug, Clone, Copy)]
pub struct NonNull<T>(pub T);

#[derive(Debug, Clone, Copy)]
pub enum Nullable<T> {
    None,
    Some(T),
}

#[derive(Debug, Clone, Copy)]
pub struct Partial<T: IODataTypes>(pub T);

impl<T: HasStructure + Copy> IODataType for NonNull<T> where T::StaticId: Sized {
    type Inner = T;

    fn max_null_region() -> NullRegion {
        NullRegion::NonNull
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion) {
        (MaybeUninit::new(self.0), NullRegion::NonNull)
    }
}

impl<T: HasStructure + Copy> IODataType for Nullable<T> where T::StaticId: Sized {
    type Inner = T;

    fn max_null_region() -> NullRegion {
        NullRegion::Null
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion) {
        match self {
            Nullable::None => (MaybeUninit::uninit(), NullRegion::Null),
            Nullable::Some(v) => (MaybeUninit::new(v), NullRegion::NonNull),
        }
    }
}

impl<T: IODataTypes> IODataType for Partial<T> {
    type Inner = T::Inner;

    fn max_null_region() -> NullRegion {
        NullRegion::Partial(T::iter_max_null_regions().collect())
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion) {
        let (value, null_regions) = self.0.split();
        (value, NullRegion::Partial(null_regions))
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
    ($CTuple:tt $( | $($name:ident),* )?) => {
impl$(<$($name: $crate::raw::IODataType),*>)? $crate::raw::IODataType for $CTuple$(<$($name,)*>)? {
    type Inner = $CTuple$(<$($name::Inner,)*>)?;
    type IterRustTypes = __impl_io_data_types__chain_type!(structural_reflection::RustType | $($($name),*)?);
    type IterNullRegions = __impl_io_data_types__chain_type!($crate::raw::NullRegion | $($($name),*)?);

    fn iter_rust_types() -> Self::IterRustTypes {
        __impl_io_data_types__chain_value!($($({ structural_reflection::RustType::of::<$name::Inner>() }),*)?)
    }

    fn iter_max_null_regions() -> Self::IterNullRegions {
        __impl_io_data_types__chain_value!($($({ $name::max_null_region() }),*)?)
    }

    #[allow(non_snake_case)]
    fn split(($($($name,)*)?): &Self) -> (std::mem::MaybeUninit<Self::Inner>, std::vec::Vec<$crate::raw::NullRegion>) {
        let value = std::mem::MaybeUninit::<Self::Inner>::uninit();
        let null_regions = std::vec::Vec::with_capacity($($({ let _ = &$name; 1 } + )*)? 0);
        let mut offsets = structural_reflection::infer_c_tuple_elem_offsets(Self::iter_rust_types());
        $($({
            let offset = offsets.next().unwrap();
            let (elem_value, elem_nullability) = $name.split();
            unsafe {
                value.as_mut_ptr().add(offset).cast::<std::mem::MaybeUninit<$name::Inner>>().write(elem_value);
            }
            null_regions.push(elem_nullability);
        })*)?
        assert!(offsets.next().is_none());

        (value, null_regions)
    }

    #[allow(non_snake_case)]
    fn iter_raw_data(($($($name,)*)?): &Self) -> Self::IterRawData {
        __impl_io_data_types__chain_value!($($({ $name.raw_data() }),*)?)
    }
}
    };
}

impl_io_data_types!(());
impl_io_data_types!(CTuple1 | A);
impl_io_data_types!(CTuple2 | A, B);
impl_io_data_types!(CTuple3 | A, B, C);
impl_io_data_types!(CTuple4 | A, B, C, D);
impl_io_data_types!(CTuple5 | A, B, C, D, E);
impl_io_data_types!(CTuple6 | A, B, C, D, E, F);
impl_io_data_types!(CTuple7 | A, B, C, D, E, F, G);