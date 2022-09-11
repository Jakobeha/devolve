use std::assert_matches::assert_matches;
use std::mem::MaybeUninit;
use structural_reflection::{HasStructure, HasTypeName, RustType};
use crate::raw::NullRegion;

pub trait IODataType: Copy where <Self::Inner as HasTypeName>::StaticId: Sized {
    type Inner: HasStructure + Copy;

    fn max_null_region() -> NullRegion;
    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion);
    fn new(inner: MaybeUninit<Self::Inner>, null_region: NullRegion) -> Self;
}

pub trait IODataTypes: Copy where <Self::Inner as HasTypeName>::StaticId: Sized {
    type Inner: HasStructure + Copy;
    type IterRustTypes: Iterator<Item=RustType>;
    type IterNullRegions: Iterator<Item=NullRegion>;

    fn iter_rust_types() -> Self::IterRustTypes;
    fn iter_max_null_regions() -> Self::IterNullRegions;
    fn len() -> usize;
    fn split(self) -> (MaybeUninit<Self::Inner>, Vec<NullRegion>);
    fn new(inner: MaybeUninit<Self::Inner>, null_regions: Vec<NullRegion>) -> Self;
}

#[derive(Debug, Clone, Copy)]
pub struct NonNull<T>(pub T);

#[derive(Debug, Clone, Copy)]
pub enum Nullable<T> {
    None,
    Some(T),
}

#[derive(Debug, Clone, Copy)]
pub struct Partial<T: IODataTypes>(pub T) where <T::Inner as HasTypeName>::StaticId: Sized;

impl<T: HasStructure + Copy> IODataType for NonNull<T> where T::StaticId: Sized {
    type Inner = T;

    fn max_null_region() -> NullRegion {
        NullRegion::NonNull
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion) {
        (MaybeUninit::new(self.0), NullRegion::NonNull)
    }

    fn new(inner: MaybeUninit<Self::Inner>, null_region: NullRegion) -> Self {
        assert_matches!(null_region, NullRegion::NonNull);
        NonNull(unsafe { inner.assume_init() })
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

    fn new(inner: MaybeUninit<Self::Inner>, null_region: NullRegion) -> Self {
        match null_region {
            NullRegion::Null => Nullable::None,
            NullRegion::NonNull => Nullable::Some(unsafe { inner.assume_init() }),
            NullRegion::Partial(_) => panic!("Cannot create Nullable from Partial"),
        }
    }
}

impl<T: IODataTypes> IODataType for Partial<T> where <T::Inner as HasTypeName>::StaticId: Sized {
    type Inner = T::Inner;

    fn max_null_region() -> NullRegion {
        NullRegion::Partial(T::iter_max_null_regions().collect())
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, NullRegion) {
        let (value, null_regions) = self.0.split();
        (value, NullRegion::Partial(null_regions))
    }

    fn new(inner: MaybeUninit<Self::Inner>, null_region: NullRegion) -> Self {
        let null_regions = null_region.into_subdivide().take(T::len()).collect();
        Partial(T::new(inner, null_regions))
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

macro_rules! __impl_io_data_types__ignore {
    ($($x:tt)*) => {};
}

macro_rules! __impl_io_data_types__index {
    ($expr:ident.A) => { $expr.0 };
    ($expr:ident.B) => { $expr.1 };
    ($expr:ident.C) => { $expr.2 };
    ($expr:ident.D) => { $expr.3 };
    ($expr:ident.E) => { $expr.4 };
    ($expr:ident.F) => { $expr.5 };
    ($expr:ident.G) => { $expr.6 };
}

macro_rules! impl_io_data_types {
    ($($name:ident),*) => {
impl<$($name: $crate::raw::IODataType),*> $crate::raw::IODataTypes for structural_reflection::c_tuple::CTuple!($($name),*) where $(<$name::Inner as HasTypeName>::StaticId: Sized),* {
    type Inner = structural_reflection::c_tuple::CTuple!($($name::Inner),*);
    type IterRustTypes = __impl_io_data_types__chain_type!(structural_reflection::RustType | $($name),*);
    type IterNullRegions = __impl_io_data_types__chain_type!($crate::raw::NullRegion | $($name),*);

    fn iter_rust_types() -> Self::IterRustTypes {
        __impl_io_data_types__chain_value!($({ structural_reflection::RustType::of::<$name::Inner>() }),*)
    }

    fn iter_max_null_regions() -> Self::IterNullRegions {
        __impl_io_data_types__chain_value!($({ $name::max_null_region() }),*)
    }

    fn len() -> usize {
        #[allow(unused_mut)]
        let mut number = 0;
        $({__impl_io_data_types__ignore!($name); number += 1; })*
        number
    }

    fn split(self) -> (std::mem::MaybeUninit<Self::Inner>, std::vec::Vec<$crate::raw::NullRegion>) {
        let rust_types = Self::iter_rust_types().collect::<Vec<_>>();
        #[allow(unused_mut)]
        let mut value = std::mem::MaybeUninit::<Self::Inner>::uninit();
        #[allow(unused_mut)]
        let mut null_regions = std::vec::Vec::with_capacity(Self::len());
        let mut offsets = structural_reflection::infer_c_tuple_elem_offsets(rust_types.iter());
        $({
            let offset = offsets.next().unwrap();
            let (elem_value, elem_nullability) = __impl_io_data_types__index!(self.$name).split();
            unsafe {
                value.as_mut_ptr().add(offset).cast::<std::mem::MaybeUninit<$name::Inner>>().write(elem_value);
            }
            null_regions.push(elem_nullability);
        })*
        assert!(offsets.next().is_none());

        (value, null_regions)
    }

    fn new(#[allow(unused_variables)] value: std::mem::MaybeUninit<Self::Inner>, null_regions: std::vec::Vec<$crate::raw::NullRegion>) -> Self {
        let rust_types = Self::iter_rust_types().collect::<Vec<_>>();
        let mut offsets = structural_reflection::infer_c_tuple_elem_offsets(rust_types.iter());
        let mut null_regions = null_regions.into_iter();
        let this = structural_reflection::c_tuple::c_tuple!($({
            let offset = offsets.next().unwrap();
            let elem_nullability = null_regions.next().unwrap();
            let elem_value = unsafe { value.as_ptr().add(offset).cast::<std::mem::MaybeUninit<$name::Inner>>().read() };
            $name::new(elem_value, elem_nullability)
        }),*);
        assert!(offsets.next().is_none() && null_regions.next().is_none());
        this
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