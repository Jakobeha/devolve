use std::assert_matches::assert_matches;
use std::mem::MaybeUninit;
use structural_reflection::{HasStructure, HasTypeName, RustType};
use crate::raw::Nullability;

/// A tuple of values with nullability info, which can be read from or written to a devolve graph
pub trait IODataTypes: Copy where <Self::Inner as HasTypeName>::StaticId: Sized {
    type Inner: HasStructure + Copy;
    type Normal: Copy;
    type IterRustTypes: Iterator<Item=RustType>;
    type IterNullRegions: Iterator<Item=Nullability>;

    fn iter_rust_types() -> Self::IterRustTypes;
    fn iter_max_nullabilitys() -> Self::IterNullRegions;
    fn len() -> usize;
    fn split(self) -> (MaybeUninit<Self::Inner>, Vec<Nullability>);
    fn into_normal(self) -> Self::Normal;
    fn new(inner: MaybeUninit<Self::Inner>, nullabilitys: Vec<Nullability>) -> Self;
}

/// A value with nullability info, which can be read from or written to a devolve graph
pub trait IODataType: Copy where <Self::Inner as HasTypeName>::StaticId: Sized {
    type Inner: HasStructure + Copy;
    type Normal: Copy;

    fn rust_type() -> RustType;
    fn max_nullability() -> Nullability;
    fn split(self) -> (MaybeUninit<Self::Inner>, Nullability);
    fn into_normal(self) -> Self::Normal;
    fn new(inner: MaybeUninit<Self::Inner>, nullability: Nullability) -> Self;
}

/// Non-null [IODataType]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonNull<T>(pub T);

/// Nullable [IODataType]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Nullable<T> {
    None,
    Some(T),
}

/// [IODataType] of a tuple or structure where some elements may be nullable and some may be non-null.
/// The tuple or structure's tuple items/fields are represented as a tuple, [IODataTypes].
#[derive(Debug, Clone, Copy)]
pub struct Partial<T: IODataTypes>(pub T) where <T::Inner as HasTypeName>::StaticId: Sized;

impl<T: HasStructure + Copy> IODataType for NonNull<T> where T::StaticId: Sized {
    type Inner = T;
    type Normal = T;

    fn rust_type() -> RustType {
        RustType::of::<T>()
    }

    fn max_nullability() -> Nullability {
        Nullability::NonNull
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, Nullability) {
        (MaybeUninit::new(self.0), Nullability::NonNull)
    }

    fn into_normal(self) -> Self::Normal {
        self.0
    }

    fn new(inner: MaybeUninit<Self::Inner>, nullability: Nullability) -> Self {
        assert_matches!(nullability, NullRegion::NonNull, "NonNull created with null data");
        NonNull(unsafe { inner.assume_init() })
    }
}

impl<T: HasStructure + Copy> IODataType for Nullable<T> where T::StaticId: Sized {
    type Inner = T;
    type Normal = Option<T>;

    fn rust_type() -> RustType {
        RustType::of::<T>()
    }

    fn max_nullability() -> Nullability {
        Nullability::Null
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, Nullability) {
        match self {
            Nullable::None => (MaybeUninit::uninit(), Nullability::Null),
            Nullable::Some(v) => (MaybeUninit::new(v), Nullability::NonNull),
        }
    }

    fn into_normal(self) -> Self::Normal {
        match self {
            Nullable::None => None,
            Nullable::Some(v) => Some(v),
        }
    }

    fn new(inner: MaybeUninit<Self::Inner>, nullability: Nullability) -> Self {
        match nullability {
            Nullability::Null => Nullable::None,
            Nullability::NonNull => Nullable::Some(unsafe { inner.assume_init() }),
            Nullability::Partial(_) => panic!("Cannot create Nullable from Partial"),
        }
    }
}

impl<T: IODataTypes> IODataType for Partial<T> where <T::Inner as HasTypeName>::StaticId: Sized {
    type Inner = T::Inner;
    type Normal = T::Normal;

    fn rust_type() -> RustType {
        RustType::of::<T::Inner>()
    }

    fn max_nullability() -> Nullability {
        Nullability::Partial(T::iter_max_nullabilitys().collect())
    }

    fn split(self) -> (MaybeUninit<Self::Inner>, Nullability) {
        let (value, nullabilitys) = self.0.split();
        (value, Nullability::Partial(nullabilitys))
    }

    fn into_normal(self) -> Self::Normal {
        self.0.into_normal()
    }

    fn new(inner: MaybeUninit<Self::Inner>, nullability: Nullability) -> Self {
        let nullabilitys = nullability.into_subdivide().take(T::len()).collect();
        Partial(T::new(inner, nullabilitys))
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
    #[allow(unused_parens)]
    type Normal = ($($name::Normal),*);
    type IterRustTypes = __impl_io_data_types__chain_type!(structural_reflection::RustType | $($name),*);
    type IterNullRegions = __impl_io_data_types__chain_type!($crate::raw::NullRegion | $($name),*);

    fn iter_rust_types() -> Self::IterRustTypes {
        __impl_io_data_types__chain_value!($({ structural_reflection::RustType::of::<$name::Inner>() }),*)
    }

    fn iter_max_nullabilitys() -> Self::IterNullRegions {
        __impl_io_data_types__chain_value!($({ $name::max_nullability() }),*)
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
        let mut nullabilitys = std::vec::Vec::with_capacity(Self::len());
        let mut offsets = structural_reflection::infer_c_tuple_elem_offsets(rust_types.iter());
        #[allow(unused_parens, non_snake_case)]
        let ($($name),*) = structural_reflection::c_tuple::CTuple::into_reg(self);
        $({
            let offset = offsets.next().unwrap();
            let (elem_value, elem_nullability) = $name.split();
            unsafe {
                value.as_mut_ptr().cast::<MaybeUninit<u8>>().add(offset).cast::<std::mem::MaybeUninit<$name::Inner>>().write(elem_value);
            }
            nullabilitys.push(elem_nullability);
        })*
        assert!(offsets.next().is_none());

        (value, nullabilitys)
    }

    fn into_normal(self) -> Self::Normal {
        #[allow(unused_parens, non_snake_case)]
        let ($($name),*) = structural_reflection::c_tuple::CTuple::into_reg(self);
        ($($name.into_normal()),*)
    }

    fn new(#[allow(unused_variables)] value: std::mem::MaybeUninit<Self::Inner>, nullabilitys: std::vec::Vec<$crate::raw::NullRegion>) -> Self {
        let rust_types = Self::iter_rust_types().collect::<Vec<_>>();
        let mut offsets = structural_reflection::infer_c_tuple_elem_offsets(rust_types.iter());
        let mut nullabilitys = nullabilitys.into_iter();
        let this = structural_reflection::c_tuple::c_tuple!($({
            let offset = offsets.next().unwrap();
            let elem_nullability = nullabilitys.next().unwrap();
            let elem_value = unsafe { value.as_ptr().cast::<MaybeUninit<u8>>().add(offset).cast::<std::mem::MaybeUninit<$name::Inner>>().read() };
            $name::new(elem_value, elem_nullability)
        }),*);
        assert!(offsets.next().is_none() && nullabilitys.next().is_none());
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

#[cfg(test)]
mod tests {
    use structural_reflection::c_tuple::{c_tuple, CTuple};
    use crate::raw::{IODataTypes, NonNull, Nullable, Nullability};

    #[test]
    pub fn regression_tests() {
        let input_data = c_tuple!(
            Nullable::Some("Placeholder"),
            NonNull("Text"),
            Nullable::<bool>::None,
            Nullable::<()>::None
        );
        let (input_raw_data, input_nullabilities) = input_data.split();
        assert_eq!(
            input_nullabilities,
            vec![Nullability::NonNull, Nullability::NonNull, Nullability::Null, Nullability::Null]
        );
        let input_data2 = <CTuple!(
            Nullable<&str>,
            NonNull<&str>,
            Nullable<bool>,
            Nullable<()>
        )>::new(input_raw_data, input_nullabilities);
        assert_eq!(input_data, input_data2);
    }
}