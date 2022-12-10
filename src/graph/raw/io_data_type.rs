use std::assert_matches::assert_matches;
use std::mem::MaybeUninit;
use structural_reflection::{HasStructure, HasTypeName, RustType};
use crate::raw::{IODataRead, IODataWrite, Nullability};

/// A value with nullability info, which can be read from or written to a devolve graph
pub trait IODataType: Copy {
    fn rust_type() -> RustType;
    fn type_nullability() -> Nullability;
    unsafe fn _read_unchecked(data: &IODataRead<'_>) -> Self;
    unsafe fn _write_unchecked(self, data: &mut IODataWrite<'_>);
}

impl<T: IODataType> IODataType for Option<T> {
    fn rust_type() -> RustType {
        T::rust_type()
    }

    fn type_nullability() -> Nullability {
        Nullability::Null
    }

    unsafe fn _read_unchecked(data: &IODataRead<'_>) -> Self {
        if matches!(data.value_nullability(), Nullability::Null) {
            None
        } else {
            Some(T::_read_unchecked(data))
        }
    }

    unsafe fn _write_unchecked(self, data: &mut IODataWrite<'_>) {
        match self {
            None => *data.value_nullability_mut() = Nullability::Null,
            Some(value) => value._write_unchecked(data),
        }
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

macro_rules! impl_io_data_type_tuple {
    ($($name:ident),*) => {
impl<$($name: $crate::raw::IODataType),*> $crate::raw::IODataType for ($($name),*) {
    fn rust_type() -> structural_reflection::RustType {
        structural_reflection::RustType::c_tuple(
            __impl_io_data_types__chain_value!($( { $name::rust_type() } ),*).collect()
        )
    }

    fn type_nullability() -> $crate::raw::Nullability {
        $crate::raw::Nullability::Partial(
            __impl_io_data_types__chain_value!($( { $name::type_nullability() } ),*).collect()
        )
    }

    unsafe fn _read_unchecked(data: &$crate::raw::IODataRead<'_>) -> Self {
        let data_iter = data.iter();
        let result = ($(
            $name::_read_unchecked(data_iter.next().expect("tuple data is too short")),
        ),*)
        if data_iter.next().is_some() {
            panic!("tuple data is too long");
        }
        result
    }

    unsafe fn _write_unchecked(self, data: &mut $crate::raw::IODataWrite<'_>) {
        let ($($name),*) = self;
        let mut data_iter = data.iter_mut();
        $(
            $name._write_unchecked(data_iter.next().expect("tuple data is too short"));
        )*
        if data_iter.next().is_some() {
            panic!("tuple data is too long");
        }
    }
}
    };
}

impl_io_data_type_tuple!();
impl_io_data_type_tuple!(A);
impl_io_data_type_tuple!(A, B);
impl_io_data_type_tuple!(A, B, C);
impl_io_data_type_tuple!(A, B, C, D);
impl_io_data_type_tuple!(A, B, C, D, E);
impl_io_data_type_tuple!(A, B, C, D, E, F);
impl_io_data_type_tuple!(A, B, C, D, E, F, G);

#[cfg(test)]
mod tests {
    use crate::raw::{IODataTypes, NonNull, Nullable, Nullability, IOData};

    #[test]
    pub fn regression_tests() {
        let input_data = (
            Some("Placeholder"),
            "Text",
            Option::<bool>::None,
            Option::<()>::None
        );
        let io_data_type = IOData::init(input_data);
        assert_eq!(io_data_type.as_read().read().expect("read failed"), input_data);
        let mut io_data_type2 = IOData::uninit::<(
            Option<&'static str>,
            &'static str,
            Option<bool>,
            Option<()>,
        )>();
        io_data_type2.as_write().write(io_data_type2.as_read().read().expect("read failed")).expect("write failed");
        assert_eq!(io_data_type2.as_read().read().expect("read failed"), input_data);
        let mut io_data_type3 = IOData::uninit::<(
            Option<&'static str>,
            &'static str,
            Option<bool>,
            Option<()>,
        )>();
        io_data_type.copy_data_into(&mut io_data_type3).expect("copy_data_into failed");
    }
}