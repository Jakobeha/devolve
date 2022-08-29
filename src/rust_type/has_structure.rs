use crate::rust_type::{TypeStructure, RustTypeName, RustPointerKind, PrimitiveType, RustType};

pub trait HasTypeName {
    fn type_name() -> RustTypeName;
}

/// A type we know the structure of at compile type. We can derive this
pub trait HasStructure: HasTypeName {
    fn structure() -> TypeStructure;
}

impl HasTypeName for () {
    fn type_name() -> RustTypeName {
        RustTypeName::Tuple {
            elems: vec![]
        }
    }
}

impl HasStructure for () {
    fn structure() -> TypeStructure {
        TypeStructure::CTuple {
            elements: vec![],
        }
    }
}

impl HasTypeName for str {
    fn type_name() -> RustTypeName {
        RustTypeName::Ident {
            qualifiers: vec![],
            simple_name: "str".to_string(),
            generic_args: vec![]
        }
    }
}

impl<T: HasTypeName> HasTypeName for [T] {
    fn type_name() -> RustTypeName {
        RustTypeName::Slice {
            elem: Box::new(T::type_name())
        }
    }
}

impl<T: HasStructure> HasStructure for [T] {
    fn structure() -> TypeStructure {
        TypeStructure::Slice {
            elem: Box::new(RustType::of::<T>())
        }
    }
}

impl<T: HasTypeName, const LEN: usize> HasTypeName for [T; LEN] {
    fn type_name() -> RustTypeName {
        RustTypeName::Array {
            elem: Box::new(T::type_name()),
            length: LEN
        }
    }
}

impl<T: HasStructure, const LEN: usize> HasStructure for [T; LEN] {
    fn structure() -> TypeStructure {
        TypeStructure::Array {
            elem: Box::new(RustType::of::<T>()),
            length: LEN
        }
    }
}

impl<T: HasTypeName> HasTypeName for Option<T> {
    fn type_name() -> RustTypeName {
        T::type_name()
    }
}

impl<T: HasStructure> HasStructure for Option<T> {
    fn structure() -> TypeStructure {
        T::structure()
    }
}

macro impl_has_structure_primitive($prim_tt:tt, $prim_type:ident) {
impl HasTypeName for $prim_tt {
    fn type_name() -> RustTypeName {
        RustTypeName::Ident {
            qualifiers: vec![],
            simple_name: stringify!($prim_tt).to_string(),
            generic_args: vec![]
        }
    }
}

impl HasStructure for $prim_tt {
    fn structure() -> TypeStructure {
        TypeStructure::Primitive(PrimitiveType::$prim_type)
    }
}
}

macro impl_has_structure_pointer(($($ptr_tt:tt)+), $ptr_kind:ident) {
impl<T: HasTypeName + ?Sized> HasTypeName for $($ptr_tt)+ T {
    fn type_name() -> RustTypeName {
        RustTypeName::Pointer {
            ptr_kind: RustPointerKind::$ptr_kind,
            refd: Box::new(T::type_name())
        }
    }
}

impl<T: HasTypeName + ?Sized> HasStructure for $($ptr_tt)+ T {
    fn structure() -> TypeStructure {
        TypeStructure::Pointer {
            refd: T::type_name(),
        }
    }
}
}

impl_has_structure_primitive!(u8, U8);
impl_has_structure_primitive!(u16, U16);
impl_has_structure_primitive!(u32, U32);
impl_has_structure_primitive!(u64, U64);
impl_has_structure_primitive!(u128, U128);
impl_has_structure_primitive!(usize, Usize);
impl_has_structure_primitive!(i8, I8);
impl_has_structure_primitive!(i16, I16);
impl_has_structure_primitive!(i32, I32);
impl_has_structure_primitive!(i64, I64);
impl_has_structure_primitive!(i128, I128);
impl_has_structure_primitive!(isize, Isize);
impl_has_structure_primitive!(bool, Bool);
impl_has_structure_primitive!(char, Char);
impl_has_structure_primitive!(f32, F32);
impl_has_structure_primitive!(f64, F64);

impl_has_structure_pointer!((&), ImmRef);
impl_has_structure_pointer!((&mut), MutRef);
impl_has_structure_pointer!((*const), ImmPtr);
impl_has_structure_pointer!((*mut), MutPtr);

