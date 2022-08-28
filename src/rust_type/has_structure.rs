use std::any::TypeId;
use crate::rust_type::{TypeStructure, IntrinsicRustType, RustTypeName, RustPointerKind, PrimitiveType};

pub trait HasStaticTypeId {
    fn static_type_id() -> TypeId;
}

impl<T: ?Sized + 'static> HasStaticTypeId for T {
    fn static_type_id() -> TypeId {
        TypeId::of::<T>()
    }
}

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
impl<T: HasTypeName> HasTypeName for $($ptr_tt)+ T {
    fn type_name() -> RustTypeName {
        RustTypeName::Pointer {
            ptr_kind: RustPointerKind::$ptr_kind,
            refd: Box::new(T::type_name())
        }
    }
}

impl<T: HasTypeName + 'static> HasStructure for $($ptr_tt)+ T {
    fn structure() -> TypeStructure {
        TypeStructure::Pointer {
            refd: IntrinsicRustType::of::<T>(),
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

