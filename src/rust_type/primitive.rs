use std::any::TypeId;
use std::fmt::{Display, Formatter};
use std::mem::{align_of, size_of};
use crate::rust_type::RustType;
use crate::rust_type::structure::TypeStructure;
use crate::rust_type::type_name::RustTypeName;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    F32,
    F64,
    Bool,
    Char
}

impl PrimitiveType {
    pub fn rust_type(&self) -> RustType {
        RustType {
            type_id: Some(self.type_id()),
            type_name: self.rust_type_name(),
            size: self.size(),
            align: self.align(),
            structure: TypeStructure::Primitive(*self)
        }
    }

    pub fn rust_type_name(&self) -> RustTypeName {
        RustTypeName::primitive(self.name())
    }

    pub fn type_id(&self) -> TypeId {
        match self {
            PrimitiveType::I8 => TypeId::of::<i8>(),
            PrimitiveType::I16 => TypeId::of::<i16>(),
            PrimitiveType::I32 => TypeId::of::<i32>(),
            PrimitiveType::I64 => TypeId::of::<i64>(),
            PrimitiveType::I128 => TypeId::of::<i128>(),
            PrimitiveType::Isize => TypeId::of::<isize>(),
            PrimitiveType::U8 => TypeId::of::<u8>(),
            PrimitiveType::U16 => TypeId::of::<u16>(),
            PrimitiveType::U32 => TypeId::of::<u32>(),
            PrimitiveType::U64 => TypeId::of::<u64>(),
            PrimitiveType::U128 => TypeId::of::<u128>(),
            PrimitiveType::Usize => TypeId::of::<usize>(),
            PrimitiveType::F32 => TypeId::of::<f32>(),
            PrimitiveType::F64 => TypeId::of::<f64>(),
            PrimitiveType::Bool => TypeId::of::<bool>(),
            PrimitiveType::Char => TypeId::of::<char>(),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            PrimitiveType::I8 => "i8",
            PrimitiveType::I16 => "i16",
            PrimitiveType::I32 => "i32",
            PrimitiveType::I64 => "i64",
            PrimitiveType::I128 => "i128",
            PrimitiveType::Isize => "isize",
            PrimitiveType::U8 => "u8",
            PrimitiveType::U16 => "u16",
            PrimitiveType::U32 => "u32",
            PrimitiveType::U64 => "u64",
            PrimitiveType::U128 => "u128",
            PrimitiveType::Usize => "usize",
            PrimitiveType::F32 => "f32",
            PrimitiveType::F64 => "f64",
            PrimitiveType::Bool => "bool",
            PrimitiveType::Char => "char",
        }
    }

    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::I8 => size_of::<i8>(),
            PrimitiveType::I16 => size_of::<i16>(),
            PrimitiveType::I32 => size_of::<i32>(),
            PrimitiveType::I64 => size_of::<i64>(),
            PrimitiveType::I128 => size_of::<i128>(),
            PrimitiveType::Isize => size_of::<isize>(),
            PrimitiveType::U8 => size_of::<u8>(),
            PrimitiveType::U16 => size_of::<u16>(),
            PrimitiveType::U32 => size_of::<u32>(),
            PrimitiveType::U64 => size_of::<u64>(),
            PrimitiveType::U128 => size_of::<u128>(),
            PrimitiveType::Usize => size_of::<usize>(),
            PrimitiveType::F32 => size_of::<f32>(),
            PrimitiveType::F64 => size_of::<f64>(),
            PrimitiveType::Bool => size_of::<bool>(),
            PrimitiveType::Char => size_of::<char>(),
        }
    }

    pub fn align(&self) -> usize {
        match self {
            PrimitiveType::I8 => align_of::<i8>(),
            PrimitiveType::I16 => align_of::<i16>(),
            PrimitiveType::I32 => align_of::<i32>(),
            PrimitiveType::I64 => align_of::<i64>(),
            PrimitiveType::I128 => align_of::<i128>(),
            PrimitiveType::Isize => align_of::<isize>(),
            PrimitiveType::U8 => align_of::<u8>(),
            PrimitiveType::U16 => align_of::<u16>(),
            PrimitiveType::U32 => align_of::<u32>(),
            PrimitiveType::U64 => align_of::<u64>(),
            PrimitiveType::U128 => align_of::<u128>(),
            PrimitiveType::Usize => align_of::<usize>(),
            PrimitiveType::F32 => align_of::<f32>(),
            PrimitiveType::F64 => align_of::<f64>(),
            PrimitiveType::Bool => align_of::<bool>(),
            PrimitiveType::Char => align_of::<char>(),
        }
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}
