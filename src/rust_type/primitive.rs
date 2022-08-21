use std::fmt::{Display, Formatter};
use std::mem::{align_of, size_of};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Bool,
    Char,
}

impl PrimitiveType {
    pub fn size(&self) -> usize {
        match self {
            PrimitiveType::I8 => size_of::<i8>(),
            PrimitiveType::I16 => size_of::<i16>(),
            PrimitiveType::I32 => size_of::<i32>(),
            PrimitiveType::I64 => size_of::<i64>(),
            PrimitiveType::Isize => size_of::<isize>(),
            PrimitiveType::U8 => size_of::<u8>(),
            PrimitiveType::U16 => size_of::<u16>(),
            PrimitiveType::U32 => size_of::<u32>(),
            PrimitiveType::U64 => size_of::<u64>(),
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
            PrimitiveType::Isize => align_of::<isize>(),
            PrimitiveType::U8 => align_of::<u8>(),
            PrimitiveType::U16 => align_of::<u16>(),
            PrimitiveType::U32 => align_of::<u32>(),
            PrimitiveType::U64 => align_of::<u64>(),
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
        match self {
            PrimitiveType::I8 => write!(f, "i8"),
            PrimitiveType::I16 => write!(f, "i16"),
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::I64 => write!(f, "i64"),
            PrimitiveType::Isize => write!(f, "isize"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::U16 => write!(f, "u16"),
            PrimitiveType::U32 => write!(f, "u32"),
            PrimitiveType::U64 => write!(f, "u64"),
            PrimitiveType::Usize => write!(f, "usize"),
            PrimitiveType::F32 => write!(f, "f32"),
            PrimitiveType::F64 => write!(f, "f64"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Char => write!(f, "char"),
        }
    }
}
