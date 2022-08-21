use std::any::{type_name, TypeId};
use std::mem::{size_of, align_of};

#[derive(Debug, Clone, PartialEq)]
pub struct IntrinsicRustType {
    pub type_id: TypeId,
    pub type_name: &'static str,
    pub size: usize,
    pub align: usize,
    _private_ctor: ()
}

impl IntrinsicRustType {
    pub fn of<T: 'static>() -> Self {
        IntrinsicRustType {
            type_id: TypeId::of::<T>(),
            type_name: type_name::<T>(),
            size: size_of::<T>(),
            align: align_of::<T>(),
            _private_ctor: ()
        }
    }
}