use std::any::{type_name, TypeId};
use std::mem::{size_of, align_of};
use crate::rust_type::HasStaticTypeId;

// This can't be exposed, otherwise users could call IntrinsicRustType::of on it
pub(super) enum UnknownIntrinsicType {}

#[derive(Debug, Clone)]
pub struct IntrinsicRustType {
    pub type_id: TypeId,
    pub type_name: &'static str,
    pub size: usize,
    pub align: usize,
    _private_ctor: ()
}

impl PartialEq for IntrinsicRustType {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}

impl Eq for IntrinsicRustType {}

impl IntrinsicRustType {
    pub fn of<T: HasStaticTypeId>() -> Self {
        IntrinsicRustType {
            type_id: T::static_type_id(),
            type_name: type_name::<T>(),
            size: size_of::<T>(),
            align: align_of::<T>(),
            _private_ctor: ()
        }
    }

    pub fn unknown() -> Self {
        IntrinsicRustType {
            type_id: TypeId::of::<UnknownIntrinsicType>(),
            type_name: "{unknown}",
            size: usize::MAX,
            align: usize::MAX,
            _private_ctor: ()
        }
    }
}