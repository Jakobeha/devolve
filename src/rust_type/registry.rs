use std::any::TypeId;
use std::mem::{size_of, align_of};
use lazy_static::lazy_static;
use log::error;
use crate::misc::catch_and_log::catch_and_log;
use crate::rust_type::RustType;

use crate::rust_type::structure::TypeStructure;
use crate::rust_type::type_name::RustTypeName;

/// A type we know the structure of at compile type. We can derive this
pub trait HasStructure: 'static {
    fn type_name() -> RustTypeName;
    fn structure() -> TypeStructure;
}

lazy_static! {
    static ref KNOWN_TYPES: RwLock<HashMap<RustTypeName, RustType>> = RwLock::new(HashMap::new());
}

impl RustType {
    pub fn of<T: HasStructure>() -> Self {
        RustType {
            type_id: Some(TypeId::of::<T>()),
            type_name: T::type_name(),
            size: size_of::<T>(),
            align: align_of::<T>(),
            structure: T::structure()
        }
    }

    pub fn register_manually(rust_type: RustType) {
        let type_name = &rust_type.type_name;
        if let Some(mut known_types) = catch_and_log!(KNOWN_TYPES.write(), "known rust types poisoned") {
            if let Some(existing_type) = known_types.get(type_name) {
                if existing_type.structure() != &known_type.structure() {
                    error!("rust type with name {} already registered with a different structure", type_name.qualified());
                }
            }
            known_types.insert(type_name.clone(), rust_type);
        }
    }

    pub fn register_auto<T: HasStructure>() {
        Self::register_manually(RustType::of::<T>());
    }

    pub fn get(type_name: &RustTypeName) -> Option<RustType> {
        match catch_and_log!(KNOWN_TYPES.read(), "known rust types poisoned") {
            None => None,
            Some(known_types) => known_types.get(type_name).cloned()
        }
    }
}