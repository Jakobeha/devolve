use std::any::TypeId;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use bimap::BiHashMap;
use std::mem::{size_of, align_of};
use lazy_static::lazy_static;
use log::error;
use crate::misc::catch_and_log::catch_and_log;
use crate::rust_type::{IntrinsicRustType, RustType};
use crate::rust_type::intrinsic::UnknownIntrinsicType;

use crate::rust_type::structure::TypeStructure;
use crate::rust_type::type_name::RustTypeName;

/// A type we know the structure of at compile type. We can derive this
pub trait HasStructure: 'static {
    fn type_name() -> RustTypeName;
    fn structure() -> TypeStructure;
}

lazy_static! {
    static ref KNOWN_TYPES: RwLock<HashMap<RustTypeName, RustType>> = RwLock::new({
        let mut known_types = HashMap::new();
        RustType::register_builtins(&mut known_types);
        known_types
    });
    static ref KNOWN_NAMES: RwLock<BiHashMap<TypeId, RustTypeName>> = RwLock::new(BiHashMap::new());
    static ref KNOWN_INTRINSICS: RwLock<HashMap<TypeId, IntrinsicType>> = RwLock::new(HashMap::new());
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

    pub fn register_manually(rust_type: RustType, intrinsic_rust_type: Option<IntrinsicRustType>) {
        if let Some(intrinsic_type) = intrinsic_rust_type {
            IntrinsicRustType::register(intrinsic_type);
        }
        if let Some(type_id) = rust_type.type_id {
            RustTypeName::register(type_id, rust_type.type_name.clone())
        }
        Self::register_just_type(rust_type);
    }

    fn register_just_type(rust_type: RustType) {
        let type_name = &rust_type.type_name;
        if let Some(mut known_types) = catch_and_log!(KNOWN_TYPES.write(), "known rust types poisoned") {
            if let Some(existing_type) = known_types.get(type_name) {
                if !existing_type.same(&rust_type) {
                    error!("rust type with name {} already registered with a different structure", type_name.qualified());
                }
            }
            known_types.insert(type_name.clone(), rust_type);
        }
    }

    pub fn register_auto<T: HasStructure>() {
        Self::register_manually(RustType::of::<T>(), IntrinsicRustType::of::<T>());
    }

    pub fn lookup(type_name: &RustTypeName) -> Option<RustType> {
        match catch_and_log!(KNOWN_TYPES.read(), "known rust types poisoned") {
            None => None,
            Some(known_types) => known_types.get(type_name).cloned()
        }
    }

    pub fn lookup_from_id(type_id: TypeId) -> Option<RustType> {
        match (catch_and_log!(KNOWN_NAMES.read(), "known rust type names poisoned"), catch_and_log!(KNOWN_TYPES.read(), "known rust types poisoned")) {
            (Some(known_names), Some(known_types)) => known_names.get_by_left(&type_id).and_then(|type_name| known_types.get(&*type_name)),
            _ => None
        }
    }
}

impl RustTypeName {
    pub fn register(type_id: TypeId, type_name: RustTypeName) {
        if let Some(mut known_names) = catch_and_log!(KNOWN_NAMES.write(), "known rust type names poisoned") {
            if let Some(existing_name) = known_names.get(type_id) {
                if existing_name != &type_name {
                    error!("rust type with id {:?} already registered with a different name: old={} new={}", type_id, existing_name.qualified(), type_name.qualified());
                }
            }
            known_names.insert(type_id, type_name);
        }
    }

    pub fn lookup(type_id: TypeId) -> Option<RustTypeName> {
        match catch_and_log!(KNOWN_NAMES.read(), "known rust type names poisoned") {
            None => None,
            Some(known_names) => known_names.get_by_left(&type_id).cloned()
        }
    }

    pub fn lookup_back(&self) -> Option<TypeId> {
        match catch_and_log!(KNOWN_NAMES.read(), "known rust type names poisoned") {
            None => None,
            Some(known_names) => known_names.get_by_right(self).copied()
        }
    }

    pub fn lookup_back_intrinsic(&self) -> Option<IntrinsicRustType> {
        self.lookup_back().and_then(IntrinsicRustType::lookup)
    }
}

impl IntrinsicRustType {
    pub fn register(intrinsic_type: IntrinsicRustType) {
        if let Some(mut known_intrinsics) = catch_and_log!(KNOWN_INTRINSICS.write(), "known intrinsic types poisoned") {
            if let Some(existing_intrinsic) = known_intrinsics.get(&intrinsic_type.type_id) {
                if existing_intrinsic != &intrinsic_type {
                    error!("intrinsic type with id {:?} already registered with a different name: old={} new={}", intrinsic_type.type_id, existing_intrinsic.qualified(), intrinsic_type.qualified());
                }
            }
            known_intrinsics.insert(intrinsic_type.type_id, intrinsic_type);
        }
    }

    pub fn lookup(type_id: TypeId) -> Option<IntrinsicRustType> {
        match catch_and_log!(KNOWN_INTRINSICS.read(), "known intrinsic types poisoned") {
            None => None,
            Some(known_intrinsics) => known_intrinsics.get(&type_id).cloned()
        }
    }
}

// region builtins
impl RustType {
    fn register_builtins(builtins: &mut HashMap<RustTypeName, RustType>) {
        RustType::register_builtin::<String>(builtins, "String");
        RustType::register_builtin::<Box<UnknownIntrinsicType>>(builtins, "Box<{unknown}>");
        RustType::register_builtin::<Vec<UnknownIntrinsicType>>(builtins, "Vec<{unknown}>");
        RustType::register_builtin::<VecDeque<UnknownIntrinsicType>>(builtins, "VecDeque<{unknown}>");
        RustType::register_builtin::<BTreeSet<UnknownIntrinsicType>>(builtins, "BTreeSet<{unknown}>");
        RustType::register_builtin::<HashSet<UnknownIntrinsicType>>(builtins, "HashSet<{unknown}>");
        RustType::register_builtin::<BTreeMap<UnknownIntrinsicType, UnknownIntrinsicType>>(builtins, "BTreeSet<{unknown}, {unknown}>");
        RustType::register_builtin::<HashMap<UnknownIntrinsicType, UnknownIntrinsicType>>(builtins, "HashSet<{unknown}, {unknown}>");
    }

    fn register_builtin<T: 'static>(builtins: &mut HashMap<RustTypeName, RustType>, name: &str) {
        let type_name = RustTypeName::try_from(name).expect("bad builtin name");
        let intrinsic = IntrinsicRustType::of::<T>();
        let rust_type = RustType {
            type_id: Some(intrinsic.type_id),
            type_name: type_name.clone(),
            size: intrinsic.size,
            align: intrinsic.align,
            structure: TypeStructure::Opaque
        };
        let old = builtins.insert(type_name.clone(), rust_type);
        if old.is_some() {
            panic!("builtin type {} already registered", type_name.qualified());
        }

        // We can access and load the other lazy members here
        RustTypeName::register(intrinsic.type_id, type_name);
        IntrinsicRustType::register(intrinsic);
    }
}
// endregion