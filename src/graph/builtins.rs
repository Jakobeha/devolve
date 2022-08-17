use lazy_static::lazy_static;
use std::sync::RwLock;
use std::collections::HashMap;
use log::error;
use crate::misc::catch_and_log::catch_and_log;
use crate::graph::mutable::{NodeInput, NodeTypeData};
use crate::graph::raw::RawComputeFn;

#[derive(Clone)]
pub struct BuiltinNodeType {
    pub compute: RawComputeFn,
    pub type_data: NodeTypeData,
    pub default_inputs: Vec<NodeInput>
}

lazy_static! {
    static ref BUILTINS: RwLock<HashMap<String, BuiltinNodeType>> = RwLock::new(HashMap::new());
}

impl BuiltinNodeType {
    pub fn register(name: String, builtin: BuiltinNodeType) {
        if let Some(mut builtins) = catch_and_log!(BUILTINS.write(), "builtin node types poisoned") {
            if builtins.contains(&name) {
                error!("builtin node type with name {} already registered", name);
            }
            builtins.insert(name, builtin);
        }
    }

    pub fn get(name: &str) -> Option<BuiltinNodeType> {
        match catch_and_log!(BUILTINS.read(), "builtin node types poisoned") {
            None => None,
            Some(builtins) => builtins.get(name).cloned()
        }
    }
}