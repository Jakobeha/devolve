use lazy_static::lazy_static;
use std::sync::RwLock;
use std::collections::HashMap;
use std::error::Error;
use log::error;
use crate::misc::catch_and_log::catch_and_log;
use crate::graph::mutable::{NodeInput, NodeTypeData};
use crate::graph::raw::RawComputeFn;
use crate::rust_type::RustType;

#[derive(Clone)]
pub struct BuiltinNodeType {
    pub compute: RawComputeFn,
    pub type_data: NodeTypeData,
    pub default_inputs: Vec<NodeInput>
}

pub struct BuiltinNodeTypeFnCtx<'a> {
    pub resolved_rust_types: &'a HashMap<String, RustType>
}

/// The function takes one argument, which may have commas. You can split on the commas and that
/// emulates taking multiple arguments.
pub type BuiltinNodeTypeFn = Box<dyn Fn(&str, BuiltinNodeTypeFnCtx<'_>) -> Result<BuiltinNodeType, Box<dyn Error>> + Send + Sync>;

lazy_static! {
    static ref BUILTINS: RwLock<HashMap<String, BuiltinNodeType>> = RwLock::new(HashMap::new());
    static ref BUILTIN_FNS: RwLock<HashMap<String, BuiltinNodeTypeFn>> = RwLock::new(HashMap::new());
}

impl BuiltinNodeType {
    pub fn register(name: String, builtin: BuiltinNodeType) {
        if let Some(mut builtins) = catch_and_log!(BUILTINS.write(), "builtin node types poisoned") {
            if builtins.contains_key(&name) {
                error!("builtin node type with name {} already registered", name);
            }
            builtins.insert(name, builtin);
        }
    }

    pub fn register_fn(name: String, builtin_fn: BuiltinNodeTypeFn) {
        if let Some(mut builtin_fns) = catch_and_log!(BUILTIN_FNS.write(), "builtin node type functions poisoned") {
            if builtin_fns.contains_key(&name) {
                error!("builtin node type function with name {} already registered", name);
            }
            builtin_fns.insert(name, builtin_fn);
        }
    }

    pub fn get(name: &str) -> Option<BuiltinNodeType> {
        match catch_and_log!(BUILTINS.read(), "builtin node types poisoned") {
            None => None,
            Some(builtins) => builtins.get(name).cloned()
        }
    }

    pub fn get_and_call_fn(fn_name: &str, fn_arg: &str, fn_ctx: BuiltinNodeTypeFnCtx<'_>) -> Option<Result<BuiltinNodeType, Box<dyn Error>>> {
        match catch_and_log!(BUILTIN_FNS.read(), "builtin node type functions poisoned") {
            None => None,
            Some(builtin_fns) => builtin_fns.get(fn_name).map(|builtin_fn| builtin_fn(fn_arg, fn_ctx))
        }
    }
}