use std::collections::HashMap;
use std::error::Error;
use log::error;
use crate::graph::mutable::{NodeInput, NodeTypeData};
use crate::graph::raw::{RawComputeFn, UsedRegion};
use crate::rust_type::RustType;

/// Map of node types available to
pub struct NodeTypes {
    statics: HashMap<String, NodeType>,
    fns: HashMap<String, NodeTypeFn>
}

#[derive(Clone)]
pub struct NodeType {
    pub compute: RawComputeFn,
    pub type_data: NodeTypeData,
    pub default_inputs: Vec<NodeInput>,
    pub required_inputs: Vec<UsedRegion>
}

pub struct NodeTypeFnCtx<'a> {
    pub resolved_rust_types: &'a HashMap<String, RustType>
}

/// The function takes one argument, which may have commas. You can split on the commas and that
/// emulates taking multiple arguments.
pub type NodeTypeFn = Box<dyn Fn(&str, NodeTypeFnCtx<'_>) -> Result<NodeType, Box<dyn Error>> + Send + Sync>;

impl NodeTypes {
    pub fn new() -> Self {
        NodeTypes {
            statics: HashMap::new(),
            fns: HashMap::new()
        }
    }

    /// Logs an error and overrides if the name already exists. Use [NodeTypes::contains] to check
    pub fn insert(&mut self, name: String, node_type: NodeType) {
        if self.statics.contains_key(&name) {
            error!("node type with name {} already inserted", name);
        }
        self.statics.insert(name, node_type);
    }

    /// Logs an error and overrides if the name already exists. Use [NodeTypes::contains_fn] to check
    pub fn insert_fn(&mut self, name: String, node_type_fn: NodeTypeFn) {
        if self.fns.contains_key(&name) {
            error!("node type function with name {} already inserted", name);
        }
        self.fns.insert(name, node_type_fn);
    }

    pub fn contains(&self, name: &str) -> bool {
        self.statics.contains_key(name)
    }

    pub fn contains_fn(&self, name: &str) -> bool {
        self.fns.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<NodeType> {
        self.statics.get(name).cloned()
    }

    pub fn get_and_call_fn(&self, fn_name: &str, fn_arg: &str, fn_ctx: NodeTypeFnCtx<'_>) -> Option<Result<NodeType, Box<dyn Error>>> {
        self.fns.get(fn_name).map(|node_type_fn| node_type_fn(fn_arg, fn_ctx))
    }
}