use std::collections::HashMap;
use std::error::Error;
use log::error;
use crate::graph::ir::{NodeInput, NodeTypeData};
use crate::graph::raw::RawComputeFn;
use crate::ir::NodeIOType;
use structural_reflection::RustType;
use derive_more::{Display, Error};

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
}

pub struct NodeTypeFnCtx<'a> {
    pub resolved_rust_types: &'a HashMap<String, RustType>,
    pub input_types: &'a [NodeIOType],
    pub output_types: &'a [NodeIOType]
}

/// The function takes one argument, which may have commas. You can split on the commas and that
/// emulates taking multiple arguments.
pub type NodeTypeFn = Box<dyn Fn(&str, NodeTypeFnCtx<'_>) -> Result<NodeType, Box<dyn Error>> + Send + Sync>;

#[derive(Debug, Display, Error)]
#[display(fmt = "node-type expected no arguments")]
pub struct NodeTypeExpectedNoArguments;

impl NodeTypes {
    pub fn new() -> Self {
        NodeTypes {
            statics: HashMap::new(),
            fns: HashMap::new()
        }
    }

    /// Inserts a static node-type which takes no args.
    ///
    /// Logs an error and overrides if the name already exists. Use [NodeTypes::contains] to check
    pub fn insert(&mut self, name: String, node_type: NodeType) {
        if self.statics.contains_key(&name) || self.fns.contains_key(&name) {
            error!("node type with name {} already inserted", name);
        }
        self.statics.insert(name, node_type);
    }

    /// Inserts a dynamic node-type AKA node-type function which gets computed from `ctx` and takes no args.
    ///
    /// Logs an error and overrides if the name already exists. Use [NodeTypes::contains_fn] to check
    pub fn insert_fn0(&mut self, name: String, node_type_fn: impl Fn(NodeTypeFnCtx<'_>) -> Result<NodeType, Box<dyn Error>> + Send + Sync + 'static) {
        if self.statics.contains_key(&name) || self.fns.contains_key(&name) {
            error!("node type function with name {} already inserted", name);
        }
        self.fns.insert(name, Box::new(move |arg, result| {
            if arg.is_empty() {
                node_type_fn(result)
            } else {
                Err(Box::new(NodeTypeExpectedNoArguments))
            }
        }));
    }

    /// Inserts a dynamic node-type AKA node-type function which gets computed from `ctx` and may take arguments.
    ///
    /// Logs an error and overrides if the name already exists. Use [NodeTypes::contains_fn] to check
    pub fn insert_fn(&mut self, name: String, node_type_fn: impl Fn(&str, NodeTypeFnCtx<'_>) -> Result<NodeType, Box<dyn Error>> + Send + Sync + 'static) {
        if self.statics.contains_key(&name) || self.fns.contains_key(&name) {
            error!("node type with name {} already inserted", name);
        }
        self.fns.insert(name, Box::new(node_type_fn));
    }

    pub fn contains(&self, name: &str) -> bool {
        self.statics.contains_key(name)
    }

    pub fn contains_fn(&self, name: &str) -> bool {
        self.fns.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<NodeType> {
        self.statics.get(name).cloned()
    }

    fn get_and_call_fn(&self, fn_name: &str, fn_arg: &str, fn_ctx: NodeTypeFnCtx<'_>) -> Option<Result<NodeType, Box<dyn Error>>> {
        self.fns.get(fn_name).map(|node_type_fn| node_type_fn(fn_arg, fn_ctx))
    }

    pub fn get_and_call(&self, name: &str, fn_arg: &str, fn_ctx: NodeTypeFnCtx<'_>) -> Option<Result<NodeType, Box<dyn Error>>> {
        self.get(name).map(|result| if fn_arg.is_empty() { Ok(result) } else { Err(Box::new(NodeTypeExpectedNoArguments) as Box<dyn Error>) })
            .or_else(|| self.get_and_call_fn(name, fn_arg, fn_ctx))
    }
}