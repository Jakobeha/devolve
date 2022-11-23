use structural_reflection::Qualifier;
use crate::graph::raw::NodeTypes;

/// Compile-time context
pub struct ComptimeCtx<RuntimeCtx: 'static + ?Sized> {
    /// Qualifier for the current module: this will qualify all items defined in this module
    pub qualifier: Qualifier,
    /// Node types
    pub node_types: NodeTypes<RuntimeCtx>
}