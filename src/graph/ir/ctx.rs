use crate::node_types::NodeTypes;

pub struct ComptimeCtx<RuntimeCtx> {
    /// Qualifiers for the current module: this will qualify all node defs in this module
    pub qualifiers: Vec<String>,
    /// Node types
    pub node_types: NodeTypes<RuntimeCtx>
}