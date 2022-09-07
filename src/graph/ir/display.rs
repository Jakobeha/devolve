use crate::ir::{Node, NodeId, NodeTypeName};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct NodeDisplay {
    pub node_id: NodeId,
    pub type_name: NodeTypeName,
}

impl Node {
    pub fn display(&self, my_id: NodeId) -> NodeDisplay {
        NodeDisplay {
            node_id: my_id,
            type_name: self.type_name.clone(),
        }
    }
}

impl Display for NodeDisplay {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: better display
        write!(f, "{}({})", self.node_id, self.type_name)
    }
}

impl Display for NodeTypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl Display for NodeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}