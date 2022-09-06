use std::cmp::Ordering;
use std::collections::HashSet;

use crate::error::{GraphValidationError, GraphValidationErrors, NodeCycle};
use crate::ir::{IrGraph, Node, NodeId, NodeInput, NodeInputDep};
use structural_reflection::RustType;

impl IrGraph {
    pub fn insert_node(&mut self, node: Node) -> NodeId {
        NodeId(self.nodes.insert(node))
    }

    pub fn delete_node(&mut self, id: NodeId) {
        self.nodes.remove(id.0);
    }

    pub fn validate(&self) -> GraphValidationErrors {
        let mut errors = Vec::new();

        if let Err(cycle) = self.check_cycle() {
            errors.push(GraphValidationError::Cycle(cycle))
        }

        todo!("\
        Check that input and output types match\
        Also check that there are no partially-filled inputs in required regions or output\
        ")

        // errors
    }

    pub fn iter_node_ids(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.nodes.iter().map(|(id, _)| NodeId(id))
    }

    pub fn iter_nodes(&self) -> impl Iterator<Item=(NodeId, &Node)> {
        self.nodes.iter().map(|(id, node)| (NodeId(id), node))
    }

    pub fn iter_mut_nodes(&mut self) -> impl Iterator<Item=(NodeId, &mut Node)> {
        self.nodes.iter_mut().map(|(id, node)| (NodeId(id), node))
    }

    /// Doesn't iterate nested types. If you want to get type names you don't need to iterate nested
    /// rust types or structures, because any type names are only in the surface types
    pub fn iter_rust_types(&self) -> impl Iterator<Item=&RustType> {
        (self.input_types.iter().chain(self.output_types.iter()))
            .map(|io_type| &io_type.rust_type)
    }

    fn check_cycle(&self) -> Result<(), NodeCycle> {
        let mut not_in_cycle = HashSet::<NodeId>::new();
        for node_id in self.iter_node_ids() {
            // Depth-first search
            let mut visited = HashSet::<NodeId>::new();
            for elem in &not_in_cycle {
                visited.insert(*elem);
            }

            let mut current_chain = Vec::new();
            let mut recurse_stack = Vec::new();
            current_chain.push(node_id);
            recurse_stack.push(0);
            while let Some(next_id) = current_chain.pop() {
                let next_node = &self[next_id];
                let recurse_idx = recurse_stack.pop().unwrap();

                let mut remaining_deps = next_node.iter_dep_nodes().skip(recurse_idx);
                if let Some(next_dep) = remaining_deps.next() {
                    if next_dep == node_id {
                        current_chain.push(node_id);
                        return Err(NodeCycle(current_chain));
                    } else if !visited.contains(&next_dep) {
                        current_chain.push(next_id);
                        recurse_stack.push(recurse_idx + 1);
                    }
                } else {
                    visited.insert(next_id);
                }
            }

            not_in_cycle.insert(node_id);
        }

        Ok(())
    }
}

impl Node {
    pub fn depends_on(&self, other_id: NodeId) -> bool {
        self.iter_dep_nodes().any(|dep| dep == other_id)
    }

    pub fn iter_deps(&self) -> impl Iterator<Item=NodeInputDep> + '_ {
        self.inputs.iter().flat_map(|input| input.deps())
    }

    pub fn iter_dep_nodes(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.inputs.iter().flat_map(|input| input.dep_nodes())
    }

    pub fn sort_by_deps(nodes: &mut [(NodeId, Self)]) {
        nodes.sort_by(|(a_id, a), (b_id, b)| {
            match (a.depends_on(*b_id), b.depends_on(*a_id)) {
                (true, true) => unreachable!("should've been detected in cycle"),
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => Ordering::Equal
            }
        })
    }
}

impl NodeInput {
    pub const ZST: NodeInput = NodeInput::Tuple(Vec::new());

    pub fn deps(&self) -> impl Iterator<Item=NodeInputDep> + '_ {
        match &self {
            NodeInput::Hole | NodeInput::Const(_) => Box::new(std::iter::empty()) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Dep(dep) => Box::new(std::iter::once(*dep)) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Array(inputs) => Box::new(inputs.iter().flat_map(|input| input.deps())) as Box<dyn Iterator<Item=NodeInputDep>>,
            NodeInput::Tuple(inputs_with_layouts) => Box::new(inputs_with_layouts.iter().flat_map(|input_with_layout| input_with_layout.input.deps())) as Box<dyn Iterator<Item=NodeInputDep>>
        }
    }

    pub fn dep_nodes(&self) -> impl Iterator<Item=NodeId> + '_ {
        self.deps().filter_map(|dep| match dep {
            NodeInputDep::GraphInput { idx: _ } => None,
            NodeInputDep::OtherNodeOutput { id, idx: _ } => Some(id)
        })
    }
}