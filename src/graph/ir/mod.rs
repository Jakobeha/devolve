use std::iter::empty;
use std::ops::{Index, IndexMut};

pub use ctx::*;
pub use types::*;

use crate::graph::error::GraphFormErrors;
use crate::graph::ir::from_ast::GraphBuilder;
use crate::graph::ir::serialize::GraphSerializer;
use crate::graph::ast::types::AstGraph;
use crate::misc::try_index::{NotFound, TryIndex, TryIndexMut};
use structural_reflection::TypeStructure;

mod query_mutate;
mod types;
mod ctx;
mod from_ast;
mod serialize;

// region serialization / deserialization
impl<'a> TryFrom<(AstGraph, &'a ComptimeCtx)> for IrGraph {
    type Error = GraphFormErrors;

    fn try_from((graph, ctx): (AstGraph, &'a ComptimeCtx)) -> Result<Self, Self::Error> {
        let mut errors = GraphFormErrors::new();
        let graph = GraphBuilder::build(graph, ctx, &mut errors);

        if errors.is_empty() {
            Ok(graph)
        } else {
            Err(errors)
        }
    }
}

impl<'a> Into<AstGraph> for (IrGraph, &'a ComptimeCtx) {
    fn into(self) -> AstGraph {
        GraphSerializer::serialize(self.0, self.1, empty())
    }
}

impl<'a> Into<AstGraph> for (IrGraph, &'a ComptimeCtx, &'a [(String, TypeStructure)]) {
    fn into(self) -> AstGraph {
        GraphSerializer::serialize(self.0, self.1, self.2.into_iter())
    }
}
// endregion

// region index boilerplate
impl TryIndex<NodeId> for IrGraph {
    type Output = Node;

    fn try_index(&self, index: NodeId) -> Result<&Self::Output, NotFound<NodeId>> {
        self.nodes.get(index.0).ok_or(NotFound { index })
    }
}

impl TryIndexMut<NodeId> for IrGraph {
    fn try_index_mut(&mut self, index: NodeId) -> Result<&mut Self::Output, NotFound<NodeId>> {
        self.nodes.get_mut(index.0).ok_or(NotFound { index })
    }
}

impl Index<NodeId> for IrGraph {
    type Output = Node;

    fn index(&self, index: NodeId) -> &Self::Output {
        self.nodes.index(index.0)
    }
}

impl IndexMut<NodeId> for IrGraph {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.nodes.index_mut(index.0)
    }
}

impl<'a> TryIndex<&'a NodeTypeName> for IrGraph {
    type Output = NodeTypeData;

    fn try_index(&self, index: &'a NodeTypeName) -> Result<&Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index(index)
    }
}

impl<'a> TryIndexMut<&'a NodeTypeName> for IrGraph {
    fn try_index_mut(&mut self, index: &'a NodeTypeName) -> Result<&mut Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index_mut(index)
    }
}

impl<'a> Index<&'a NodeTypeName> for IrGraph {
    type Output = NodeTypeData;

    fn index(&self, index: &'a NodeTypeName) -> &Self::Output {
        self.types.index(index)
    }
}

impl<'a> IndexMut<&'a NodeTypeName> for IrGraph {
    fn index_mut(&mut self, index: &'a NodeTypeName) -> &mut Self::Output {
        self.types.get_mut(index).expect("index_mut: key not found")
    }
}
// endregion
