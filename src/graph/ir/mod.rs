use std::iter::empty;
use std::ops::{Index, IndexMut};

pub use ctx::*;
pub use display::*;
pub use types::*;

use crate::graph::error::GraphFormErrors;
use crate::graph::ir::from_ast::GraphBuilder;
use crate::graph::ir::serialize::GraphSerializer;
use crate::graph::ast::types::AstGraph;
use crate::misc::try_index::{NotFound, TryIndex, TryIndexMut};
use structural_reflection::TypeStructure;

/// IR datatype traversal
mod query_mutate;
/// IR datatypes
mod types;
/// Pretty printing for diagnostics
mod display;
/// Compile-time context (small module)
mod ctx;
/// Ast to IR
mod from_ast;
/// IR to Ast
mod serialize;

// region serialization / deserialization
impl<'a, RuntimeCtx: ?Sized> TryFrom<(AstGraph, &'a ComptimeCtx<RuntimeCtx>)> for IrGraph<RuntimeCtx> {
    type Error = GraphFormErrors;

    fn try_from((graph, ctx): (AstGraph, &'a ComptimeCtx<RuntimeCtx>)) -> Result<Self, Self::Error> {
        let mut errors = GraphFormErrors::new();
        let graph = GraphBuilder::build(graph, ctx, &mut errors);

        if errors.is_empty() {
            Ok(graph)
        } else {
            Err(errors)
        }
    }
}

impl<'a, RuntimeCtx: ?Sized> Into<AstGraph> for (IrGraph<RuntimeCtx>, &'a ComptimeCtx<RuntimeCtx>) {
    fn into(self) -> AstGraph {
        GraphSerializer::serialize(self.0, self.1, empty())
    }
}

impl<'a, RuntimeCtx: ?Sized> Into<AstGraph> for (IrGraph<RuntimeCtx>, &'a ComptimeCtx<RuntimeCtx>, &'a [(String, TypeStructure)]) {
    fn into(self) -> AstGraph {
        GraphSerializer::serialize(self.0, self.1, self.2.into_iter())
    }
}
// endregion

// region index boilerplate
impl<RuntimeCtx: 'static + ?Sized> TryIndex<NodeId> for IrGraph<RuntimeCtx> {
    type Output = Node<RuntimeCtx>;

    fn try_index(&self, index: NodeId) -> Result<&Self::Output, NotFound<NodeId>> {
        self.nodes.get(index.0).ok_or(NotFound { index })
    }
}

impl<RuntimeCtx: 'static + ?Sized> TryIndexMut<NodeId> for IrGraph<RuntimeCtx> {
    fn try_index_mut(&mut self, index: NodeId) -> Result<&mut Self::Output, NotFound<NodeId>> {
        self.nodes.get_mut(index.0).ok_or(NotFound { index })
    }
}

impl<RuntimeCtx: 'static + ?Sized> Index<NodeId> for IrGraph<RuntimeCtx> {
    type Output = Node<RuntimeCtx>;

    fn index(&self, index: NodeId) -> &Self::Output {
        self.nodes.index(index.0)
    }
}

impl<RuntimeCtx: 'static + ?Sized> IndexMut<NodeId> for IrGraph<RuntimeCtx> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        self.nodes.index_mut(index.0)
    }
}

impl<'a, RuntimeCtx: ?Sized> TryIndex<&'a NodeTypeName> for IrGraph<RuntimeCtx> {
    type Output = NodeTypeData;

    fn try_index(&self, index: &'a NodeTypeName) -> Result<&Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index(index)
    }
}

impl<'a, RuntimeCtx: ?Sized> TryIndexMut<&'a NodeTypeName> for IrGraph<RuntimeCtx> {
    fn try_index_mut(&mut self, index: &'a NodeTypeName) -> Result<&mut Self::Output, NotFound<&'a NodeTypeName>> {
        self.types.try_index_mut(index)
    }
}

impl<'a, RuntimeCtx: ?Sized> Index<&'a NodeTypeName> for IrGraph<RuntimeCtx> {
    type Output = NodeTypeData;

    fn index(&self, index: &'a NodeTypeName) -> &Self::Output {
        self.types.index(index)
    }
}

impl<'a, RuntimeCtx: ?Sized> IndexMut<&'a NodeTypeName> for IrGraph<RuntimeCtx> {
    fn index_mut(&mut self, index: &'a NodeTypeName) -> &mut Self::Output {
        self.types.get_mut(index).expect("index_mut: key not found")
    }
}
// endregion
