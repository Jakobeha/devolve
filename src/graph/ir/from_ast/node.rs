use std::borrow::Cow;
use std::iter::zip;
use std::mem::take;
use crate::error::GraphFormError;
use crate::ir::from_ast::{ForwardNode, GraphBuilder};
use crate::ir::{FieldHeader, Node, NodeId, NodeIO, NodeIOType, NodeMetadata, NodeTypeData, NodeTypeName};
use crate::graph::raw::{NodeType, NodeTypeFnCtx};
use crate::ast::types::{AstField, AstFieldElem, AstFieldHeader, AstNode, AstNodeAttr, NodeColor, NodePos};
use crate::raw::NullRegion;

impl<'a, RuntimeCtx: ?Sized> GraphBuilder<'a, RuntimeCtx> {
    pub(super) fn forward_resolved_node(&self, node_name: &str) -> Option<(NodeId, &ForwardNode)> {
        self.forward_resolved_nodes.get(node_name).map(|(id, node)| (*id, node))
    }

    pub(super) fn resolved_node_type(&self, node_name: &str) -> Option<&NodeTypeData> {
        self.resolved_nodes.get(node_name).map(|(_, node)| {
            self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type")
        })
    }

    pub(super) fn resolved_node_and_type(&self, node_name: &str) -> Option<(NodeId, &NodeTypeData, &Node<RuntimeCtx>)> {
        self.resolved_nodes.get(node_name).map(|(node_id, node)| {
            let node_type = self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type");
            (*node_id, node_type, node)
        })
    }

    pub(super) fn forward_resolve_node(&mut self, node: &AstNode) -> ForwardNode {
        let input_field_names = if node.node_type.is_none() {
            Some(self.forward_resolve_field_names(&node.input_fields))
        } else {
            // Determining field names is too complex, since we need to resolve the type, which requires field types, which we don't have
            None
        };

        ForwardNode {
            input_field_names
        }
    }

    pub(super) fn resolve_node(&mut self, node_name: &str, node_id: NodeId, node: AstNode) -> (NodeTypeData, Node<RuntimeCtx>) {
        let (node_id_, _) = self.forward_resolved_nodes.remove(node_name).expect("node not forward resolved");
        debug_assert!(node_id == node_id_, "sanity check failed");

        // Get input / output fields and metadata
        let mut pos = None;
        let mut color = None;
        let (input_types, mut inputs, input_headers) = self.resolve_field_elems(node.input_fields, node_name, &mut pos, &mut color);
        let (output_types, mut outputs, output_headers) = self.resolve_field_elems(node.output_fields, node_name, &mut pos, &mut color);

        // Get and resolve inherited type (includes computing type fn)
        let inherited_type = node.node_type.as_ref()
            .and_then(|node_type| self.resolve_node_type(node_type, node_name, &input_types, &output_types));

        // Get node type name and data, fill in more inputs / outputs with default values
        // and reorder inputs and outputs if we have an inherited type with different order
        let self_type_data = NodeTypeData {
            inputs: input_types,
            outputs: output_types
        };
        let (node_type_name, type_data, compute) = match inherited_type {
            None => {
                // Use structural self-type, no reordering or defaults necessary
                (NodeTypeName::from(node_name.to_string()), self_type_data, None)
            },
            Some(inherited_type) => {
                // Reorder inputs and outputs to match inherited type, then add defaults which were not overridden
                self.apply_inherited_to_fields(
                    &inherited_type.type_data.inputs,
                    &inherited_type.default_inputs,
                    &self_type_data.inputs,
                    &mut inputs
                );
                self.apply_inherited_to_fields(
                    &inherited_type.type_data.outputs,
                    &inherited_type.default_default_outputs,
                    &self_type_data.outputs,
                    &mut outputs
                );

                // Use inherited type
                let inherited_type_name = NodeTypeName::from(node.node_type.unwrap());
                let mut inherited_type_data = inherited_type.type_data.clone();
                self.merge_inherited_type_data(node_name, &mut inherited_type_data, self_type_data);
                (inherited_type_name, inherited_type_data, Some(inherited_type.compute.clone()))
            }
        };

        // Put together and return
        let meta = NodeMetadata {
            node_name: node_name.to_string(),
            pos,
            primary_color: color,
            input_headers,
            output_headers
        };

        let node = Node {
            type_name: node_type_name,
            inputs,
            default_outputs: outputs,
            compute,
            meta
        };

        (type_data, node)
    }

    fn resolve_node_type(&mut self, type_name: &str, node_name: &str, input_types: &[NodeIOType], output_types: &[NodeIOType]) -> Option<NodeType<RuntimeCtx>> {
        if let Some((fn_name, fn_arg)) = type_name.split_once('(') {
            let fn_arg = fn_arg.strip_suffix(')').unwrap_or_else(|| {
                self.errors.push(GraphFormError::NodeTypeFunctionMissingRParen { node_name: node_name.to_string() });
                fn_arg
            });
            self._resolve_node_type(fn_name, fn_arg, node_name, input_types, output_types)
        } else {
            self._resolve_node_type(type_name, "", node_name, input_types, output_types)
        }
    }

    fn _resolve_node_type(&mut self, type_name: &str, fn_arg: &str, node_name: &str,  input_types: &[NodeIOType], output_types: &[NodeIOType]) -> Option<NodeType<RuntimeCtx>> {
        match self.resolved_rust_types.get(type_name) {
            None => match self.ctx.node_types.get_and_call(type_name, fn_arg, self.node_type_fn_ctx(input_types, output_types)) {
                None => {
                    self.errors.push(GraphFormError::NodeTypeNotFound {
                        type_name: type_name.to_string(),
                        node_name: node_name.to_string()
                    });
                    None
                },
                Some(Err(error)) => {
                    self.errors.push(GraphFormError::NodeTypeFunctionError {
                        error,
                        node_name: node_name.to_string()
                    });
                    None
                },
                Some(Ok(node_type)) => Some(node_type)
            }
            Some(_) => {
                self.errors.push(GraphFormError::NodeIsDataType {
                    type_name: type_name.to_string(),
                    node_name: node_name.to_string()
                });
                None
            },
        }
    }

    /// Add extra self-type data to inherited type data. Specifically: missing size for slices
    fn merge_inherited_type_data(&mut self, node_name: &str, inherited_type_data: &mut NodeTypeData, self_type_data: NodeTypeData) {
        self.merge_inherited_types(node_name, &mut inherited_type_data.inputs, self_type_data.inputs);
        self.merge_inherited_types(node_name, &mut inherited_type_data.outputs, self_type_data.outputs);
    }

    fn merge_inherited_types(&mut self, node_name: &str, inherited_types: &mut [NodeIOType], mut self_types: Vec<NodeIOType>) {
        for inherited_type in inherited_types {
            if let Some(self_type) = self_types.drain_filter(|self_type| self_type.name == inherited_type.name).next() {
                let field_name = self_type.name;
                self.merge_resolved_type2(
                    node_name,
                    &field_name,
                    &mut inherited_type.rust_type,
                    self_type.rust_type
                );
            }
        }
    }

    fn node_type_fn_ctx<'b>(&'b self, input_types: &'b [NodeIOType], output_types: &'b [NodeIOType]) -> NodeTypeFnCtx<'b> {
        NodeTypeFnCtx {
            resolved_rust_types: &self.resolved_rust_types,
            input_types,
            output_types
        }
    }

    fn forward_resolve_field_names(&mut self, fields: &[AstFieldElem]) -> Vec<String> {
        fields.into_iter().filter_map(|field| match field {
            AstFieldElem::Field { field } => Some(field.name.clone()),
            AstFieldElem::Header { .. } => None
        }).collect()
    }

    fn resolve_field_elems(
        &mut self,
        fields: Vec<AstFieldElem>,
        node_name: &str,
        pos: &mut Option<NodePos>,
        color: &mut Option<NodeColor>
    ) -> (Vec<NodeIOType>, Vec<NodeIO>, Vec<FieldHeader>) {
        let mut types: Vec<NodeIOType> = Vec::new();
        let mut inputs: Vec<NodeIO> = Vec::new();
        let mut headers: Vec<FieldHeader> = Vec::new();
        for (index, field) in fields.into_iter().enumerate() {
            match field {
                AstFieldElem::Header { header } => {
                    // Extract information from header
                    match &header {
                        AstFieldHeader::Message(_) => {}
                        AstFieldHeader::NodeAttr(attr) => match attr {
                            AstNodeAttr::Pos(new_pos) => {
                                if pos.is_none() {
                                    *pos = Some(*new_pos)
                                } else {
                                    self.errors.push(GraphFormError::NodeMetaMultiplePos {
                                        node_name: node_name.to_string()
                                    });
                                }
                            },
                            AstNodeAttr::PrimaryColor(new_color) => {
                                if color.is_none() {
                                    *color = Some(*new_color)
                                } else {
                                    self.errors.push(GraphFormError::NodeMetaMultipleColor {
                                        node_name: node_name.to_string()
                                    });
                                }
                            }
                        }
                    }
                    // Add header even if we extract so we can losslessly reconstruct
                    headers.push(FieldHeader { index, header });
                }
                AstFieldElem::Field { field } => {
                    let (input_type, input) = self.resolve_node_field(field, node_name);
                    types.push(input_type);
                    inputs.push(input);
                }
            }
        }
        (types, inputs, headers)
    }

    fn resolve_node_field(&mut self, field: AstField, node_name: &str) -> (NodeIOType, NodeIO) {
        let rust_type = self.resolve_type(
            field.rust_type,
            (field.value_head.as_ref(), &field.value_children),
        );
        let value = self.resolve_value(
            (field.value_head, field.value_children),
            Cow::Borrowed(&rust_type),
            node_name,
            &field.name
        );
        let io_type = NodeIOType {
            name: field.name,
            rust_type,
            null_region: if field.rust_type_may_be_null { NullRegion::Null } else { NullRegion::NonNull }
        };
        (io_type, value)
    }

    fn apply_inherited_to_fields(&mut self, inherited_field_types: &[NodeIOType], inherited_field_defaults: &[NodeIO], field_types: &[NodeIOType], fields: &mut Vec<NodeIO>) {
        let mut old_fields = Vec::new();
        old_fields.append(fields);
        for field_io_type in inherited_field_types {
            let field_field_name = &field_io_type.name;
            let field_idx = field_types.iter().position(|field_type| &field_type.name == field_field_name);

            fields.push(match field_idx {
                None => NodeIO::Hole,
                Some(field_idx) => take(&mut old_fields[field_idx])
            });
        }

        // Add default fields which were not overridden
        for (field, default_field) in zip(fields, inherited_field_defaults) {
            if matches!(field, NodeIO::Hole) {
                // Add this default field since it's not overridden
                *field = default_field.clone();
            }
        }
    }
}