use std::borrow::Cow;
use std::iter::zip;
use std::mem::take;
use crate::error::GraphFormError;
use crate::ir::from_ast::{ForwardNode, GraphBuilder};
use crate::ir::{FieldHeader, Node, NodeId, NodeInput, NodeIOType, NodeMetadata, NodeTypeData, NodeTypeName};
use crate::node_types::{NodeType, NodeTypeFnCtx};
use crate::ast::types::{AstField, AstFieldElem, AstFieldHeader, AstNode, AstNodePos};
use crate::raw::RawComputeFn;

impl<'a> GraphBuilder<'a> {
    pub(super) fn forward_resolved_node(&self, node_name: &str) -> Option<(NodeId, &ForwardNode)> {
        self.forward_resolved_nodes.get(node_name).map(|(id, node)| (*id, node))
    }

    pub(super) fn resolved_node_type(&self, node_name: &str) -> Option<&NodeTypeData> {
        self.resolved_nodes.get(node_name).map(|(_, node)| {
            self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type")
        })
    }

    pub(super) fn resolved_node_and_type(&self, node_name: &str) -> Option<(NodeId, &NodeTypeData, &Node)> {
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

    pub(super) fn resolve_node(&mut self, node_name: &str, node: AstNode) -> (NodeTypeData, Node) {
        let mut pos = None;
        let (input_types, mut inputs, input_headers) = self.resolve_field_elems(node.input_fields, node_name, &mut pos, false);
        let (output_types, outputs, output_headers) = self.resolve_field_elems(node.output_fields, node_name, &mut pos, true);

        let inherited_type = node.node_type.as_ref()
            .and_then(|node_type| self.resolve_node_type(node_type, node_name, &input_types, &output_types));

        let output_idxs_with_values = outputs.iter().enumerate().filter(|(_, output_value)| !matches!(output_value, NodeInput::Hole)).map(|(idx, _)| idx).collect::<Vec<_>>();
        for output_idx in output_idxs_with_values {
            let output_name = output_types[output_idx].name.clone();
            self.errors.push(GraphFormError::OutputHasValue {
                node_name: node_name.to_string(),
                output_name
            });
        }

        let (node_type_name, type_data, compute) = match inherited_type {
            None => {
                // Use structural self-type
                let self_type_data = NodeTypeData {
                    inputs: input_types,
                    outputs: output_types
                };
                (NodeTypeName::from(node_name.to_string()), self_type_data, RawComputeFn::panicking())
            },
            Some(inherited_type) => {
                // Rearrange inputs and fill with holes, to match inherited type (there are no outputs)
                let mut old_inputs = Vec::new();
                old_inputs.append(&mut inputs);
                for input_io_type in inherited_type.type_data.inputs.iter() {
                    let input_field_name = &input_io_type.name;
                    let input_idx = input_types.iter().position(|input_type| &input_type.name == input_field_name);

                    inputs.push(match input_idx {
                        None => NodeInput::Hole,
                        Some(input_idx) => take(&mut old_inputs[input_idx])
                    });
                }

                // Add default inputs which were not overridden
                for (input, default_input) in zip(inputs.iter_mut(), inherited_type.default_inputs.iter()) {
                    if matches!(input, NodeInput::Hole) {
                        // Add this default input since it's not overridden
                        *input = default_input.clone();
                    }
                }

                // Use inherited type
                let inherited_type_name = NodeTypeName::from(node.node_type.unwrap());
                let inherited_type_data = inherited_type.type_data.clone();
                (inherited_type_name, inherited_type_data, inherited_type.compute.clone())
            }
        };

        let meta = NodeMetadata {
            node_name: node_name.to_string(),
            pos,
            input_headers,
            output_headers
        };

        let node = Node {
            type_name: node_type_name,
            inputs,
            compute,
            meta
        };

        (type_data, node)
    }

    fn resolve_node_type(&mut self, type_name: &str, node_name: &str, input_types: &[NodeIOType], output_types: &[NodeIOType]) -> Option<NodeType> {
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

    fn _resolve_node_type(&mut self, type_name: &str, fn_arg: &str, node_name: &str,  input_types: &[NodeIOType], output_types: &[NodeIOType]) -> Option<NodeType> {
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
        pos: &mut Option<AstNodePos>,
        is_output: bool
    ) -> (Vec<NodeIOType>, Vec<NodeInput>, Vec<FieldHeader>) {
        let mut types: Vec<NodeIOType> = Vec::new();
        let mut inputs: Vec<NodeInput> = Vec::new();
        let mut headers: Vec<FieldHeader> = Vec::new();
        for (index, field) in fields.into_iter().enumerate() {
            match field {
                AstFieldElem::Header { header } => {
                    // Extract information from header
                    match &header {
                        AstFieldHeader::Pos(new_pos) if pos.is_none() => *pos = Some(*new_pos),
                        _ => {}
                    }
                    // Add header even if we extract so we can losslessly reconstruct
                    headers.push(FieldHeader { index, header });
                }
                AstFieldElem::Field { field } => {
                    let (input_type, input) = self.resolve_node_field(field, node_name, is_output);
                    types.push(input_type);
                    inputs.push(input);
                }
            }
        }
        (types, inputs, headers)
    }

    fn resolve_node_field(&mut self, field: AstField, node_name: &str, is_output_field: bool) -> (NodeIOType, NodeInput) {
        let rust_type = self.resolve_type(
            field.rust_type,
            (field.value.as_ref(), &field.value_children),
        );
        let value = self.resolve_value(
            (field.value, field.value_children),
            Cow::Borrowed(&rust_type),
            node_name,
            &field.name,
            is_output_field
        );
        let io_type = NodeIOType {
            name: field.name,
            rust_type,
            rust_type_may_be_null: field.rust_type_may_be_null,
        };
        (io_type, value)
    }

}