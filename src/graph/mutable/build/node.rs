use std::borrow::Cow;
use std::iter::zip;
use std::mem::take;
use crate::error::GraphFormError;
use crate::mutable::build::GraphBuilder;
use crate::mutable::{FieldHeader, Node, NodeId, NodeInput, NodeIOType, NodeMetadata, NodeTypeData, NodeTypeName};
use crate::node_types::{NodeType, NodeTypeFnCtx};
use crate::parse::types::{SerialField, SerialFieldElem, SerialNode};
use crate::raw::RawComputeFn;

impl<'a> GraphBuilder<'a> {
    pub(super) fn resolved_node_type(&self, node_name: &str) -> Option<&NodeTypeData> {
        self.resolved_nodes.get(node_name).map(|(_, node)| {
            self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type")
        })
    }

    pub(super) fn resolved_node_and_type(&self, node_name: &str) -> Option<(&NodeId, &NodeTypeData, &Node)> {
        self.resolved_nodes.get(node_name).map(|(node_id, node)| {
            let node_type = self.resolved_node_types.get(&node.type_name).expect("resolved node missing its type");
            (node_id, node_type, node)
        })
    }

    pub(super) fn resolve_node(&mut self, node_name: &str, node: SerialNode) -> (NodeTypeData, Node) {
        let inherited_type = node.node_type.as_ref()
            .and_then(|node_type| self.resolve_node_type(node_type, node_name));

        let (defined_input_types, defined_inputs, input_headers) = self.resolve_field_elems(node.input_fields, node_name);
        let (defined_output_types, defined_outputs, output_headers) = self.resolve_field_elems(node.output_fields, node_name);

        let output_idxs_with_values = defined_outputs.iter().enumerate().filter(|(_, output_value)| !matches!(output_value, NodeInput::Hole)).map(|(idx, _)| idx).collect::<Vec<_>>();
        for output_idx in output_idxs_with_values {
            let output_name = defined_output_types[output_idx].name.clone();
            self.errors.push(GraphFormError::OutputHasValue {
                node_name: node_name.to_string(),
                output_name
            });
        }

        let mut inputs = defined_inputs;

        let (node_type_name, type_data, compute) = match inherited_type {
            None => {
                // Use structural self-type
                let self_type_data = NodeTypeData {
                    inputs: defined_input_types,
                    outputs: defined_output_types
                };
                (NodeTypeName::from(node_name.to_string()), self_type_data, RawComputeFn::panicking())
            },
            Some(inherited_type) => {
                // Rearrange inputs and fill with holes, to match inherited type (there are no outputs)
                let mut old_inputs = Vec::new();
                old_inputs.append(&mut inputs);
                for input_io_type in inherited_type.type_data.inputs.iter() {
                    let input_field_name = &input_io_type.name;
                    let input_idx = defined_input_types.iter().position(|input_type| &input_type.name == input_field_name);

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

    fn resolve_node_type(&mut self, type_name: &str, node_name: &str) -> Option<NodeType> {
        if let Some((fn_name, fn_arg)) = type_name.split_once('(') {
            let fn_arg = fn_arg.strip_suffix(')').unwrap_or_else(|| {
                self.errors.push(GraphFormError::NodeTypeFunctionMissingRParen { node_name: node_name.to_string() });
                fn_arg
            });
            self.resolve_node_type_fn(fn_name, fn_arg, node_name)
        } else {
            self.resolve_node_type_const(type_name, node_name)
        }
    }

    fn resolve_node_type_const(&mut self, type_name: &str, node_name: &str) -> Option<NodeType> {
        match self.resolved_rust_types.get(type_name) {
            None => match self.ctx.node_types.get(type_name) {
                None => {
                    self.errors.push(GraphFormError::NodeTypeNotFound {
                        type_name: type_name.to_string(),
                        node_name: node_name.to_string()
                    });
                    None
                },
                Some(node_type) => Some(node_type)
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

    fn resolve_node_type_fn(&mut self, fn_name: &str, fn_arg: &str, node_name: &str) -> Option<NodeType> {
        match self.ctx.node_types.get_and_call_fn(fn_name, fn_arg, self.node_type_fn_ctx()) {
            None => {
                self.errors.push(GraphFormError::NodeTypeFunctionNotFound {
                    type_fn_name: fn_name.to_string(),
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
    }

    fn node_type_fn_ctx(&self) -> NodeTypeFnCtx<'_> {
        NodeTypeFnCtx {
            resolved_rust_types: &self.resolved_rust_types
        }
    }

    fn resolve_field_elems(&mut self, fields: Vec<SerialFieldElem>, node_name: &str) -> (Vec<NodeIOType>, Vec<NodeInput>, Vec<FieldHeader>) {
        let mut defined_types: Vec<NodeIOType> = Vec::new();
        let mut defined_values: Vec<NodeInput> = Vec::new();
        let mut headers: Vec<FieldHeader> = Vec::new();
        for (index, field) in fields.into_iter().enumerate() {
            match field {
                SerialFieldElem::Header { header } => {
                    headers.push(FieldHeader { index, header });
                }
                SerialFieldElem::Field(field) => {
                    let (defined_input_type, defined_input) = self.resolve_node_field(field, node_name);
                    defined_types.push(defined_input_type);
                    defined_values.push(defined_input);
                }
            }
        }
        (defined_types, defined_values, headers)
    }

    fn resolve_node_field(&mut self, field: SerialField, node_name: &str) -> (NodeIOType, NodeInput) {
        let rust_type = self.resolve_type(
            field.rust_type,
            (field.value.as_ref(), &field.value_children),
        );
        let value = self.resolve_value((field.value, field.value_children), Cow::Borrowed(&rust_type), node_name, &field.name);
        let io_type = NodeIOType {
            name: field.name,
            rust_type
        };
        (io_type, value)
    }

}