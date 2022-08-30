#![feature(decl_macro)]
#![feature(is_some_with)]

mod misc;
mod batch_file;

use std::error::Error;
use dui_graph::mutable::{ComptimeCtx, MutableGraph, NodeInput, NodeIOType, NodeTypeData};
use dui_graph::node_types::{NodeType, NodeTypes};
use dui_graph::parse::types::SerialGraph;
use dui_graph::raw::{RawComputeFn, UsedRegion};
use structural_reflection::c_tuple::CTuple2;
use structural_reflection::RustType;
use structural_reflection::derive::{HasTypeName, HasStructure};
use dui_graph::StaticStrs;
use crate::batch_file::{RunTest, RunTestsOnFiles};
use crate::misc::{assert_eq_multiline, ErrorNodes, try_or_return};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
#[repr(transparent)]
pub struct ViewId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
#[repr(C)]
pub struct CopyRange<Idx> {
    pub start: Idx,
    pub end: Idx
}

#[test]
fn test_parse_serial() {
    ErrorNodes::log_errors_and_panic(|errors| {
        RunTestsOnFiles {
            dir_name: "duis",
            try_parse: |input_string, path| SerialGraph::parse_from_input(&input_string, path).map_err(|err| Box::new(err) as Box<dyn Error + 'static>),
            tests: &[
                RunTest {
                    test_name: "round-trip",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files| {
                        let input_string = input.to_string();
                        let input2 = try_or_return!(
                            SerialGraph::parse_from_input(&input_string, input_path),
                            errors, "couldn't re-parse input:\n{}", input_string
                        );
                        let input2_string = input2.to_string();
                        assert_eq_multiline!(input_string, input2_string, errors, "round trip failed");
                    }
                },
                RunTest {
                    test_name: "ir",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files| {
                        let mut node_types = NodeTypes::new();
                        node_types.insert(String::from("Button"), NodeType {
                            compute: RawComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO Button");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "text".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                    },
                                    NodeIOType {
                                        name: "is_enabled".to_string(),
                                        rust_type: RustType::of::<bool>(),
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>()
                                    },
                                    NodeIOType {
                                        name: String::from("click"),
                                        rust_type: RustType::of::<()>()
                                    }
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::const_(false)
                            ],
                            required_inputs: vec![
                                UsedRegion::Used,
                                UsedRegion::Used
                            ]
                        });
                        node_types.insert(String::from("TextField"), NodeType {
                            compute: RawComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO TextField");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "text".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                    },
                                    NodeIOType {
                                        name: "placeholder".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>()
                                    },
                                    NodeIOType {
                                        name: String::from("text"),
                                        rust_type: RustType::of::<&str>()
                                    },
                                    NodeIOType {
                                        name: String::from("text_modified"),
                                        rust_type: RustType::of::<CTuple2<CopyRange<usize>, &str>>()
                                    },
                                    NodeIOType {
                                        name: String::from("enter_key"),
                                        rust_type: RustType::of::<()>()
                                    }
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::const_("")
                            ],
                            required_inputs: vec![
                                UsedRegion::Used,
                                UsedRegion::Used
                            ]
                        });
                        node_types.insert(String::from("Box"), NodeType {
                            compute: RawComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO Box");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "children".to_string(),
                                        rust_type: RustType::of::<&[ViewId]>(),
                                    },
                                    NodeIOType {
                                        name: "width".to_string(),
                                        rust_type: RustType::of::<usize>()
                                    },
                                    NodeIOType {
                                        name: "height".to_string(),
                                        rust_type: RustType::of::<usize>()
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>()
                                    },
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::const_(0),
                                NodeInput::const_(0),
                            ],
                            required_inputs: vec![
                                UsedRegion::Used,
                                UsedRegion::Used,
                                UsedRegion::Used
                            ]
                        });

                        let input = input.clone();
                        let graph = MutableGraph::try_from((input, &ComptimeCtx {
                            qualifiers: vec![],
                            node_types
                        })).unwrap();
                    }
                }
            ]
        }.run(errors)
    })
}

/* #[prompt]
async fn text_input(
    mut c: PromptContext<'_>,
    placeholder: In<str>,
    text: InOut<String>,
    text_modified: OutSend<(Range<usize>, String)>,
    ok_enabled: In<bool>
) -> String {
    let (enter_key_send, enter_key) = out_channel();
    let (click_ok_send, click_ok) = out_channel();
    let (click_cancel_send, click_cancel) = out_channel();
    // Note: `c.set_view` requires a mutable reference to `c`
    c.set_view("text_input.dui", iface![
        placeholder,
        text,
        text_modified,
        enter_key: enter_key_send,
        click_ok: click_ok_send,
        click_cancel: click_cancel_send,
        ok_enabled
    ]);
    select_inline((
        async {
            // ok_enabled being set to false when text is empty may be possible in the .dui directly in the future
            // But we can always do it in Rust
            loop {
                text_modified.await;
                *ok_enabled = text.get().len() > 0;
            }
        },
        async {
            // Listen for ok or enter
            concurrent_race([click_ok, enter_key.recv()]).await;
            // We can clone
            // Some(text.get().clone())
            // ...or we can consume the value from text. This will *panic at runtime* if we let the UI run after consuming an output, so be careful doing this and make sure it's on the last frame (there are no awaits afterwards)
            Some(text.consume())
        },
        async {
            // Listen for cancel
            click_cancel.await;
            None
        },
    )).await
}*/