#![feature(decl_macro)]
#![feature(is_some_with)]

mod misc;
mod batch_file;

use std::error::Error;
use std::sync::Mutex;
use dui_graph::ir::{ComptimeCtx, IrGraph, NodeInput, NodeIOType, NodeTypeData};
use dui_graph::node_types::{NodeType, NodeTypes};
use dui_graph::ast::types::AstGraph;
use dui_graph::lower::LowerGraph;
use dui_graph::raw::{NullRegion, ComputeFn, IOData, InputData, OutputData};
use structural_reflection::c_tuple::CTuple2;
use structural_reflection::RustType;
use structural_reflection::derive::{HasTypeName, HasStructure};
use dui_graph::{CompoundViewCtx, StaticStrs};
use crate::batch_file::{RunTest, RunTestsOnFiles};
use crate::misc::{assert_eq_multiline, ErrorNodes, try_or_none};
use ttmap::ValueBox;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
#[repr(transparent)]
pub struct ViewId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, HasTypeName, HasStructure)]
#[repr(C)]
pub struct CRange<Idx> {
    pub start: Idx,
    pub end: Idx
}

struct TestRuntimeCtx {

}

#[test]
fn tests_on_files() {
    ErrorNodes::log_errors_and_panic(|errors| {
        RunTestsOnFiles {
            dir_name: "duis",
            try_parse: |input_string, path| AstGraph::parse_from_input(&input_string, path).map_err(|err| Box::new(err) as Box<dyn Error + 'static>),
            tests: &[
                RunTest {
                    test_name: "round-trip",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files, _prior_tests| {
                        let input_string = input.to_string();
                        let input2 = try_or_none!(
                            AstGraph::parse_from_input(&input_string, input_path),
                            errors, "couldn't re-parse input:\n{}", input_string
                        )?;
                        let input2_string = input2.to_string();
                        assert_eq_multiline!(input_string, input2_string, errors, "round trip failed");

                        None
                    }
                },
                RunTest {
                    test_name: "ir",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files, _prior_tests| {
                        let mut node_types = NodeTypes::new();
                        node_types.insert(String::from("Button"), NodeType {
                            compute: ComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO Button");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "text".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                        null_region: NullRegion::NonNull
                                    },
                                    NodeIOType {
                                        name: "is_enabled".to_string(),
                                        rust_type: RustType::of::<bool>(),
                                        null_region: NullRegion::NonNull,
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: String::from("click"),
                                        rust_type: RustType::of::<()>(),
                                        null_region: NullRegion::Null
                                    }
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::const_(false)
                            ],
                            default_default_outputs: vec![
                                NodeInput::Hole,
                                NodeInput::Hole,
                            ]
                        });
                        node_types.insert(String::from("TextField"), NodeType {
                            compute: ComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO TextField");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "text".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: "placeholder".to_string(),
                                        rust_type: RustType::of::<&str>(),
                                        null_region: NullRegion::Null,
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: String::from("text"),
                                        rust_type: RustType::of::<&str>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: String::from("text_modified"),
                                        rust_type: RustType::of::<CTuple2<CRange<usize>, &str>>(),
                                        null_region: NullRegion::Null
                                    },
                                    NodeIOType {
                                        name: String::from("enter_key"),
                                        rust_type: RustType::of::<()>(),
                                        null_region: NullRegion::Null
                                    }
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::const_("")
                            ],
                            default_default_outputs: vec![
                                NodeInput::Hole,
                                NodeInput::Hole,
                                NodeInput::Hole,
                                NodeInput::Hole,
                            ]
                        });
                        node_types.insert_fn0(String::from("Box"), |ctx| Ok(NodeType {
                            compute: ComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO Box");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "children".to_string(),
                                        rust_type: RustType::of_array::<ViewId>(ctx.input_types.get(0).and_then(|input_type| input_type.rust_type.structure.array_or_tuple_length()).unwrap_or(0)),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: "width".to_string(),
                                        rust_type: RustType::of::<usize>(),
                                        null_region: NullRegion::Null,
                                    },
                                    NodeIOType {
                                        name: "height".to_string(),
                                        rust_type: RustType::of::<usize>(),
                                        null_region: NullRegion::Null,
                                    }
                                ],
                                outputs: vec![
                                    NodeIOType {
                                        name: String::from(StaticStrs::SELF_FIELD),
                                        rust_type: RustType::of::<ViewId>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                ]
                            },
                            default_inputs: vec![
                                NodeInput::Hole,
                                NodeInput::Hole,
                                NodeInput::Hole,
                            ],
                            default_default_outputs: vec![
                                NodeInput::Hole
                            ]
                        }));

                        let input = input.clone();
                        let comptime_ctx = ComptimeCtx { qualifiers: vec![], node_types };
                        try_or_none!(
                            IrGraph::try_from((input, &comptime_ctx)),
                            errors,
                            "graph to IR failed"
                        ).map(|x| Box::new(x) as ValueBox)
                    }
                },
                RunTest {
                    test_name: "lower",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files, prior_tests| {
                        let ir_graph = prior_tests.get::<IrGraph>()?;

                        try_or_none!(
                            LowerGraph::try_from(ir_graph.clone()),
                            errors,
                            "IR to lower graph failed"
                        ).map(| graph| Box::new(Mutex::new(graph)) as ValueBox)
                    }
                },
                RunTest {
                    test_name: "run",
                    associated_files: &[],
                    run: |errors, input, input_path, _associated_files, prior_tests| {
                        let mut lower_graph = prior_tests.get::<Mutex<LowerGraph>>()?.lock().unwrap();

                        let input_types = [
                            RustType::of::<&str>(),
                            RustType::of::<&str>(),
                            RustType::of::<bool>(),
                            RustType::of::<()>()
                        ];
                        let input_nullability = [
                            NullRegion::NonNull,
                            NullRegion::NonNull,
                            NullRegion::NonNull,
                            NullRegion::Null,
                        ];
                        let output_types = [
                            RustType::of::<&str>(),
                            RustType::of::<CTuple2<CRange<usize>, &str>>(),
                            RustType::of::<()>(),
                            RustType::of::<()>(),
                            RustType::of::<()>()
                        ];
                        let output_nullability = [
                            NullRegion::NonNull,
                            NullRegion::Null,
                            NullRegion::Null,
                            NullRegion::Null,
                            NullRegion::Null,
                        ];
                        let check_errors = lower_graph.check(&input_types, &output_types, &input_nullability, &output_nullability);
                        if !check_errors.is_empty() {
                            errors.push(format!("lower graph check failed: {}", check_errors));
                            return None;
                        }

                        /*
                        let input_data = input_types.iter().map(|x| Box::new_uninit_slice(x.size)).collect::<Vec<_>>();
                        let output_data = output_types.iter().map(|x| Box::new_uninit_slice(x.size)).collect::<Vec<_>>();
                        let inputs = RawData {
                            types: input_types.to_vec(),
                            null_regions: input_nullability.to_vec(),
                            data: input_data
                        };
                        let mut outputs = RawData {
                            types: output_types.to_vec(),
                            null_regions: output_nullability.to_vec(),
                            data: output_data
                        };

                        // TODO: run inputs and check outputs with some sets of data

                        let mut ctx = TestRuntimeCtx {};
                        // compute does the exact same as compute_unchecked, but runs check first
                        unsafe { lower_graph.compute_unchecked(&mut ctx, RawInputs::from(&inputs), RawOutputs::from(&mut outputs)) };
                        */

                        None
                    }
                },
            ]
        }.run(errors)
    })
}

/* #[prompt]
async fn text_input(
    mut c: PromptContext<'_>,
    placeholder: In<str>,
    text: InOut<String>,
    text_modified: OutSend<(CRange<usize>, String)>,
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