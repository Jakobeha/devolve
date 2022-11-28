#![feature(decl_macro)]
#![feature(is_some_and)]

mod misc;
mod batch_file;

use std::error::Error;
use std::sync::Mutex;
use devolve::ir::{ComptimeCtx, IrGraph, NodeIO, NodeIOType, NodeTypeData};
use devolve::ast::types::AstGraph;
use devolve::lower::LowerGraph;
use devolve::raw::{ComputeFn, NodeType, NodeTypes, NullRegion, StoreData, LoadData, NonNull, Nullable, NodeTypeMetadata};
use structural_reflection::c_tuple::{c_tuple, CTuple, CTuple2};
use structural_reflection::{qualifier, RustType};
use structural_reflection::derive::{HasTypeName, HasStructure};
use devolve::StaticStrs;
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
            dir_name: "dvls",
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
                        let mut node_types = NodeTypes::<TestRuntimeCtx>::new();
                        node_types.insert("Button", NodeType {
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
                                NodeIO::Hole,
                                NodeIO::inline_const(false)
                            ],
                            default_default_outputs: vec![
                                NodeIO::Hole,
                                NodeIO::Hole,
                            ],
                            meta: NodeTypeMetadata::default()
                        });
                        node_types.insert("TextField", NodeType {
                            compute: ComputeFn::new(|ctx, inputs: &LoadData, outputs: &mut StoreData| {
                                let (text, placeholder) = inputs.load_all::<CTuple!(
                                    NonNull<&str>,
                                    Nullable<&str>
                                )>().expect("TextField inputs are of bad type").into_reg();
                                assert_eq!((text, placeholder), (NonNull("Text"), Nullable::Some("Placeholder")));
                                outputs.store_all::<CTuple!(
                                    NonNull<ViewId>,
                                    NonNull<&str>,
                                    Nullable<CTuple2<CRange<i64>, &str>>,
                                    Nullable<()>
                                )>(c_tuple!(
                                    NonNull(ViewId(0)),
                                    NonNull(&text.0),
                                    Nullable::None,
                                    Nullable::None
                                )).expect("TextField outputs are of bad type");
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
                                        rust_type: RustType::of::<CTuple2<CRange<i64>, &str>>(),
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
                                NodeIO::Hole,
                                NodeIO::inline_const("")
                            ],
                            default_default_outputs: vec![
                                NodeIO::Hole,
                                NodeIO::Hole,
                                NodeIO::Hole,
                                NodeIO::Hole,
                            ],
                            meta: NodeTypeMetadata::default()
                        });
                        node_types.insert("Box", NodeType {
                            compute: ComputeFn::new(|ctx, inputs, outputs| {
                                eprintln!("TODO Box");
                            }),
                            type_data: NodeTypeData {
                                inputs: vec![
                                    NodeIOType {
                                        name: "children".to_string(),
                                        rust_type: RustType::of_slice::<ViewId>(),
                                        null_region: NullRegion::NonNull,
                                    },
                                    NodeIOType {
                                        name: "width".to_string(),
                                        rust_type: RustType::of::<i64>(),
                                        null_region: NullRegion::Null,
                                    },
                                    NodeIOType {
                                        name: "height".to_string(),
                                        rust_type: RustType::of::<i64>(),
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
                                NodeIO::Hole,
                                NodeIO::Hole,
                                NodeIO::Hole,
                            ],
                            default_default_outputs: vec![
                                NodeIO::Hole
                            ],
                            meta: NodeTypeMetadata::default()
                        });

                        let input = input.clone();
                        let comptime_ctx = ComptimeCtx { qualifier: qualifier![], node_types };
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
                        let ir_graph = prior_tests.get::<IrGraph<TestRuntimeCtx>>()?;

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
                        let mut lower_graph = prior_tests.get::<Mutex<LowerGraph<TestRuntimeCtx>>>()?.lock().unwrap();

                        let input_types = [
                            RustType::of::<&str>(),
                            RustType::of::<&str>(),
                            RustType::of::<bool>(),
                            RustType::of::<()>()
                        ];
                        let input_type_nullabilities = [
                            NullRegion::Null,
                            NullRegion::NonNull,
                            NullRegion::Null,
                            NullRegion::Null,
                        ];
                        let input_value_nullabilities = [
                            NullRegion::NonNull,
                            NullRegion::NonNull,
                            NullRegion::Null,
                            NullRegion::Null,
                        ];
                        let output_types = [
                            RustType::of::<&str>(),
                            RustType::of::<CTuple2<CRange<i64>, &str>>(),
                            RustType::of::<()>(),
                            RustType::of::<()>(),
                            RustType::of::<()>()
                        ];
                        let output_nullabilities = [
                            NullRegion::NonNull,
                            NullRegion::Null,
                            NullRegion::Null,
                            NullRegion::Null,
                            NullRegion::Null,
                        ];
                        let check_errors = lower_graph.check(
                            &input_types,
                            &output_types,
                            &input_type_nullabilities,
                            &input_value_nullabilities,
                            &output_nullabilities
                        );
                        if !check_errors.is_empty() {
                            errors.push(format!("lower graph check failed: {}", check_errors));
                            return None;
                        }

                        // TODO: test more complex computations, probably without view nodes

                        // placeholder field is before text field in text_input.dvl
                        let inputs = LoadData::init(c_tuple!(
                            Nullable::Some("Placeholder"),
                            NonNull("Text"),
                            Nullable::<bool>::None,
                            Nullable::<()>::None
                        ));
                        let (text, text_modified, enter_key, click_ok, click_cancel) = StoreData::with::<CTuple!(
                            NonNull<&str>,
                            Nullable<CTuple2<CRange<i64>, &str>>,
                            Nullable<()>,
                            Nullable<()>,
                            Nullable<()>
                        )>(|outputs| {
                            let mut ctx = TestRuntimeCtx {};
                            lower_graph.compute(&mut ctx, &inputs, outputs)
                                .expect("compute shouldn't have returned any errors, especially because we already checked");
                        }).expect("data not stored").into_reg();
                        assert_eq!(text, NonNull("Text"));
                        assert_eq!(text_modified, Nullable::None);
                        assert_eq!(enter_key, Nullable::None);
                        assert_eq!(click_ok, Nullable::None);
                        assert_eq!(click_cancel, Nullable::None);

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
    text_modified: OutSend<(CRange<i64>, String)>,
    ok_enabled: In<bool>
) -> String {
    let (enter_key_send, enter_key) = out_channel();
    let (click_ok_send, click_ok) = out_channel();
    let (click_cancel_send, click_cancel) = out_channel();
    // Note: `c.set_view` requires a mutable reference to `c`
    c.set_view("text_input.dvl", iface![
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
            // ok_enabled being set to false when text is empty may be possible in the .dvl directly in the future
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