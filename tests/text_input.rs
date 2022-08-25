#![feature(decl_macro)]
#![feature(is_some_with)]

mod misc;
mod batch_file;

use std::error::Error;
use dui_graph::parse::types::SerialGraph;
use crate::batch_file::RunTestsOnFiles;
use crate::misc::ErrorNodes;

#[test]
fn test_parse_serial() {
    ErrorNodes::log_errors_and_panic(|errors| {
        RunTestsOnFiles {
            dir_name: "duis",
            try_parse: |_input, path| SerialGraph::parse_from(path).map_err(|err| Box::new(err) as Box<dyn Error + 'static>),
            tests: &[]
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