use std::collections::HashSet;
use std::path::PathBuf;
use std::time::SystemTime;
use crate::ast::types::AstGraph;
use crate::ir::{ComptimeCtx, IrGraph};
use crate::lower::{LowerGraph, SelfContainedLowerGraph};
use crate::raw::IOData;

pub trait GraphResolver<RuntimeCtx: 'static + ?Sized> {
    fn with_resolve<R>(&mut self, fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R;
}

pub struct IdentityGraphResolver<'a, RuntimeCtx: 'static + ?Sized>(&'a mut SelfContainedLowerGraph<RuntimeCtx>);

impl<'a, RuntimeCtx: 'static + ?Sized> GraphResolver<RuntimeCtx> for IdentityGraphResolver<'a, RuntimeCtx> {
    fn with_resolve<R>(&mut self, _fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R {
        fun(&mut self.0.graph, &mut self.0.input_data, &mut self.0.output_data, false)
    }
}

pub struct PathGraphResolver<'a, RuntimeCtx: 'static + ?Sized> {
    pub path: PathBuf,
    pub comptime_ctx: &'a ComptimeCtx<RuntimeCtx>,
    pub loaded_time: SystemTime,
    pub loaded: Option<SelfContainedLowerGraph<RuntimeCtx>>,
    pub checked: HashSet<&'static str>,
}

impl<'a, RuntimeCtx: 'static + ?Sized> PathGraphResolver<'a, RuntimeCtx> {
    pub fn new(path: PathBuf, comptime_ctx: &'a ComptimeCtx<RuntimeCtx>) -> Self {
        Self {
            path: path.to_path_buf(),
            comptime_ctx,
            loaded_time: SystemTime::UNIX_EPOCH,
            loaded: None,
            checked: HashSet::new(),
        }
    }

    fn reload_if_necessary(&mut self) {
        self.clear_if_newer();
        self.load_if_empty();
    }

    fn clear_if_newer(&mut self) {
        if self.path_is_newer() {
            self.clear()
        }
    }

    fn load_if_empty(&mut self) {
        if !self.loaded.is_some() {
            self.loaded = Some(self.load());
            self.loaded_time = self.path
                .metadata().expect("failed to get graph file metadata")
                .modified().expect("failed to get graph file modified time");
        }
    }

    fn clear(&mut self) {
        self.loaded = None;
        self.checked.clear();
    }

    fn load(&self) -> SelfContainedLowerGraph<RuntimeCtx> {
        // Use unwrap_or_else and panic! because these errors produce nice messages
        let graph = AstGraph::parse_from(&self.path).unwrap_or_else(|err| panic!("failed to read and parse graph file\n{}", err));
        let graph = IrGraph::try_from((graph, self.comptime_ctx)).unwrap_or_else(|err| panic!("failed to compile graph\n{}", err));
        let graph = LowerGraph::try_from(graph).unwrap_or_else(|err| panic!("failed to lower graph\n{}", err));
        SelfContainedLowerGraph::new(graph)
    }

    fn path_is_newer(&self) -> bool {
        // Also unwrap_or_else and panic because we want the path in the error message
        let metadata = std::fs::metadata(&self.path).unwrap_or_else(|err| panic!("failed to stat graph file at\n\t{}\n\t{}", self.path.display(), err));
        let modified = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);
        self.loaded_time < modified
    }
}

impl<'a, RuntimeCtx: 'static + ?Sized> GraphResolver<RuntimeCtx> for PathGraphResolver<'a, RuntimeCtx> {
    fn with_resolve<R>(&mut self, fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R {
        self.reload_if_necessary();

        let graph = self.loaded.as_mut().unwrap();
        // If newly inserted = not checked
        let is_checked = !self.checked.insert(fn_name);
        fun(&mut graph.graph, &mut graph.input_data, &mut graph.output_data, is_checked)
    }
}

pub fn resolve_graph_path(path: &str, cargo_manifest_dir: &str, file: &str) -> PathBuf {
    let mut buf = PathBuf::from(cargo_manifest_dir);
    match path.strip_prefix("$CARGO_MANIFEST_DIR") {
        None => {
            buf.push(file);
            buf.pop();
            buf.push(path);
        },
        Some(mut path) => {
            if path.starts_with('/') || path.starts_with('\\') {
                path = &path[1..];
            }
            buf.push(path)
        }
    }
    buf
}