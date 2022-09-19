use std::borrow::Cow;
use std::cell::{Cell, RefCell, RefMut};
use std::collections::HashSet;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use crate::ast::types::AstGraph;
use crate::ir::{ComptimeCtx, IrGraph};
use crate::lower::{LowerGraph, SelfContainedLowerGraph};
use crate::raw::IOData;

pub trait GraphResolver<RuntimeCtx> {
    fn with_resolve<R>(&mut self, fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R;
}

pub struct IdentityGraphResolver<'a, RuntimeCtx>(&'a mut SelfContainedLowerGraph<RuntimeCtx>);

impl<'a, RuntimeCtx> GraphResolver<RuntimeCtx> for IdentityGraphResolver<'a, RuntimeCtx> {
    fn with_resolve<R>(&mut self, _fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R {
        fun(&mut self.0.graph, &mut self.0.input_data, &mut self.0.output_data, false)
    }
}

pub struct PathGraphResolver<'a, RuntimeCtx> {
    pub path: PathBuf,
    pub comptime_ctx: &'a ComptimeCtx<RuntimeCtx>,
    pub loaded_time: Cell<SystemTime>,
    pub loaded: RefCell<Option<SelfContainedLowerGraph<RuntimeCtx>>>,
    pub checked: HashSet<&'static str>,
}

impl<'a, RuntimeCtx> PathGraphResolver<'a, RuntimeCtx> {
    pub fn new(path: PathBuf, comptime_ctx: &'a ComptimeCtx<RuntimeCtx>) -> Self {
        Self {
            path: path.to_path_buf(),
            comptime_ctx,
            loaded_time: Cell::new(SystemTime::UNIX_EPOCH),
            loaded: RefCell::new(None),
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
        if !self.loaded.borrow().is_some() {
            *self.loaded.borrow_mut() = Some(self.load());
            self.loaded_time.set(self.path.metadata().unwrap().modified().unwrap());
        }
    }

    fn clear(&mut self) {
        self.loaded.replace(None);
        self.checked.clear();
    }

    fn load(&self) -> SelfContainedLowerGraph<RuntimeCtx> {
        let graph = AstGraph::parse_from(&self.path).expect("failed to read and parse graph file");
        let graph = IrGraph::try_from((graph, self.comptime_ctx)).expect("failed to compile graph");
        let graph = LowerGraph::try_from(graph).expect("failed to compile graph");
        SelfContainedLowerGraph::new(graph)
    }

    fn path_is_newer(&self) -> bool {
        let metadata = std::fs::metadata(&self.path).expect("failed to stat graph file");
        let modified = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);
        let loaded_time = self.loaded_time.get();
        loaded_time < modified
    }
}

impl<RuntimeCtx> GraphResolver<RuntimeCtx> for PathGraphResolver<RuntimeCtx> {
    fn with_resolve<R>(&mut self, fn_name: &'static str, fun: impl FnOnce(&mut LowerGraph<RuntimeCtx>, &mut IOData, &mut IOData, bool) -> R) -> R {
        self.reload_if_necessary();

        let graph = RefMut::map(self.loaded.borrow_mut(), |x| x.as_mut().unwrap());
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
            buf.push(path);
        },
        Some(path) => buf.push(path)
    }
    buf
}