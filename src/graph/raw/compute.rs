use std::hash::{Hash, Hasher};
use crate::raw::{InputData, OutputData};

pub struct ComputeFn<RuntimeCtx: 'static + ?Sized>(Box<dyn ComputeFnTrait<RuntimeCtx>>);

#[derive(Clone)]
struct PanickingComputeFn;

struct StaticComputeFn<RuntimeCtx: 'static + ?Sized>(fn(&mut RuntimeCtx, &InputData, &mut OutputData));

pub trait ComputeFnTrait<RuntimeCtx: 'static + ?Sized>: Fn(&mut RuntimeCtx, &InputData, &mut OutputData) + Send + Sync + 'static {
    fn box_clone(&self) -> Box<dyn ComputeFnTrait<RuntimeCtx>>;
}

impl<RuntimeCtx: 'static + ?Sized> ComputeFn<RuntimeCtx> {
    pub fn panicking() -> Self {
        ComputeFn::new(|_, _, _| panic!("RawComputeFn::panicking()"))
    }

    pub fn new(fun: impl Fn(&mut RuntimeCtx, &InputData, &mut OutputData) + Clone + Send + Sync + 'static) -> Self {
        ComputeFn(Box::new(fun) as Box<dyn ComputeFnTrait<RuntimeCtx>>)
    }

    pub fn call(&self, ctx: &mut RuntimeCtx, inputs: &InputData, outputs: &mut OutputData) {
        (self.0)(ctx, inputs, outputs)
    }
}

impl<RuntimeCtx: 'static + ?Sized, F: Fn(&mut RuntimeCtx, &InputData, &mut OutputData) + Clone + Send + Sync + 'static> ComputeFnTrait<RuntimeCtx> for F {
    fn box_clone(&self) -> Box<dyn ComputeFnTrait<RuntimeCtx>> {
        Box::new(self.clone())
    }
}

// region boilerplate impls
impl<RuntimeCtx: 'static + ?Sized> Clone for ComputeFn<RuntimeCtx> {
    fn clone(&self) -> Self {
        ComputeFn(self.0.box_clone())
    }
}

impl<RuntimeCtx: 'static + ?Sized> Clone for StaticComputeFn<RuntimeCtx> {
    fn clone(&self) -> Self {
        StaticComputeFn(self.0)
    }
}

impl<RuntimeCtx: 'static + ?Sized> Copy for StaticComputeFn<RuntimeCtx> {}

impl<RuntimeCtx: 'static + ?Sized> PartialEq for StaticComputeFn<RuntimeCtx> {
    fn eq(&self, other: &Self) -> bool {
        self.0 as *const () == other.0 as *const ()
    }
}

impl<RuntimeCtx: 'static + ?Sized> Eq for StaticComputeFn<RuntimeCtx> {}

impl<RuntimeCtx: 'static + ?Sized> Hash for StaticComputeFn<RuntimeCtx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const ()).hash(state)
    }
}
// endregion