use std::any::TypeId;
use std::mem::MaybeUninit;

#[derive(Clone)]
pub struct RawComputeFn(Box<dyn Fn(&mut CompoundViewCtx, RawInputs<'_>, RawOutputs<'_>) + Send + Clone>);

// TODO: Ensure once we put in data it's consumed so we don't cause multiple drops
pub struct RawInputs<'a>(&'a RawData);

// TODO: Ensure we can only take out data once so we don't cause multiple drops
pub struct RawOutputs<'a>(&'a mut RawData);

pub struct RawData {
    pub types: Vec<TypeId>,
    pub data: Vec<Box<[MaybeUninit<u8>]>>
}

impl<'a> RawInputs<'a> {
    pub fn types(&self) -> &[TypeId] {
        &self.0.types
    }

    pub fn data(&self) -> &[Box<[MaybeUninit<u8>]>] {
        &self.0.data
    }
}

impl<'a> RawOutputs<'a> {
    pub fn types(&self) -> &[TypeId] {
        &self.0.types
    }

    pub fn data(&mut self) -> &mut [Box<[MaybeUninit<u8>]>] {
        &mut self.0.data
    }
}

impl<'a> From<&'a RawData> for RawInputs<'a> {
    fn from(data: &'a RawData) -> Self {
        RawInputs(data)
    }
}

impl<'a> From<&'a mut RawData> for RawOutputs<'a> {
    fn from(data: &'a mut RawData) -> Self {
        RawOutputs(data)
    }
}

impl RawComputeFn {
    pub fn panicking() -> Self {
        RawComputeFn(Box::new(|_, _, _| {
            panic!("RawComputeFn::panicking() (dummy compute function, should've never been called)");
        }))
    }

    pub fn run(&self, ctx: &mut CompoundViewCtx, inputs: RawInputs<'_>, outputs: RawOutputs<'_>) {
        (self.0)(ctx, inputs, outputs)
    }
}

