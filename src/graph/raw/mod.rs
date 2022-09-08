use std::mem::MaybeUninit;
use structural_reflection::{HasStructure, RustType};
pub use nullability::*;
pub use tuple_of_has_structure::*;

mod nullability;
mod tuple_of_has_structure;

pub struct RawComputeFn<RuntimeCtx>(Box<dyn RawComputeFnTrait<RuntimeCtx>>);

// TODO: Ensure we can only put in copyable data and references somehow,
//   as data in the graph is freely copied and destroyed

pub struct RawInputs<'a>(&'a RawData);

pub struct RawOutputs<'a>(&'a mut RawData);

pub struct RawData {
    pub types: Vec<RustType>,
    /// Non-null in input, required to be set in output
    pub null_regions: Vec<NullRegion>,
    pub data: Vec<Box<[MaybeUninit<u8>]>>
}

#[derive(Clone)]
struct PanickingComputeFn;

#[derive(Clone)]
struct StaticComputeFn<RuntimeCtx>(fn(&mut RuntimeCtx, RawInputs<'_>, RawOutputs<'_>));

pub trait RawComputeFnTrait<RuntimeCtx>: Send + Sync + 'static {
    fn box_clone(&self) -> Box<dyn RawComputeFnTrait<RuntimeCtx>>;

    fn run(&self, ctx: &mut RuntimeCtx, inputs: RawInputs<'_>, outputs: RawOutputs<'_>);
}

impl<'a> RawInputs<'a> {
    pub fn new<Values: TupleOfHasStructure>(values: Values::OfOptionals) -> RawInputs<'a> where Values::StaticId: Sized {
        RawInputs(RawData {
            types: values.iter_rust_types().collect(),
            null_regions: Values::iter_is_some(values).map(|is_some| if is_some { NullRegion::NonNull } else { NullRegion::Null }).collect(),
            data: values.iter().map(|value| value.into_boxed_slice()).collect()
        })
    }

    pub fn types(&self) -> &[RustType] {
        &self.0.types
    }

    pub fn nonnull_regions(&self) -> &[NullRegion] {
        &self.0.null_regions
    }

    pub fn data(&self) -> &[Box<[MaybeUninit<u8>]>] {
        &self.0.data
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.0.types.len() == self.0.data.len(), "sanity check failed");
        self.0.types.len()
    }
}

impl<'a> RawOutputs<'a> {
    pub fn types(&self) -> &[RustType] {
        &self.0.types
    }

    pub fn nonnull_regions(&self) -> &[NullRegion] {
        &self.0.null_regions
    }

    pub fn data(&mut self) -> &mut [Box<[MaybeUninit<u8>]>] {
        &mut self.0.data
    }

    pub fn len(&self) -> usize {
        debug_assert!(self.0.types.len() == self.0.data.len(), "sanity check failed");
        self.0.types.len()
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
        RawComputeFn::from(PanickingComputeFn)
    }

    pub fn new(fun: fn(&mut CompoundViewCtx, RawInputs<'_>, RawOutputs<'_>)) -> Self {
        RawComputeFn::from(StaticComputeFn(fun))
    }

    pub fn run(&self, ctx: &mut CompoundViewCtx, inputs: RawInputs<'_>, outputs: RawOutputs<'_>) {
        self.0.run(ctx, inputs, outputs)
    }
}

impl Clone for RawComputeFn {
    fn clone(&self) -> Self {
        RawComputeFn(self.0.box_clone())
    }
}

impl<T: RawComputeFnTrait> From<T> for RawComputeFn {
    fn from(f: T) -> Self {
        RawComputeFn(Box::new(f))
    }
}

impl RawComputeFnTrait for PanickingComputeFn {
    fn box_clone(&self) -> Box<dyn RawComputeFnTrait> {
        Box::new(self.clone())
    }

    fn run(&self, _: &mut CompoundViewCtx, _: RawInputs<'_>, _: RawOutputs<'_>) {
        panic!("RawComputeFn::panicking()");
    }
}

impl RawComputeFnTrait for StaticComputeFn {
    fn box_clone(&self) -> Box<dyn RawComputeFnTrait> {
        Box::new(self.clone())
    }

    fn run(&self, ctx: &mut CompoundViewCtx, inputs: RawInputs<'_>, outputs: RawOutputs<'_>) {
        (self.0)(ctx, inputs, outputs)
    }
}
