use std::fmt::{Display, Formatter};

pub struct ValueAndCtx<'a, 'b, T: DisplayWithCtx + ?Sized> {
    value: &'a T,
    ctx: &'b T::Ctx
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indent(pub usize);

impl Indent {
    pub const ZERO: Self = Indent(0);

    pub fn next(&self) -> Indent {
        Indent(self.0 + 1)
    }

    pub fn prev(&self) -> Indent {
        Indent(self.0 - 1)
    }
}

pub trait DisplayWithCtx {
    type Ctx;

    fn fmt(&self, f: &mut Formatter<'_>, ctx: &Self::Ctx) -> std::fmt::Result;

    fn with_ctx<'a, 'b>(&'a self, ctx: &'b Self::Ctx) -> ValueAndCtx<'a, 'b, Self> where Self: Sized {
        ValueAndCtx {
            value: self,
            ctx
        }
    }
}

impl<Ctx> dyn DisplayWithCtx<Ctx=Ctx> {
    fn with_ctx<'a, 'b>(&'a self, ctx: &'b Ctx) -> ValueAndCtx<'a, 'b, dyn DisplayWithCtx<Ctx=Ctx>> {
        ValueAndCtx {
            value: self,
            ctx
        }
    }
}

impl<T: Display> DisplayWithCtx for T {
    type Ctx = ();

    fn fmt(&self, f: &mut Formatter<'_>, (): &()) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl<'a, 'b, T: DisplayWithCtx + ?Sized> Display for ValueAndCtx<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f, &self.ctx)
    }
}