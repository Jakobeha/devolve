use std::fmt::{Display, Formatter};

pub struct ValueAndCtx<'a, 'b, T: DisplayWithCtx + ?Sized> {
    value: &'a T,
    ctx: &'b T::Ctx
}

pub struct ValueAndCtx2<'a, 'b, 'c, T: DisplayWithCtx2 + ?Sized> {
    value: &'a T,
    ctx: (&'b T::Ctx1, &'c T::Ctx2),
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

impl Display for Indent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "  ")?;
        }
        Ok(())
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

pub trait DisplayWithCtx2 {
    type Ctx1;
    type Ctx2;

    fn fmt(&self, f: &mut Formatter<'_>, ctx: (&Self::Ctx1, &Self::Ctx2)) -> std::fmt::Result;

    fn with_ctx<'a, 'b, 'c>(&'a self, ctx: (&'b Self::Ctx1, &'c Self::Ctx2)) -> ValueAndCtx2<'a, 'b, 'c, Self> where Self: Sized {
        ValueAndCtx2 {
            value: self,
            ctx
        }
    }
}

// I don't really care that this is not actually used.
// Should probably move this code into a crate eventually
#[allow(dead_code)]
impl<Ctx> dyn DisplayWithCtx<Ctx=Ctx> {
    fn with_ctx<'a, 'b>(&'a self, ctx: &'b Ctx) -> ValueAndCtx<'a, 'b, dyn DisplayWithCtx<Ctx=Ctx>> {
        ValueAndCtx {
            value: self,
            ctx
        }
    }
}

#[allow(dead_code)]
impl<Ctx1, Ctx2> dyn DisplayWithCtx2<Ctx1=Ctx1, Ctx2=Ctx2> {
    fn with_ctx<'a, 'b, 'c>(&'a self, ctx: (&'b Ctx1, &'c Ctx2)) -> ValueAndCtx2<'a, 'b, 'c, dyn DisplayWithCtx2<Ctx1=Ctx1, Ctx2=Ctx2>> {
        ValueAndCtx2 {
            value: self,
            ctx
        }
    }
}

impl<'a, 'b, T: DisplayWithCtx + ?Sized> Display for ValueAndCtx<'a, 'b, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f, &self.ctx)
    }
}

impl<'a, 'b, 'c, T: DisplayWithCtx2 + ?Sized> Display for ValueAndCtx2<'a, 'b, 'c, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (ctx1, ctx2) = self.ctx;
        self.value.fmt(f, (&ctx1, &ctx2))
    }
}