use std::future::Future;
use std::pin::Pin;

/// helper to `select` futures which you define inline
pub macro select_inline {
    [$({ $(token:tt)* }),* $(,)?] => {
        select_inline((
            $(
                async {
                    $(token)*
                },
            )*
        ))
    }
}

/// helper to `select` futures which you define inline
pub fn select_inline<Futures: TupleOfFutures>(futures: Futures) -> Futures::SelectN {
    futures.into_select_n()
}

pub struct Select1<O, F1: Future<Output=O>>(F1);

pub struct Select2<O, F1: Future<Output=O>, F2: Future<Output=O>>(F1, F2);

pub struct Select3<O, F1: Future<Output=O>, F2: Future<Output=O>, F3: Future<Output=O>>(F1, F2, F3);

impl<O, F1: Future<Output=O>> Future for Select1<O, F1> {
    type Output = O;

    fn poll(mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        match [
            Pin::new(&mut this.0).poll(cx),
        ].iter().find(|p| !p.is_pending()) {
            None => std::task::Poll::Pending,
            Some(ready) => ready
        }
    }
}

impl<O, F1: Future<Output=O>, F2: Future<Output=O>> Future for Select2<O, F1, F2> {
    type Output = O;

    fn poll(mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        match [
            Pin::new(&mut this.0).poll(cx),
            Pin::new(&mut this.1).poll(cx),
        ].iter().find(|p| !p.is_pending()) {
            None => std::task::Poll::Pending,
            Some(ready) => ready
        }
    }
}

impl<O, F1: Future<Output=O>, F2: Future<Output=O>, F3: Future<Output=O>> Future for Select3<O, F1, F2, F3> {
    type Output = O;

    fn poll(mut self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        let this = unsafe { self.get_unchecked_mut() };
        match [
            Pin::new(&mut this.0).poll(cx),
            Pin::new(&mut this.1).poll(cx),
            Pin::new(&mut this.2).poll(cx),
        ].iter().find(|p| !p.is_pending()) {
            None => std::task::Poll::Pending,
            Some(ready) => ready
        }
    }
}

pub trait TupleOfFutures {
    type SelectN;

    fn into_select_n(self) -> Self::SelectN;
}

impl<O, F1: Future<Output=O>> TupleOfFutures for (F1,) {
    type SelectN = Select1<O, F1>;

    fn into_select_n(self) -> Self::SelectN {
        Select1(Self.0)
    }
}

impl<O, F1: Future<Output=O>, F2: Future<Output=O>> TupleOfFutures for (F1, F2,) {
    type SelectN = Select2<O, F1, F2>;

    fn into_select_n(self) -> Self::SelectN {
        Select2(Self.0, Self.1)
    }
}

impl<O, F1: Future<Output=O>, F2: Future<Output=O>, F3: Future<Output=O>> TupleOfFutures for (F1, F2, F3,) {
    type SelectN = Select3<O, F1, F2, F3>;

    fn into_select_n(self) -> Self::SelectN {
        Select3(Self.0, Self.1, Self.2)
    }
}