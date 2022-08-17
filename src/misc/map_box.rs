pub(crate) fn map_box<A, B>(box_: Box<A>, fun: impl FnOnce(A) -> B) -> Box<B> {
    Box::new(fun(*box_))
}