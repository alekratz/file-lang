pub trait Visit<A: Accept> {
    type Out;
    fn visit(&mut self, accept: &A) -> Self::Out;
}

pub trait Accept: Sized {
    fn accept<V: Visit<Self>>(&self, visit: &mut V) {
        visit.visit(self);
    }
}
