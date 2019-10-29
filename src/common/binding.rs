use crate::common::prelude::*;
use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct Binding(pub usize);

impl Binding {
    pub fn index(&self) -> usize {
        self.0
    }
}

pub trait Bound {
    fn binding(&self) -> Binding;
}

pub type Bindings = Mapping<String, Binding>;
