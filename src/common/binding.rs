use crate::common::prelude::*;
use shrinkwraprs::Shrinkwrap;

/// The base name binding type, used throughout the codebase.
///
/// This is an index value that is used as a lightweight reference to some local value in the
/// function stack. Bindings are lexical, so they are assigned during the initial syntax and IR
/// passes. They are subsequently used to define builtin function names, as well as operators.
/// Ultimately, they are used in the VM layer as register IDs.
#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
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
