pub use crate::vm::fun::{BuiltinFun, BuiltinFunPtr};

use crate::compile::binding::*;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Builtins {
    functions: HashMap<Binding, BuiltinFun>,
    types: HashMap<Binding, BuiltinType>,
}

impl Builtins {
    pub fn functions(&self) -> &HashMap<Binding, BuiltinFun> {
        &self.functions
    }

    pub fn functions_mut(&mut self) -> &mut HashMap<Binding, BuiltinFun> {
        &mut self.functions
    }

    pub fn types(&self) -> &HashMap<Binding, BuiltinType> {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut HashMap<Binding, BuiltinType> {
        &mut self.types
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinType {
    binding: Binding,
    members: HashMap<String, BuiltinFunPtr>,
}

impl BuiltinType {
    pub fn new(binding: Binding, members: HashMap<String, BuiltinFunPtr>) -> Self {
        BuiltinType { binding, members, }
    }

    pub fn members(&self) -> &HashMap<String, BuiltinFunPtr> {
        &self.members
    }
}
