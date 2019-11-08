pub use crate::vm::fun::{BuiltinFun, BuiltinFunPtr};

use crate::{compile::binding::*, syn::op::*, vm::object::Object};
use std::{
    any::{self, TypeId},
    collections::HashMap,
};

#[derive(Debug, Default)]
pub struct Builtins {
    bin_ops: HashMap<OpList, Binding>,
    functions: HashMap<Binding, BuiltinFun>,
    types: HashMap<Binding, BuiltinType>,
    type_bindings: HashMap<TypeId, Binding>,
}

impl Builtins {
    pub fn bin_ops(&self) -> &HashMap<OpList, Binding> {
        &self.bin_ops
    }

    pub fn insert_bin_op(&mut self, op: OpList, binding: Binding) {
        let previous = self.bin_ops.insert(op.clone(), binding);
        assert!(
            previous.is_none(),
            "bin op {} already registered",
            op
        );
    }

    pub fn functions(&self) -> &HashMap<Binding, BuiltinFun> {
        &self.functions
    }

    pub fn insert_function(&mut self, binding: Binding, fun: BuiltinFun) {
        self.functions.insert(binding, fun);
    }

    pub fn types(&self) -> &HashMap<Binding, BuiltinType> {
        &self.types
    }

    pub fn insert_type<O: 'static + Object>(&mut self, binding: Binding, ty: BuiltinType) {
        let type_id = TypeId::of::<O>();
        self.types.insert(binding, ty);
        let previous = self.type_bindings.insert(type_id, binding);
        assert!(
            previous.is_none(),
            "type already registered for type {}",
            any::type_name::<O>()
        );
    }

    pub fn get_type<O: 'static + Object>(&self) -> Option<&BuiltinType> {
        let type_id = TypeId::of::<O>();
        self.type_bindings
            .get(&type_id)
            .and_then(|binding| self.types.get(binding))
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinType {
    binding: Binding,
    members: HashMap<String, BuiltinFunPtr>,
}

impl BuiltinType {
    pub fn new(binding: Binding, members: HashMap<String, BuiltinFunPtr>) -> Self {
        BuiltinType { binding, members }
    }

    pub fn members(&self) -> &HashMap<String, BuiltinFunPtr> {
        &self.members
    }
}
