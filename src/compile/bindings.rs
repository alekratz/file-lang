use crate::{common::builtins, compile::ir::BoundFun, syn::op::OpKind, vm::value::Binding};
use std::{collections::HashMap, mem};

pub type Bindings = HashMap<String, Binding>;

#[derive(Debug)]
pub struct BindingStack<'bindings> {
    stack: Vec<Bindings>,
    bindings: &'bindings mut Vec<String>,
}

pub fn op_binding_name(op: &[OpKind]) -> String {
    assert!(op.len() > 0);
    let mut name = op[0].to_string();
    for op in &op[1..] {
        name += &op.to_string();
    }
    name
}

pub fn binary_op_binding_name(op: &[OpKind]) -> String {
    format!("#*binary op {}*#", op_binding_name(op))
}

pub fn unary_op_binding_name(op: &[OpKind]) -> String {
    format!("#*unary op {}*#", op_binding_name(op))
}

impl<'bindings> BindingStack<'bindings> {
    pub fn new(bindings: &'bindings mut Vec<String>) -> Self {
        BindingStack {
            stack: vec![Default::default()],
            bindings,
        }
    }

    /// Collects all of the remaining bindings in the stack, and merges them into a single Bindings
    /// layer, popping all layers off.
    pub fn collapse(&mut self) -> Bindings {
        let stack = mem::replace(&mut self.stack, Vec::new());
        stack
            .into_iter()
            .flat_map(|bindings| bindings.into_iter())
            .collect()
    }

    /// Inserts builtin functions and operators to the current binding stack level, and pushes a
    /// new clean stack layer.
    pub fn insert_builtin_functions(&mut self) -> Vec<BoundFun> {
        let mut funs = Vec::new();
        for (name, fun) in builtins::FUNS.iter() {
            let binding = self.create_binding(name.to_string());
            funs.push(BoundFun::Builtin(binding, Box::new(*fun)));
        }

        for (op, fun) in builtins::BIN_OPS.iter() {
            let name = binary_op_binding_name(&op);
            let binding = self.create_binding(name);
            funs.push(BoundFun::Builtin(binding, Box::new(*fun)));
        }

        for (op, fun) in builtins::UN_OPS.iter() {
            let name = unary_op_binding_name(&op);
            let binding = self.create_binding(name);
            funs.push(BoundFun::Builtin(binding, Box::new(*fun)));
        }

        self.push_default();
        funs
    }

    pub fn push(&mut self, bindings: Bindings) {
        self.stack.push(bindings);
    }

    pub fn push_default(&mut self) {
        self.push(Default::default())
    }

    pub fn pop(&mut self) -> Option<Bindings> {
        self.stack.pop()
    }

    pub fn pop_expect(&mut self) -> Bindings {
        self.pop().expect("mismatched bindings stack")
    }

    pub fn get_binding(&self, name: &str) -> Option<Binding> {
        self.stack
            .iter()
            .rev()
            .filter_map(|map| map.get(name))
            .next()
            .copied()
    }

    pub fn get_bin_op_binding(&self, op: &[OpKind]) -> Option<Binding> {
        self.get_binding(&binary_op_binding_name(op))
    }

    pub fn get_un_op_binding(&self, op: &[OpKind]) -> Option<Binding> {
        self.get_binding(&unary_op_binding_name(op))
    }

    pub fn get_local_binding(&self, name: &str) -> Option<Binding> {
        self.stack.last().and_then(|map| map.get(name)).copied()
    }

    pub fn get_or_create_binding(&mut self, name: &str) -> Binding {
        self.get_binding(name)
            .unwrap_or_else(|| self.create_binding(name.to_string()))
    }

    pub fn get_or_create_local_binding(&mut self, name: &str) -> Binding {
        self.stack
            .last()
            .unwrap()
            .get(name)
            .copied()
            .unwrap_or_else(|| self.create_binding(name.to_string()))
    }

    pub fn create_binding(&mut self, name: String) -> Binding {
        let binding = Binding(self.bindings.len());
        let lexical_bindings = self.stack.last_mut().expect("no bindings");
        lexical_bindings.insert(name.clone(), binding);
        self.bindings.push(name);
        binding
    }
}
