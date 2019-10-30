use crate::syn::prelude::OpKind;

pub use crate::common::binding::{Binding, Bindings};

fn op_binding_name(op: &[OpKind]) -> String {
    assert!(op.len() > 0);
    let mut name = op[0].to_string();
    for op in &op[1..] {
        name += &op.to_string();
    }
    name
}

fn binary_op_binding_name(op: &[OpKind]) -> String {
    format!("#*binary op {}*#", op_binding_name(op))
}

fn unary_op_binding_name(op: &[OpKind]) -> String {
    format!("#*unary op {}*#", op_binding_name(op))
}

#[derive(Debug)]
pub struct BindingStack {
    stack: Vec<Bindings>,
    all: Vec<String>,
}

impl Default for BindingStack {
    fn default() -> Self {
        BindingStack {
            stack: vec![Default::default()],
            all: Vec::new(),
        }
    }
}

impl BindingStack {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn push_layer(&mut self, layer: Bindings) {
        self.stack.push(layer)
    }

    pub fn push_default(&mut self) {
        self.push_layer(Default::default())
    }

    pub fn pop_layer(&mut self) -> Option<Bindings> {
        self.stack.pop()
    }

    pub fn pop_expect(&mut self) -> Bindings {
        self.pop_layer().expect("empty binding stack")
    }

    pub fn last_layer(&self) -> Option<&Bindings> {
        self.stack.last()
    }

    pub fn last_layer_mut(&mut self) -> Option<&mut Bindings> {
        self.stack.last_mut()
    }

    pub fn create_binding(&mut self, name: String) -> Binding {
        let binding = Binding(self.all.len());
        self.all.push(name.clone());
        self.last_layer_mut()
            .expect("empty binding stack")
            .insert(name, binding);
        binding
    }

    pub fn get_or_create_binding(&mut self, name: &str) -> Binding {
        self.stack
            .iter()
            .rev()
            .filter_map(|layer| layer.get(name).copied())
            .next()
            .unwrap_or_else(|| self.create_binding(name.to_string()))
    }

    pub fn get_or_create_local_binding(&mut self, name: &str) -> Binding {
        self.last_layer()
            .expect("empty binding stack")
            .get(name)
            .copied()
            .unwrap_or_else(|| self.create_binding(name.to_string()))
    }

    pub fn get_local_binding(&self, name: &str) -> Option<Binding> {
        self.stack
            .last()
            .and_then(|bindings| bindings.get(name))
            .copied()
    }

    pub fn get_binding(&self, name: &str) -> Option<Binding> {
        self.stack
            .iter()
            .rev()
            .filter_map(|layer| layer.get(name).copied())
            .next()
    }

    pub fn get_bin_op_binding(&self, op: &[OpKind]) -> Option<Binding> {
        self.get_binding(&binary_op_binding_name(op))
    }

    pub fn get_un_op_binding(&self, op: &[OpKind]) -> Option<Binding> {
        self.get_binding(&unary_op_binding_name(op))
    }
}

impl From<BindingStack> for Vec<String> {
    fn from(other: BindingStack) -> Self {
        other.all
    }
}
