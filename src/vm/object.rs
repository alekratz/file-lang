use crate::vm::{
    pool::Pool,
    value::{Binding, ValueRef, CopyValue},
};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Object {
    binding: Binding,
    type_ref: Option<ValueRef>,
    members: HashMap<String, CopyValue>,
}

impl Object {
    pub fn new(
        binding: Binding,
        type_ref: Option<ValueRef>,
        members: HashMap<String, CopyValue>,
    ) -> Self {
        Object {
            binding,
            type_ref,
            members,
        }
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }

    pub fn type_ref(&self) -> Option<ValueRef> {
        self.type_ref
    }

    pub fn members(&self) -> &HashMap<String, CopyValue> {
        &self.members
    }

    pub fn members_mut(&mut self) -> &mut HashMap<String, CopyValue> {
        &mut self.members
    }
}
