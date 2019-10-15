use crate::vm::value::{Binding, CopyValue, ValueRef};
use std::{cell::RefCell, collections::HashMap};

pub const CTOR_NAME: &str = "__init__";
pub const CALL_NAME: &str = "__call__";

#[derive(Debug, Clone)]
pub struct Object {
    binding: Binding,
    type_ref: Option<ValueRef>,
    members: RefCell<HashMap<String, CopyValue>>,
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
            members: RefCell::new(members),
        }
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }

    pub fn type_ref(&self) -> Option<ValueRef> {
        self.type_ref
    }

    pub fn get_attr(&self, attr: impl AsRef<str>) -> Option<CopyValue> {
        let members = self.members.borrow();
        members.get(attr.as_ref()).copied()
    }

    pub fn set_attr(&self, attr: String, value: CopyValue) {
        let mut members = self.members.borrow_mut();
        members.insert(attr, value);
    }
}
