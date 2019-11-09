use crate::vm::{object::*, value::*};
use std::{any::Any, cell::RefCell, fmt::Debug};

/// The base object type.
///
/// This probably should not be used by the language directly, and instead should be used as a way
/// of backing object storage.
#[derive(Debug, Clone)]
pub struct BaseObject {
    value_ref: ValueRef,
    members: RefCell<ObjectMembers>,
}

impl BaseObject {
    pub fn new(value_ref: ValueRef, members: ObjectMembers) -> Self {
        BaseObject {
            value_ref,
            members: members.into(),
        }
    }

    pub fn with_members<B, F>(&self, fun: F) -> B
    where
        F: Fn(&ObjectMembers) -> B,
    {
        (fun)(&self.members.borrow())
    }
}

impl Object for BaseObject {
    fn get_attr(&self, name: &StringObject) -> Option<StackValue> {
        let members = self.members.borrow();
        members.get(&name).copied()
    }

    fn set_attr(&self, name: StringObject, value: StackValue) {
        let mut members = self.members.borrow_mut();
        members.insert(name, value);
    }

    fn attrs(&self) -> Vec<StringObject> {
        let members = self.members.borrow();
        members.keys().cloned().collect()
    }

    fn value_ref(&self) -> ValueRef {
        self.value_ref
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
