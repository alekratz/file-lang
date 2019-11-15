use crate::vm::{object::*, value::*};
use std::{any::Any, fmt::Debug};

/// The base object type.
///
/// This probably should not be used by the language directly, and instead should be used as a way
/// of backing object storage.
#[derive(Debug, Clone)]
pub struct BaseObject {
    members: ObjectMembers,
}

impl BaseObject {
    pub fn new(members: ObjectMembers) -> Self {
        BaseObject {
            members: members.into(),
        }
    }
}

impl Object for BaseObject {
    fn get_attr(&self, name: &str) -> Option<Value> {
        self.members.get(name).copied()
    }

    fn set_attr(&mut self, name: String, value: Value) {
        self.members.insert(name, value);
    }

    fn members(&self) -> &ObjectMembers {
        &self.members
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn base_object(&self) -> &BaseObject {
        &self
    }
}
