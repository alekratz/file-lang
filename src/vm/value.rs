use crate::{
    common::prelude::*,
    vm::{inst::Inst, storage::Storage},
};
use shrinkwraprs::Shrinkwrap;
use std::cell::RefCell;

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct ConstRef(pub usize);

#[derive(Shrinkwrap, Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub struct HeapRef(pub usize);

/// A value that lives on the stack.
///
/// Stack values must be lightweight, and more importantly, copyable. Stack values are either
/// numbers, an empty ("unset") value, a heap reference, or a constant value reference.
#[derive(Debug, Clone, Copy)]
pub enum StackValue {
    ConstRef(ConstRef),
    HeapRef(HeapRef),
    Int(i64),
    Float(f64),
    Empty,
}

pub trait Object {
    fn get_attr(&self, name: &str) -> Option<StackValue>;
    fn set_attr(&self, name: String, value: StackValue);
}

/// The base object type.
///
/// This probably should not be used by the language directly, and instead should be used as a way
/// of backing object storage.
#[derive(Debug, Default)]
struct BaseObject {
    members: RefCell<Mapping<String, StackValue>>,
}

impl BaseObject {
    pub fn new(members: Mapping<String, StackValue>) -> Self {
        BaseObject {
            members: members.into(),
        }
    }
}

impl Object for BaseObject {
    fn get_attr(&self, name: &str) -> Option<StackValue> {
        let members = self.members.borrow();
        members.get(name)
            .copied()
    }

    fn set_attr(&self, name: String, value: StackValue) {
        let mut members = self.members.borrow_mut();
        members.insert(name, value);
    }
}

struct TypeObject {
    type_name: ConstRef,
    base_object: BaseObject,
}

impl TypeObject {
    pub fn new(type_name: ConstRef, storage: &mut Storage) -> Self {
        TypeObject {
            type_name,
            base_object: BaseObject::default(),
        }
    }
}
