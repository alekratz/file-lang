use crate::{
    common::prelude::*,
    vm::{fun::BuiltinFun, inst::Inst, storage::Storage, value::*, object::*},
};
use lazy_static::lazy_static;
use shrinkwraprs::Shrinkwrap;
use std::{
    any::Any,
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
};

#[derive(Debug)]
pub struct StringObject {
    type_name: ConstRef,
    base_object: BaseObject,
    string: String,
}

impl Display for StringObject {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.string, fmt)
    }
}

impl Object for StringObject {
    fn get_attr(&self, name: &str) -> Option<StackValue> {
        self.base_object.get_attr(name)
    }

    fn set_attr(&self, name: String, value: StackValue) {
        self.base_object.set_attr(name, value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

