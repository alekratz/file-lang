use crate::vm::{object::*, value::*};

use std::{
    any::Any,
    fmt::{self, Debug, Display, Formatter},
};

#[derive(Debug)]
pub struct StringObject {
    base_object: BaseObject,
    string: String,
}

impl StringObject {
    pub fn new(base_object: BaseObject, string: String) -> Self {
        StringObject {
            base_object,
            string,
        }
    }

    pub fn string(&self) -> &String {
        &self.string
    }

    pub fn string_mut(&mut self) -> &mut String {
        &mut self.string
    }
}

impl Display for StringObject {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.string(), fmt)
    }
}

impl Object for StringObject {
    fn get_attr(&self, name: &str) -> Option<StackValue> {
        self.base_object.get_attr(name)
    }

    fn set_attr(&self, name: String, value: StackValue) {
        self.base_object.set_attr(name, value)
    }

    fn attrs(&self) -> Vec<String> {
        self.base_object.attrs()
    }

    fn value_ref(&self) -> ValueRef {
        self.base_object.value_ref()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

