use crate::vm::{value::*, object::*};


use std::{
    any::Any,
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

