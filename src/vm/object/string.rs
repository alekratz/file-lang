use crate::vm::{object::*, value::*};
use derivative::Derivative;

use std::{
    any::Any,
    borrow::Cow,
    fmt::{self, Debug, Display, Formatter},
};

#[derive(Debug, Derivative)]
#[derivative(Hash)]
#[derivative(PartialEq)]
pub struct StringObject {
    #[derivative(Hash = "ignore")]
    #[derivative(PartialEq = "ignore")]
    base_object: BaseObject,
    string: String,
}

impl Eq for StringObject {}

impl StringObject {
    pub fn new(base_object: BaseObject, string: String) -> Self {
        StringObject {
            base_object,
            string,
        }
    }

    pub fn string(&self) -> Cow<String> {
        Cow::Borrowed(&self.string)
    }
}

impl Display for StringObject {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        Display::fmt(&self.string(), fmt)
    }
}

impl Object for StringObject {
    fn get_attr(&self, name: &str) -> Option<Value> {
        self.base_object.get_attr(name)
    }

    fn set_attr(&mut self, name: String, value: Value) {
        self.base_object.set_attr(name, value)
    }

    fn members(&self) -> &ObjectMembers {
        self.base_object.members()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn base_object(&self) -> &BaseObject {
        &self.base_object
    }
}
