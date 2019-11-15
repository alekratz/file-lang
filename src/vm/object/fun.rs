use crate::vm::{fun::*, object::*, value::*};
use std::any::Any;

#[derive(Debug, Clone)]
pub struct CallableObject {
    base_object: BaseObject,
    fun: Fun,
}

impl CallableObject {
    pub fn new(base_object: BaseObject, fun: Fun) -> Self {
        CallableObject { base_object, fun }
    }

    pub fn fun(&self) -> &Fun {
        &self.fun
    }
}

impl Object for CallableObject {
    fn get_attr(&self, name: &str) -> Option<Value> {
        self.base_object.get_attr(name)
    }

    fn set_attr(&mut self, name: String, value: Value) {
        self.base_object.set_attr(name, value)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn base_object(&self) -> &BaseObject {
        &self.base_object
    }
}
