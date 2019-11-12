use crate::vm::{fun::*, object::*, value::*};
use std::any::Any;

#[derive(Debug, Clone)]
pub struct CallableObject {
    base_object: BaseObject,
    fun: Fun,
}

impl CallableObject {
    pub fn new(base_object: BaseObject, fun: Fun) -> Self {
        CallableObject {
            base_object,
            fun,
        }
    }

    pub fn fun(&self) -> &Fun {
        &self.fun
    }
}

impl Object for CallableObject {
    fn get_attr(&self, name: &StringObject) -> Option<StackValue> {
        self.base_object.get_attr(name)
    }

    fn set_attr(&self, name: StringObject, value: StackValue) {
        self.base_object.set_attr(name, value)
    }

    fn attrs(&self) -> Vec<StringObject> {
        self.base_object.attrs()
    }

    fn value_ref(&self) -> ValueRef {
        self.base_object.value_ref()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
