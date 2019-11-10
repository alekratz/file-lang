use crate::vm::{fun::BuiltinFun, object::*, value::*};
use std::any::Any;

#[derive(Debug, Clone)]
pub struct BuiltinFunObject {
    base_object: BaseObject,
    builtin_fun: BuiltinFun,
}

impl BuiltinFunObject {
    pub fn new(base_object: BaseObject, builtin_fun: BuiltinFun) -> Self {
        BuiltinFunObject {
            base_object,
            builtin_fun,
        }
    }

    pub fn builtin_fun(&self) -> &BuiltinFun {
        &self.builtin_fun
    }
}

impl Object for BuiltinFunObject {
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
