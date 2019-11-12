use crate::vm::{object::*, value::*};

#[derive(Debug)]
pub struct TypeObject {
    type_name: ConstRef,
    base_object: BaseObject,
}

impl TypeObject {
    pub fn new(type_name: ConstRef, base_object: BaseObject) -> Self {
        TypeObject {
            type_name,
            base_object,
        }
    }
}

impl Object for TypeObject {
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
