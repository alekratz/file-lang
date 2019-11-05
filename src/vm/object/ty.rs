use crate::vm::{object::*, value::*};

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
