use crate::vm::{storage::Storage, value::*, object::*};

pub struct TypeObject {
    type_name: ConstRef,
    base_object: BaseObject,
}

impl TypeObject {
    pub fn new(type_name: ConstRef, _storage: &mut Storage) -> Self {
        TypeObject {
            type_name,
            base_object: BaseObject::default(),
        }
    }
}
