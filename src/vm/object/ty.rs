use crate::{
    common::prelude::*,
    vm::{fun::BuiltinFun, inst::Inst, storage::Storage, value::*, object::*},
};
use std::{
    any::Any,
    cell::RefCell,
    fmt::{self, Debug, Display, Formatter},
};

pub struct TypeObject {
    type_name: ConstRef,
    base_object: BaseObject,
}

impl TypeObject {
    pub fn new(type_name: ConstRef, storage: &mut Storage) -> Self {
        TypeObject {
            type_name,
            base_object: BaseObject::default(),
        }
    }
}
