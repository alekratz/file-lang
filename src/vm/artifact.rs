use crate::vm::{object::*, value::*};

pub struct Artifact {
    main_function_ref: ValueRef,
    constants: Vec<ObjectValue>,
}

impl Artifact {
    pub fn new(main_function_ref: ValueRef, constants: Vec<ObjectValue>) -> Self {
        Artifact {
            main_function_ref,
            constants,
        }
    }

    pub fn main_function_ref(&self) -> ValueRef {
        self.main_function_ref
    }

    pub fn constants(&self) -> &Vec<ObjectValue> {
        &self.constants
    }

    pub fn main_function(&self) -> &dyn Object {
        &*self.constants[*self.main_function_ref()]
    }
}
