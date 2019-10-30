use crate::{
    vm::{
        value::*,
        object::*,
    }
};

pub struct Artifact {
    main_function_ref: ConstRef,
    constants: Vec<ObjectValue>,
}

impl Artifact {
    pub fn new(main_function_ref: ConstRef, constants: Vec<ObjectValue>) -> Self {
        Artifact {
            main_function_ref,
            constants,
        }
    }

    pub fn main_function_ref(&self) -> ConstRef {
        self.main_function_ref
    }

    pub fn constants(&self) -> &Vec<ObjectValue> {
        &self.constants
    }

    pub fn main_function(&self) -> &dyn Object {
        &*self.constants[*self.main_function_ref()]
    }
}
