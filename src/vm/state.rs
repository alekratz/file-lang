use crate::vm::{
    storage::*,
    value::*,
};

pub struct State {
    storage: Storage,
}

impl State {
    pub fn call(&mut self, _fun_ref: ValueRef, _args: Vec<StackValue>) {
        unimplemented!()
    }

    pub fn storage(&self) -> &Storage {
        &self.storage
    }

    pub fn storage_mut(&mut self) -> &mut Storage {
        &mut self.storage
    }
}
