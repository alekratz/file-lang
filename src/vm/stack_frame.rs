use crate::{
    common::prelude::*,
    vm::{inst::Inst, value::StackValue},
};
use std::{collections::BTreeMap, rc::Rc};

#[derive(Debug, Clone)]
pub struct StackFrame {
    /// The stack size before this frame was created.
    pub base: usize,

    /// The current instruction address.
    pub ip: usize,

    /// The code executing in this stack frame.
    pub code: Rc<Vec<Inst>>,

    // TODO(exception) stack frame exception object
    pub bindings: BTreeMap<Binding, StackValue>,
}

impl StackFrame {
    pub fn decode(&self) -> Inst {
        self.code[self.ip]
    }
}
