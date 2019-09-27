use crate::vm::{fun::StackFrame, value::CopyValue};
use shrinkwraprs::Shrinkwrap;
use std::ops::DerefMut;

#[derive(Debug, Clone, Default, Shrinkwrap)]
pub struct Stack {
    #[shrinkwrap(main_field)]
    stack: Vec<CopyValue>,
    frames: Vec<StackFrame>,
}

impl Stack {
    pub fn push_frame(&mut self, stack_frame: StackFrame) {
        self.frames.push(stack_frame);
    }

    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        self.frames.pop()
    }

    pub fn frames(&self) -> &Vec<StackFrame> {
        &self.frames
    }

    pub fn last_frame(&self) -> Option<&StackFrame> {
        self.frames().last()
    }

    pub fn frames_mut(&mut self) -> &mut Vec<StackFrame> {
        &mut self.frames
    }

    pub fn last_frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.frames_mut().last_mut()
    }

    pub fn pop_args(&mut self, argc: usize) -> Vec<CopyValue> {
        let at = self.len() - argc;
        self.split_off(at)
    }
}

impl DerefMut for Stack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}
