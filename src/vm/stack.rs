use crate::vm::{
    value::CopyValue,
    fun::StackFrame,
};
use shrinkwraprs::Shrinkwrap;
use std::ops::DerefMut;

#[derive(Debug, Clone, Default, Shrinkwrap)]
pub struct Stack<'fun> {
    #[shrinkwrap(main_field)]
    stack: Vec<CopyValue>,
    frames: Vec<StackFrame<'fun>>,
}

impl<'fun> Stack<'fun> {
    pub fn push_frame(&mut self, stack_frame: StackFrame<'fun>) {
        self.frames.push(stack_frame);
    }

    pub fn pop_frame(&mut self) -> Option<StackFrame<'fun>> {
        self.frames.pop()
    }

    pub fn frames(&self) -> &Vec<StackFrame<'fun>> {
        &self.frames
    }

    pub fn last_frame(&self) -> Option<&StackFrame<'fun>> {
        self.frames().last()
    }

    pub fn frames_mut(&mut self) -> &mut Vec<StackFrame<'fun>> {
        &mut self.frames
    }

    pub fn last_frame_mut(&mut self) -> Option<&mut StackFrame<'fun>> {
        self.frames_mut().last_mut()
    }

    pub fn pop_args(&mut self, argc: usize) -> Vec<CopyValue> {
        let at = self.len() - argc;
        self.split_off(at)
    }
}

impl DerefMut for Stack<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stack
    }
}
