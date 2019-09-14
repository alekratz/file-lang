pub mod fun;
pub mod value;

pub mod prelude {
    pub use super::fun::Fun;
    pub use super::value::*;
    pub use super::{
        Inst, Vm
    };
}

use fun::*;
use shrinkwraprs::Shrinkwrap;
use value::*;

#[derive(Debug, Clone, Copy)]
pub enum Inst {
    Pop(usize),
    PushValue(CopyValue),
    Load(Binding),
    Store(Binding),
    Return,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Vm<'pool> {
    stack: Vec<CopyValue>,
    stack_frames: Vec<StackFrame<'pool>>,
    heap: Vec<Option<Value>>,
    const_pool: &'pool Vec<Value>,
    halt: bool,
}

impl<'pool> Vm<'pool> {
    pub fn new(const_pool: &'pool Vec<Value>) -> Self {
        Vm {
            stack: Default::default(),
            stack_frames: Default::default(),
            heap: Default::default(),
            const_pool,
            halt: false,
        }
    }

    pub fn halt(&mut self) {
        self.halt = true;
    }

    fn stack_frame(&self) -> &'pool StackFrame {
        self.stack_frames.last().expect("no stack frames")
    }

    fn stack_frame_mut(&mut self) -> &'pool mut StackFrame {
        self.stack_frames.last_mut().expect("no stack frames")
    }

    fn ip(&self) -> usize {
        self.stack_frame().ip
    }

    fn run(&mut self) -> Result<(), String> {
        loop {
            if self.halt {
                break;
            }
            let stack_frame = self.stack_frame();
            let op = stack_frame.decode();

            match op {
                Inst::Pop(n) => {
                    let retain = self.stack.len() - n;
                    self.stack.truncate(retain);
                }
                Inst::PushValue(value) => self.push(value),
                Inst::Load(binding) => {
                    if let Some(value) = self.load(binding) {
                        self.push(value);
                    } else {
                        unimplemented!("TODO: use-before-assign error");
                    }
                }
                Inst::Store(binding) => {
                    let value = self.pop().expect("no value on top of stack for store");
                    self.store(binding, value);
                }
                Inst::Return => {
                    self.stack_frames.pop();
                    break;
                }
                Inst::Halt => {
                    self.halt();
                }
            }
        }
        Ok(())
    }

    fn push(&mut self, value: CopyValue) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<CopyValue> {
        self.stack.pop()
    }

    fn load(&self, binding: Binding) -> Option<CopyValue> {
        for stack_frame in self.stack_frames.iter().rev() {
            if let Some(value) = stack_frame.registers.get(&binding) {
                return Some(*value);
            }
        }
        None
    }

    fn store(&mut self, binding: Binding, value: CopyValue) {
        for stack_frame in self.stack_frames.iter_mut().rev() {
            if let Some(value_slot) = stack_frame.registers.get_mut(&binding) {
                *value_slot = value;
                return;
            }
        }
        panic!("could not find value slot for binding {:?}", binding);
    }
}
