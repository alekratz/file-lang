pub mod fun;
pub mod stack;
pub mod value;

pub mod prelude {
    pub use super::fun::{BuiltinFun, Fun, UserFun};
    pub use super::value::*;
    pub use super::{Inst, Vm};
}

use crate::{
    compile::pool::Pool,
    vm::{fun::*, stack::*, value::*},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Inst {
    /// Pop N values off of the stack.
    Pop(usize),

    /// Push a constant value to the stack.
    PushValue(CopyValue),

    /// Load a variable binding and push it to the stack.
    Load(Binding),

    /// Pop the top value off of the stack and store it in a variable binding.
    Store(Binding),

    /// Pop a storage target followed by a value off of the stack, and store the value in the
    /// target.
    PopStore,

    /// Pop the top value off of the stack and store it in the return value register.
    StoreReturn,

    /// Pop the top value off of the stack and attempt to call it with the given number of arguments.
    PopCall(usize),

    /// Exit the current function.
    Return,

    /// Halt the VM.
    Halt,
}

#[derive(Debug, Clone)]
pub struct Vm<'pool> {
    pool: &'pool Pool,
    stack: Stack<'pool>,
    heap: Vec<Value>,
    halt: bool,
}

impl<'pool> Vm<'pool> {
    pub fn new(pool: &'pool Pool) -> Self {
        Vm {
            pool,
            stack: Default::default(),
            heap: Default::default(),
            halt: false,
        }
    }

    pub fn main(&mut self, fun: &'pool Fun) -> Result<(), String> { 
        self.stack.clear();
        self.stack.frames_mut().clear();
        self.call(fun, vec![]);
        self.run()
    }

    pub fn halt(&mut self) {
        self.halt = true;
    }

    pub fn stack_frame(&self) -> &StackFrame<'pool> {
        self.stack.last_frame().expect("no stack frames")
    }

    pub fn stack_frame_mut(&mut self) -> &mut StackFrame<'pool> {
        self.stack.last_frame_mut().expect("no stack frames")
    }

    pub fn stack(&self) -> &Stack<'pool> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut Stack<'pool> {
        &mut self.stack
    }

    fn run(&mut self) -> Result<(), String> {
        loop {
            if self.halt {
                break;
            }
            let op = {
                let stack_frame = self.stack_frame_mut();
                let op = stack_frame.decode();
                stack_frame.ip += 1;
                op
            };

            match op {
                Inst::Pop(n) => {
                    let retain = self.stack.len() - n;
                    self.stack.truncate(retain);
                }
                Inst::PushValue(value) => self.stack.push(value),
                Inst::Load(binding) => {
                    let value = if let Some(value) = self.load(binding) {
                        value
                    } else {
                        panic!(
                            "could not find {:?} (variable {:?})",
                            binding,
                            self.pool.get_binding_name(binding)
                        );
                    };
                    self.stack.push(value);
                }
                Inst::Store(binding) => {
                    let value = self
                        .stack
                        .pop()
                        .expect("no value on top of stack for store");
                    self.store(binding, value);
                }
                Inst::PopStore => {
                    let target = self
                        .stack
                        .pop()
                        .expect("no value on top of stack for pop_store target");
                    // TODO : Raise an error here when exceptions are ready, instead of a hard crash
                    assert!(
                        target.is_ref(),
                        "cannot store values in a non-ref target {:?}",
                        target
                    );
                    let value = self
                        .stack
                        .pop()
                        .expect("no value on top of stack for pop_store value");
                    match (target, value) {
                        (CopyValue::ConstRef(_), _) => panic!("tried to store a value {:?} in a const ref {:?}", value, target),
                        (CopyValue::FunRef(_), _) => panic!("tried to store a value {:?} in a fun ref {:?}", value, target),
                        (CopyValue::HeapRef(target_id), value) => {
                            if let Some(value) = self.deref_value(value) {
                                self.heap[*target_id] = value.clone();
                            } else {
                                self.heap[*target_id] = Value::CopyValue(value);
                            }
                        }
                        v => { unreachable!("tried to store a value in a non-ref, non-literal value: {:?}", v) }
                    }
                }
                Inst::StoreReturn => {
                    let value = self
                        .stack
                        .pop()
                        .expect("no value on top of stack for store_return");
                    self.store_return(value);
                }
                Inst::PopCall(argc) => {
                    let tos = self.stack.pop().expect("no tos for pop_call");
                    let fun = if let Some(fun) = self.deref_fun(tos) {
                        fun
                    } else {
                        panic!("could not call {:?} as a function", tos);
                    };
                    let args = self.stack.pop_args(argc);
                    assert_eq!(args.len(), argc, "incorrect number of arguments on the stack");
                    // TODO : canary to ensure no stack misalignment
                    self.call(fun, args);
                }
                Inst::Return => {
                    let frame = self.stack.pop_frame().expect("returned to empty stack frame");
                    if let Some(value) = frame.return_value {
                        self.stack.push(value);
                    }
                }
                Inst::Halt => {
                    self.halt();
                }
            }
        }
        Ok(())
    }

    pub fn call(&mut self, fun: &'pool Fun, args: Vec<CopyValue>) {
        match fun {
            Fun::User(fun) => {
                let mut frame = fun.make_stack_frame(self.stack.len());
                // store initial param values in function stack frame registers
                for (arg, binding) in args.into_iter().zip(fun.params().iter().copied()) {
                    frame.store_register(binding, arg);
                }
                //println!("frame registers: {:#?}", frame.registers);
                self.stack.push_frame(frame);
            }
            Fun::Builtin(fun) => {
                // builtin functions manage their own stack and run independently of
                // the VM
                fun.call(self, args);
            }
        }
    }

    /// Tries to get a function by dereferencing a value until a function is reached.
    pub fn deref_fun(&self, value: CopyValue) -> Option<&'pool Fun> {
        match value {
            CopyValue::FunRef(ref_id) => Some(self.pool.get_fun(ref_id)),
            CopyValue::HeapRef(ref_id) => {
                if let Value::CopyValue(value) = self.deref_heap(ref_id) {
                    self.deref_fun(*value)
                } else {
                    None
                }
            }
            CopyValue::ConstRef(ref_id) => {
                if let Value::CopyValue(value) = self.deref_const(ref_id) {
                    self.deref_fun(*value)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn deref_value(&self, value: CopyValue) -> Option<&Value> {
        match value {
            CopyValue::HeapRef(ref_id) => match self.deref_heap(ref_id) {
                Value::CopyValue(c) if c.is_ref() => self.deref_value(*c),
                v => Some(v),
            },
            CopyValue::ConstRef(ref_id) => match self.deref_const(ref_id) {
                Value::CopyValue(c) if c.is_ref() => self.deref_value(*c),
                v => Some(v),
            },
            CopyValue::FunRef(ref_id) => None,
            _ => None,
        }
    }

    /// Gets a value from the const pool.
    pub fn deref_const(&self, ref_id: ConstRef) -> &'pool Value {
        self.pool.get_const(ref_id)
    }

    /// Gets a value from the heap.
    pub fn deref_heap(&self, ref_id: HeapRef) -> &Value {
        &self.heap[*ref_id]
    }

    /// Loads a value from a register using a variable binding.
    pub fn load(&self, binding: Binding) -> Option<CopyValue> {
        for stack_frame in self.stack.frames().iter().rev() {
            if let Some(value) = stack_frame.registers.get(&binding) {
                return Some(*value);
            }
        }
        None
    }

    pub fn store(&mut self, binding: Binding, value: CopyValue) {
        for stack_frame in self.stack.frames_mut().iter_mut().rev() {
            if let Some(value_slot) = stack_frame.registers.get_mut(&binding) {
                *value_slot = value;
                return;
            }
        }
        panic!(
            "could not find value slot for binding {:?} (var name {:?})",
            binding,
            self.pool.get_binding_name(binding)
        );
    }

    fn store_return(&mut self, value: CopyValue) {
        let stack_frame = self.stack_frame_mut();
        stack_frame.return_value = Some(value);
    }
}
