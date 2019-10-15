pub mod fun;
pub mod pool;
pub mod stack;
pub mod store;
pub mod value;
pub mod object;
pub mod inst;

pub mod prelude {
    pub use super::fun::{BuiltinFun, Fun, UserFun};
    pub use super::pool::*;
    pub use super::value::*;
    pub use super::inst::*;
    pub use super::Vm;
}

use crate::vm::{fun::*, pool::*, stack::*, store::*, value::*, inst::*};
use std::mem;

#[derive(Debug, Clone)]
pub struct Vm {
    storage: Storage,
    halt: bool,
    return_value: Option<CopyValue>,
}

impl Vm {
    pub fn new(pool: Pool) -> Self {
        Vm {
            storage: Storage::new(pool),
            halt: false,
            return_value: None,
        }
    }

    pub fn main(&mut self, fun: &Fun) -> Result<(), String> {
        self.stack_mut().clear();
        self.stack_mut().frames_mut().clear();
        self.call(fun, vec![]);
        self.run()
    }

    pub fn halt(&mut self) {
        self.halt = true;
    }

    pub fn storage(&self) -> &Storage {
        &self.storage
    }

    pub fn storage_mut(&mut self) -> &mut Storage {
        &mut self.storage
    }

    pub fn stack_frame(&self) -> &StackFrame {
        self.stack().last_frame().expect("no stack frames")
    }

    pub fn stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack_mut().last_frame_mut().expect("no stack frames")
    }

    pub fn stack(&self) -> &Stack {
        self.storage().stack()
    }

    pub fn stack_mut(&mut self) -> &mut Stack {
        self.storage_mut().stack_mut()
    }

    pub fn pool(&self) -> &Pool {
        self.storage().pool()
    }

    pub fn deref_value(&self, value: CopyValue) -> Option<&Value> {
        self.storage().deref_value(value)
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

            // TODO: jump table(?)
            // how would this work with ops that take a value?
            // although discriminants work, getting a "possible" value is impossible unless we use
            // program arguments as parameters (not a bad idea)
            // NOTE : consider using a union for the above? Unions are "unsafe", however, if we're
            // careful, we can make them work

            match op {
                Inst::Pop(n) => {
                    let retain = self.stack().len() - n;
                    self.stack_mut().truncate(retain);
                }
                Inst::PushValue(value) => self.stack_mut().push(value),
                Inst::Load(binding) => {
                    let value = if let Some(value) = self.storage().load_binding(binding) {
                        value
                    } else {
                        panic!(
                            "could not find {:?} (variable {:?})",
                            binding,
                            self.pool().get_binding_name(binding)
                        );
                    };
                    self.stack_mut().push(value);
                }
                Inst::Store(binding) => {
                    let value = self
                        .stack_mut()
                        .pop()
                        .expect("no value on top of stack for store");
                    self.storage_mut().store_binding(binding, value);
                }
                Inst::GetAttr(ref_id) => {
                    let obj_ref = self.stack_mut().pop().unwrap();
                    let object = if let Some(object) = self.storage().deref_value(obj_ref) {
                        if let Value::Object(object) = object {
                            object
                        } else {
                            // TODO(exception) 
                            panic!("expected object ref on top of the stack, but got {:?} instead",
                                   object);
                        }
                    } else {
                        // TODO(exception) 
                        panic!("expected object ref on top of the stack, but got {:?} instead",
                               obj_ref);
                    };
                    let attr_value = self.storage().load_const(ref_id);
                    let attr = if let Value::String(s) = attr_value {
                        s
                    } else {
                        // TODO(exception) 
                        panic!("expected string attribute, but got {:?} instead", attr_value);
                    };
                    let value = object.members().get(attr)
                        .copied()
                        .unwrap_or(CopyValue::Empty);
                    self.stack_mut().push(value);
                }
                Inst::PopStore => {
                    let target = self
                        .stack_mut()
                        .pop()
                        .expect("no value on top of stack for pop_store target");
                    // TODO(exception) : Raise an error here when exceptions are ready, instead of a hard crash
                    assert!(
                        target.is_ref(),
                        "cannot store values in a non-ref target {:?}",
                        target
                    );
                    let value = self
                        .stack_mut()
                        .pop()
                        .expect("no value on top of stack for pop_store value");
                    match (target, value) {
                        (CopyValue::ConstRef(_), _) => panic!(
                            "tried to store a value {:?} in a const ref {:?}",
                            value, target
                        ),
                        (CopyValue::HeapRef(target_id), value) => {
                            if let Some(value) = self.deref_value(value) {
                                let value = value.clone();
                                self.storage_mut().store_heap(target_id, value);
                            } else {
                                self.storage_mut()
                                    .store_heap(target_id, Value::CopyValue(value));
                            }
                        }
                        v => panic!(
                            // TODO(exception)
                            "tried to store a value in a non-ref, non-literal value: {:?}",
                            v
                        ),
                    }
                }
                Inst::StoreReturn => {
                    let value = self
                        .stack_mut()
                        .pop()
                        .expect("no value on top of stack for store_return");
                    self.store_return(value);
                }
                Inst::PushReturn => {
                    let return_value =
                        mem::replace(&mut self.return_value, None).unwrap_or(CopyValue::Empty);
                    self.stack_mut().push(return_value);
                }
                Inst::DiscardReturn => {
                    self.return_value = None;
                }
                Inst::PopCall(argc) => {
                    let args = self.stack_mut().pop_args(argc);
                    let tos = self.stack_mut().pop().expect("no tos for pop_call");
                    let fun = if let Some(Value::Fun(fun)) = self.storage().deref_value(tos) {
                        // TODO - cheaper cloning or ref-counting functions in the pool (?)
                        fun.clone()
                    } else {
                        panic!("could not call value {:?} as a function", tos);
                    };
                    self.call(&fun, args);
                }
                Inst::Return => {
                    let frame = self
                        .stack_mut()
                        .pop_frame()
                        .expect("returned to empty stack frame");
                    self.return_value = frame.return_value;
                }
                Inst::Halt => {
                    self.halt();
                }
            }
        }
        Ok(())
    }

    pub fn call(&mut self, fun: &Fun, args: Vec<CopyValue>) {
        match fun {
            Fun::User(fun) => {
                let mut frame = self.storage().make_stack_frame(fun);
                // store initial param values in function stack frame registers
                for (arg, binding) in args.into_iter().zip(fun.params().iter().copied()) {
                    frame.store_register(binding, arg);
                }
                //println!("frame registers: {:#?}", frame.registers);
                self.stack_mut().push_frame(frame);
            }
            Fun::Builtin(fun) => {
                // builtin functions manage their own stack and run independently of
                // the VM
                fun.call(self, args);
            }
        }
    }

    fn store_return(&mut self, value: CopyValue) {
        let stack_frame = self.stack_frame_mut();
        stack_frame.return_value = Some(value);
    }
}
