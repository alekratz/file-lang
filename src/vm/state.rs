use crate::vm::{fun::*, inst::*, object::*, stack_frame::*, storage::*, value::*};
use std::rc::Rc;

pub struct State {
    storage: Storage,
    halt: bool,
    compare_flag: bool,
}

impl State {
    pub fn new(constants: Vec<ObjectValue>) -> Self {
        State {
            storage: Storage::new(constants),
            halt: false,
            compare_flag: false,
        }
    }

    pub fn call(&mut self, fun_ref: ValueRef, args: Vec<StackValue>) {
        let fun = self.storage()
            .downcast_ref::<CallableObject>(fun_ref)
            .expect("expected CallableObject")
            .fun()
            .clone();
        // NOTE: cloning a Fun should be pretty cheap.
        self.call_fun(&fun, args)
    }

    pub fn call_fun(&mut self, fun: &Fun, args: Vec<StackValue>) {
        match fun {
            Fun::User(fun) => self.call_user_fun(fun, args),
            Fun::Builtin(fun) => self.call_builtin_fun(fun, args),
        }
    }

    pub fn call_user_fun(&mut self, fun: &UserFun, args: Vec<StackValue>) {
        let base = self.storage().stack().len();
        let stack_frame = StackFrame {
            base,
            ip: 0,
            code: fun.code(),
            bindings: fun.bindings().clone(),
        };
        self.storage_mut()
            .stack_frames_mut()
            .push(stack_frame);
    }

    pub fn call_builtin_fun(&mut self, fun: &BuiltinFunPtr, args: Vec<StackValue>) {
        (fun)(self, args)
    }

    pub fn storage(&self) -> &Storage {
        &self.storage
    }

    pub fn storage_mut(&mut self) -> &mut Storage {
        &mut self.storage
    }

    pub fn execute(&mut self) {
        while !self.halt {
            let inst = {
                let stack_frame = self.storage_mut().stack_frame_mut();
                let inst = stack_frame.decode();
                stack_frame.ip += 1;
                inst
            };

            match inst {
                Inst::Pop(n) => {
                    self.storage_mut().pop_n(n);
                }
                Inst::PushValue(stack_value) => {
                    self.storage_mut().push_stack(stack_value);
                }
                Inst::Load(binding) => {
                    let value = self
                        .storage()
                        .load_binding(binding)
                        .expect("Load - unknown binding");
                    self.storage_mut().push_stack(value);
                }
                Inst::GetAttr(_attr) => {
                    let tos = self
                        .storage_mut()
                        .pop_stack()
                        .and_then(|value| {
                            self.storage().downcast_stack_value::<StringObject>(value)
                        })
                        .expect("GetAttr - expected Str value reference on stack");
                    let peek = self
                        .storage()
                        .peek_stack()
                        .and_then(|value| value.to_value_ref())
                        .expect("GetAttr - expected value reference value on stack");
                    let _attr = self.storage().deref(peek).get_attr(&tos.string());
                }
                Inst::Store(binding) => {
                    let value = self
                        .storage_mut()
                        .pop_stack()
                        .expect("Store - expected stack value");
                    self.storage_mut().store_binding(binding, value);
                }
                Inst::StoreLocal(binding) => {
                    let value = self
                        .storage_mut()
                        .pop_stack()
                        .expect("Store - expected stack value");
                    self.storage_mut().store_binding_local(binding, value);
                }
                Inst::PopStore => {
                    let target = self
                        .storage_mut()
                        .pop_stack()
                        .and_then(|value| value.to_value_ref())
                        .expect("PopStore - expected value reference value on stack");
                    // TODO : protect constant/readonly values, somehow
                    let _value = self
                        .storage_mut()
                        .pop_stack()
                        .expect("PopStore - expected stack value");
                    let _target = self.storage_mut().deref_mut(target);
                    // TODO : store stack values in target
                    todo!()
                }
                Inst::StoreReturn => {
                    let value = self
                        .storage_mut()
                        .pop_stack()
                        .expect("StoreReturn - expected stack value");
                    self.storage_mut().set_return_register(value);
                }
                Inst::PushReturn => {
                    let value = self
                        .storage_mut()
                        .take_return_register()
                        .expect("PushReturn - expected return value");
                    self.storage_mut().push_stack(value);
                }
                Inst::DiscardReturn => {
                    self.storage_mut().take_return_register();
                }
                Inst::PopCall(argc) => {
                    self.pop_call(argc);
                }
                Inst::PopCmp => todo!(),
                Inst::Jump(address) => {
                    self.jump(address);
                }
                Inst::JumpTrue(address) => {
                    if self.compare_flag {
                        self.jump(address);
                    }
                }
                Inst::JumpFalse(address) => {
                    if !self.compare_flag {
                        self.jump(address);
                    }
                }
                Inst::Return => {
                    self.storage_mut()
                        .stack_frames_mut()
                        .pop()
                        .expect("no stack frame to return");
                }
                Inst::Halt => {
                    self.halt = true;
                }
            }
        }
    }

    fn jump(&mut self, _address: usize) {
        todo!()
    }

    fn pop_call(&mut self, argc: usize) {
        let callable = self.storage_mut()
            .pop_stack()
            .and_then(|value| value.to_value_ref())
            .expect("expected value ref value on stack");
        let args = self.storage_mut()
            .pop_n(argc);
        self.call(callable, args);
    }
}
