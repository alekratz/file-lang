use crate::{
    common::binding::Binding,
    vm::{inst::Inst, state::State, value::StackValue, value::*},
};
use shrinkwraprs::Shrinkwrap;
use std::{
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
};

pub struct UserFun {
    name_binding: Binding,
    code: Vec<Inst>,
    arity: usize,
}

impl UserFun {
    pub fn new(name_binding: Binding, code: Vec<Inst>, arity: usize) -> Self {
        UserFun {
            name_binding,
            code,
            arity,
        }
    }

    pub fn name_binding(&self) -> Binding {
        self.name_binding
    }
}

#[derive(Shrinkwrap)]
pub struct BuiltinFunPtr(pub Box<fn(&mut State, Vec<StackValue>)>);

impl BuiltinFunPtr {
    pub fn new(ptr: fn(&mut State, Vec<StackValue>)) -> Self {
        BuiltinFunPtr(Box::new(ptr))
    }

    fn fun_address(&self) -> usize {
        &self.0 as *const _ as usize
    }
}

impl Debug for BuiltinFunPtr {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_tuple("BuiltinFunPtr")
            .field(&format!("builtin function at {:#x}", &self.fun_address()))
            .finish()
    }
}

impl Clone for BuiltinFunPtr {
    fn clone(&self) -> Self {
        BuiltinFunPtr(Box::new(*self.0.as_ref()))
    }
}

impl Hash for BuiltinFunPtr {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.fun_address().hash(hasher);
    }
}

#[derive(Debug, Clone)]
pub struct BuiltinFun {
    name_binding: Binding,
    fun: BuiltinFunPtr,
}

impl BuiltinFun {
    pub fn new(name_binding: Binding, fun: BuiltinFunPtr) -> Self {
        BuiltinFun { name_binding, fun }
    }

    pub fn fun(&self) -> &BuiltinFunPtr {
        &self.fun
    }

    pub fn name_binding(&self) -> Binding {
        self.name_binding
    }
}

impl PartialEq for BuiltinFun {
    fn eq(&self, other: &Self) -> bool {
        self.name_binding == other.name_binding
            && (self.fun.fun_address() == other.fun.fun_address())
    }
}

impl Eq for BuiltinFun {}
