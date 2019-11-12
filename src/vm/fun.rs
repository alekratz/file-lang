use crate::{
    common::binding::Binding,
    vm::{inst::Inst, state::State, value::StackValue},
};
use shrinkwraprs::Shrinkwrap;
use std::{
    collections::BTreeMap,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
};

#[derive(Debug, Clone)]
pub enum Fun {
    User(UserFun),
    Builtin(BuiltinFun),
}

impl Fun {
    pub fn call(&self, state: &mut State, args: Vec<StackValue>) {
        todo!()
    }

    pub fn binding(&self) -> Binding {
        match self {
            Fun::User(u) => u.binding(),
            Fun::Builtin(b) => b.binding(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserFun {
    binding: Binding,
    bindings: BTreeMap<Binding, StackValue>,
    code: Vec<Inst>,
    arity: usize,
}

impl UserFun {
    pub fn new(binding: Binding, bindings: Vec<Binding>, code: Vec<Inst>, arity: usize) -> Self {
        UserFun {
            binding,
            bindings: bindings.into_iter()
                .map(|b| (b, StackValue::Empty))
                .collect(),
            code,
            arity,
        }
    }

    pub fn binding(&self) -> Binding {
        self.binding
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
    binding: Binding,
    fun: BuiltinFunPtr,
}

impl BuiltinFun {
    pub fn new(binding: Binding, fun: BuiltinFunPtr) -> Self {
        BuiltinFun { binding, fun }
    }

    pub fn fun(&self) -> &BuiltinFunPtr {
        &self.fun
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }
}

impl PartialEq for BuiltinFun {
    fn eq(&self, other: &Self) -> bool {
        self.binding == other.binding
            && (self.fun.fun_address() == other.fun.fun_address())
    }
}

impl Eq for BuiltinFun {}