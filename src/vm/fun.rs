use crate::vm::{inst::Inst, storage::Storage, value::StackValue, value::*};
use std::{
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
};

pub struct UserFun {
    name: ConstRef,
    code: Vec<Inst>,
    arity: usize,
}

impl UserFun {
    pub fn new(name: ConstRef, code: Vec<Inst>, arity: usize) -> Self {
        UserFun { name, code, arity }
    }
}

pub type BuiltinFunPtr = Box<fn(&mut Storage, Vec<StackValue>)>;

pub struct BuiltinFun {
    name: String,
    fun: BuiltinFunPtr,
}

impl BuiltinFun {
    pub fn new(name: String, fun: BuiltinFunPtr) -> Self {
        BuiltinFun { name, fun }
    }

    fn fun_address(&self) -> usize {
        &self.fun as *const _ as usize
    }

    pub fn call(&self, storage: &mut Storage, args: Vec<StackValue>) {
        (self.fun)(storage, args)
    }
}

impl Clone for BuiltinFun {
    fn clone(&self) -> Self {
        BuiltinFun {
            name: self.name.clone(),
            fun: Box::new(*self.fun.as_ref()),
        }
    }
}

impl Debug for BuiltinFun {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_struct("BuiltinFun")
            .field("name", &self.name)
            .field(
                "fun",
                &format!("builtin function at {:#x}", &self.fun as *const _ as usize),
            )
            .finish()
    }
}

impl PartialEq for BuiltinFun {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && (self.fun_address() == other.fun_address())
    }
}

impl Eq for BuiltinFun {}

impl Hash for BuiltinFun {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
        self.fun_address().hash(hasher);
    }
}
