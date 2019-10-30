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

//pub type BuiltinFunPtr = fn(&mut Storage, args: Vec<StackValue>);
pub type BuiltinFunPtr = Box<dyn 'static + Fn(&mut Storage, Vec<StackValue>) + Sync>;

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
