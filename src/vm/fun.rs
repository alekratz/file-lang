use crate::vm::{Inst, CopyValuePool, CopyValue, Binding, stack::Stack};
use std::{
    hash::{Hash, Hasher},
    cmp::Ordering,
    fmt::{Debug, Formatter, self},
};

#[derive(Debug, Clone)]
pub enum Fun {
    User(UserFun),
    Builtin(BuiltinFun),
}

impl Fun {
    pub fn name(&self) -> &str {
        match self {
            Fun::User(u) => u.name(),
            Fun::Builtin(b) => b.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserFun {
    name: String,
    params: Vec<Binding>,
    binding: Binding,
    code: Vec<Inst>,
    registers: CopyValuePool,
}

impl UserFun {
    pub fn new(name: String, params: Vec<Binding>, binding: Binding, code: Vec<Inst>, registers: CopyValuePool) -> Self {
        UserFun {
            name,
            params,
            binding,
            code,
            registers,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &[Binding] {
        &self.params
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }

    pub fn code(&self) -> &[Inst] {
        &self.code
    }

    pub fn registers(&self) -> &CopyValuePool {
        &self.registers
    }
    
    pub fn arity(&self) -> usize {
        self.params().len()
    }

    pub fn make_stack_frame<'fun>(&'fun self, stack_base: usize) -> StackFrame<'fun> {
        StackFrame {
            ip: Default::default(),
            stack_base,
            return_value: None,
            registers: self.registers.clone(),
            fun: self,
        }
    }
}

impl Eq for UserFun {}

impl PartialEq for UserFun {
    fn eq(&self, other: &Self) -> bool {
        self.binding == other.binding
    }
}

impl PartialOrd for UserFun {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.binding.partial_cmp(&other.binding)
    }
}

impl Ord for UserFun {
    fn cmp(&self, other: &Self) -> Ordering {
        self.binding.cmp(&other.binding)
    }
}

impl Hash for UserFun {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.binding.hash(hasher)
    }
}

pub struct BuiltinFun {
    name: String,
    fun: Box<fn(&mut Stack)>,
}

impl BuiltinFun {
    pub fn new(name: String, fun: Box<fn(&mut Stack)>) -> Self {
        BuiltinFun { name, fun }
    }

    pub fn call(&self, frame: &mut Stack) {
        (self.fun)(frame)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Clone for BuiltinFun {
    fn clone(&self) -> Self {
        BuiltinFun { name: self.name.clone(), fun: Box::new(*self.fun) }
    }
}

impl Debug for BuiltinFun {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_struct("BuiltinFun")
            .field("name", &self.name)
            .field("fun", &format!("function at {:#x}", &self.fun as *const _ as usize))
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame<'fun> {
    pub ip: usize,
    pub stack_base: usize,
    pub registers: CopyValuePool,
    pub return_value: Option<CopyValue>,
    pub fun: &'fun UserFun,
}

impl StackFrame<'_> {
    pub fn decode(&self) -> Inst {
        self.fun.code[self.ip]
    }

    pub fn load_register(&self, binding: &Binding) -> Option<CopyValue> {
        self.registers.get(binding).copied()
    }

    pub fn store_register(&mut self, binding: Binding, value: CopyValue) -> Option<CopyValue> {
        self.registers.insert(binding, value)
    }

    pub fn store_return(&mut self, value: CopyValue) {
        self.return_value = Some(value);
    }
}
