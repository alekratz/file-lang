use crate::vm::{Binding, CopyValue, CopyValuePool, Inst, Vm};
use std::{
    cmp::Ordering,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub enum Fun {
    User(Rc<UserFun>),
    Builtin(BuiltinFun),
}

impl Fun {
    pub fn binding(&self) -> Binding {
        match self {
            Fun::User(u) => u.binding(),
            Fun::Builtin(b) => b.binding(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UserFun {
    params: Vec<Binding>,
    binding: Binding,
    code: Vec<Inst>,
    registers: CopyValuePool,
}

impl UserFun {
    pub fn new(
        params: Vec<Binding>,
        binding: Binding,
        code: Vec<Inst>,
        registers: CopyValuePool,
    ) -> Self {
        UserFun {
            params,
            binding,
            code,
            registers,
        }
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

pub type BuiltinFunPtr = fn(&mut Vm, Vec<CopyValue>);

pub struct BuiltinFun {
    binding: Binding,
    fun: Box<BuiltinFunPtr>,
}

impl BuiltinFun {
    pub fn new(binding: Binding, fun: Box<BuiltinFunPtr>) -> Self {
        BuiltinFun { binding, fun }
    }

    pub fn binding(&self) -> Binding {
        self.binding
    }

    pub fn call(&self, frame: &mut Vm, args: Vec<CopyValue>) {
        (self.fun)(frame, args)
    }
}

impl Clone for BuiltinFun {
    fn clone(&self) -> Self {
        BuiltinFun {
            binding: self.binding,
            fun: Box::new(*self.fun),
        }
    }
}

impl Debug for BuiltinFun {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_struct("BuiltinFun")
            .field("binding", &self.binding)
            .field(
                "fun",
                &format!("function at {:#x}", &self.fun as *const _ as usize),
            )
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub ip: usize,
    pub stack_base: usize,
    pub registers: CopyValuePool,
    pub return_value: Option<CopyValue>,
    pub fun: Rc<UserFun>,
}

impl StackFrame {
    pub fn decode(&self) -> Inst {
        self.fun.code[self.ip]
    }

    pub fn load_register(&self, binding: &Binding) -> Option<CopyValue> {
        self.registers.get(binding).copied()
    }

    pub fn store_register(&mut self, binding: Binding, value: CopyValue) -> Option<CopyValue> {
        self.registers.insert(binding, value)
    }
}
