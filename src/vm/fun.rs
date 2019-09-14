use crate::vm::{Inst, CopyValuePool, CopyValue, Binding};
use std::{
    hash::{Hash, Hasher},
    cmp::Ordering,
};

#[derive(Debug, Clone)]
pub struct Fun {
    name: String,
    binding: Binding,
    code: Vec<Inst>,
    registers: CopyValuePool,
}

impl Fun {
    pub fn new(name: String, binding: Binding, code: Vec<Inst>, registers: CopyValuePool) -> Self {
        Fun {
            name,
            binding,
            code,
            registers,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
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
}

impl Eq for Fun {}

impl PartialEq for Fun {
    fn eq(&self, other: &Self) -> bool {
        self.binding == other.binding
    }
}

impl PartialOrd for Fun {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.binding.partial_cmp(&other.binding)
    }
}

impl Ord for Fun {
    fn cmp(&self, other: &Self) -> Ordering {
        self.binding.cmp(&other.binding)
    }
}

impl Hash for Fun {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.binding.hash(hasher)
    }
}

#[derive(Debug, Clone)]
pub(in super) struct StackFrame<'fun> {
    pub ip: usize,
    pub stack_base: usize,
    pub registers: CopyValuePool,
    pub fun: &'fun Fun,
}

impl StackFrame<'_> {
    pub fn decode(&self) -> Inst {
        self.fun.code[self.ip]
    }

    pub fn load_register(&self, binding: &Binding) -> Option<CopyValue> {
        self.registers.get(binding).copied()
    }
}
