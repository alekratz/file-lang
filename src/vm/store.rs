use crate::{
    vm::{
        value::{CopyValue, Value, HeapRef, ConstRef, Binding},
        stack::Stack,
        fun::{UserFun, StackFrame},
    },
    vm::pool::Pool,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Storage {
    stack: Stack,
    heap: Vec<Value>,
    pool: Pool,
}

impl Storage {
    pub fn new(pool: Pool) -> Self {
        Storage {
            stack: Default::default(),
            heap: Default::default(),
            pool: pool,
        }
    }

    pub fn stack(&self) -> &Stack {
        &self.stack
    }
    
    pub fn stack_mut(&mut self) -> &mut Stack {
        &mut self.stack
    }

    pub fn pool(&self) -> &Pool {
        &self.pool
    }
    
    pub fn pool_mut(&mut self) -> &mut Pool {
        &mut self.pool
    }

    pub fn deref_value(&self, value: CopyValue) -> Option<&Value> {
        match value {
            CopyValue::ConstRef(ref_id) => match self.load_const(ref_id) {
                Value::CopyValue(c) if c.is_ref() => self.deref_value(*c),
                v => Some(v),
            }
            CopyValue::HeapRef(ref_id) => match self.load_heap(ref_id) {
                Value::CopyValue(c) if c.is_ref() => self.deref_value(*c),
                v => Some(v),
            }
            _ => None,
        }
    }

    pub fn load_heap(&self, ref_id: HeapRef) -> &Value {
        &self.heap[*ref_id]
    }

    pub fn store_heap(&mut self, ref_id: HeapRef, value: Value) {
        self.heap[*ref_id] = value;
    }

    pub fn load_const(&self, ref_id: ConstRef) -> &Value {
        &self.pool.get_const(ref_id)
    }

    /// Loads a value from a register using a variable binding.
    pub fn load_binding(&self, binding: Binding) -> Option<CopyValue> {
        for stack_frame in self.stack().frames().iter().rev() {
            if let Some(value) = stack_frame.registers.get(&binding) {
                return Some(*value);
            }
        }
        None
    }

    pub fn store_binding(&mut self, binding: Binding, value: CopyValue) {
        for stack_frame in self.stack_mut().frames_mut().iter_mut().rev() {
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

    pub fn make_stack_frame(&self, fun: &Rc<UserFun>) -> StackFrame {
        let stack_base = self.stack().len();
        StackFrame {
            ip: Default::default(),
            stack_base,
            return_value: None,
            registers: fun.registers().clone(),
            fun: Rc::clone(fun),
        }
    }
}
