use crate::{
    vm::pool::Pool,
    vm::{
        fun::{StackFrame, UserFun},
        stack::Stack,
        value::{Binding, ConstRef, CopyValue, HeapRef, Value},
    },
};
use std::rc::Rc;

const HEAP_GROWTH_FACTOR: f64 = 2.0;
const HEAP_INITIAL_SIZE: usize = 128;

#[derive(Debug, Clone)]
pub struct Storage {
    stack: Stack,
    heap: Vec<Option<Value>>,
    pool: Pool,
}

impl Storage {
    pub fn new(pool: Pool) -> Self {
        Storage {
            stack: Default::default(),
            heap: vec!(None; HEAP_INITIAL_SIZE),
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
            },
            CopyValue::HeapRef(ref_id) => match self.load_heap(ref_id) {
                Value::CopyValue(c) if c.is_ref() => self.deref_value(*c),
                v => Some(v),
            },
            _ => None,
        }
    }

    pub fn allocate_heap(&mut self, value: Value) -> HeapRef {
        // TODO better heap allocator implementation
        let next_ref = self.heap.iter()
            .position(|v| v.is_none());
        let next_ref = if let Some(next_ref) = next_ref {
            next_ref
        } else {
            let end = self.heap.len();
            let new_length = ((end as f64) * HEAP_GROWTH_FACTOR) as usize;
            self.heap.resize_with(new_length, || None);
            end
        };
        self.heap[next_ref] = Some(value);
        HeapRef(next_ref)
    }

    pub fn free_heap(&mut self, ref_id: HeapRef) {
        assert!(self.heap[*ref_id].is_some(), "attempted to free freed heap reference");
        self.heap[*ref_id] = None;
    }

    pub fn load_heap(&self, ref_id: HeapRef) -> &Value {
        self.heap[*ref_id]
            .as_ref()
            .expect("attempted to load freed heap reference")
    }

    pub fn load_heap_mut(&mut self, ref_id: HeapRef) -> &mut Value {
        self.heap[*ref_id]
            .as_mut()
            .expect("attempted to load freed heap reference")
    }

    pub fn store_heap(&mut self, ref_id: HeapRef, value: Value) {
        assert!(self.heap[*ref_id].is_some(), "attempted to store to freed heap reference");
        self.heap[*ref_id] = Some(value);
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
