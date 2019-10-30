use crate::vm::{
    value::*,
    object::*,
};
use std::mem;

pub type HeapSlot = Option<ObjectValue>;

/// The initial number of slots for the heap to have.
///
/// This number is pretty arbitrary.
/// 
// TODO(gc): It would probably make sense to add this as a configuration parameter for GC tuning
const HEAP_INITIAL_SIZE: usize = 64;

/// The size multiplier to use for the heap when there aren't any free slots.
// TODO(gc): GC behavior should probably be factored out
const HEAP_GROWTH_FACTOR: f64 = 2.0;

pub struct Storage {
    stack: Vec<StackValue>,
    heap: Vec<HeapSlot>,
    constant_pool: Vec<ObjectValue>,
}

impl Storage {
    pub fn new(constant_pool: Vec<ObjectValue>) -> Self {
        let mut heap = Vec::with_capacity(HEAP_INITIAL_SIZE);
        heap.resize_with(HEAP_INITIAL_SIZE, Default::default);
        Storage {
            stack: Default::default(),
            heap,
            constant_pool,
        }
    }

    pub fn stack(&self) -> &Vec<StackValue> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut Vec<StackValue> {
        &mut self.stack
    }

    pub fn allocate(&mut self, value: impl Object + 'static) -> HeapRef {
        // TODO : faster allocation method
        // Find next open slot in the heap
        let first = self.heap.iter()
            .enumerate()
            .filter(|(_, v)| v.is_none())
            .map(|(i, _)| i)
            .next();
        let index = if let Some(index) = first {
            index
        } else {
            let heap_len = self.heap.len();
            let new_heap_size = ((heap_len as f64) * HEAP_GROWTH_FACTOR) as usize;
            self.heap.resize_with(new_heap_size, Default::default);
            heap_len
        };
        assert!(self.heap[index].is_none());
        self.heap[index] = Some(Box::new(value));
        HeapRef(index)
    }

    pub fn free(&mut self, heap_ref: HeapRef) {
        let old_value = mem::replace(&mut self.heap[*heap_ref], None);
        assert!(
            old_value.is_some(),
            "attempted to free already-freed heap memory, ref: {:?}",
            heap_ref
        );
    }

    /// Attempts to dereference and downcast a stack value to a more concrete object type.
    pub fn downcast_stack_value<O: Object + 'static>(&self, stack_value: StackValue) -> Option<&O> {
        self.deref_stack_value(stack_value)?
            .as_any()
            .downcast_ref::<O>()
    }

    /// Attempts to dereference a stack value ConstRef or HeapRef.
    pub fn deref_stack_value(&self, stack_value: StackValue) -> Option<&dyn Object> {
        match stack_value {
            StackValue::ConstRef(c) => Some(self.deref_const(c)),
            StackValue::HeapRef(h) => Some(self.deref_heap(h)),
            _ => None,
        }
    }

    /// Gets the object behind a HeapRef.
    pub fn deref_heap(&self, heap_ref: HeapRef) -> &dyn Object {
        let value: &Box<dyn Object> = self.heap[*heap_ref]
            .as_ref()
            .expect("heap ref");
        // the deref story here is a little weird but whatever
        &**value
    }

    /// Gets the object behind a ConstRef.
    pub fn deref_const(&self, const_ref: ConstRef) -> &dyn Object {
        &*self.constant_pool[*const_ref]
    }
    
    /// Pushes a value to the stack.
    pub fn push_stack(&mut self, value: StackValue) {
        self.stack_mut().push(value);
    }

    /// Attempts to pop a value from the stack.
    pub fn pop_stack(&mut self) -> Option<StackValue> {
        self.stack_mut().pop()
    }

    /// Pops the last N values from the stack.
    pub fn pop_n(&mut self, n: usize) -> Vec<StackValue> {
        let split_index = self.stack().len() - n;
        self.stack_mut().split_off(split_index)
    }
}
