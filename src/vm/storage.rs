use crate::vm::value::*;
use std::mem;

pub type ObjectValue = Box<dyn Object>;
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

    pub fn deref_stack_value(&self, stack_value: StackValue) -> Option<&dyn Object> {
        match stack_value {
            StackValue::ConstRef(c) => Some(self.deref_const(c)),
            StackValue::HeapRef(h) => Some(self.deref_heap(h)),
            _ => None,
        }
    }

    pub fn deref_heap(&self, heap_ref: HeapRef) -> &dyn Object {
        let value: &Box<dyn Object> = self.heap[*heap_ref]
            .as_ref()
            .expect("heap ref");
        // the deref story here is a little weird but whatever
        &**value
    }

    pub fn deref_const(&self, const_ref: ConstRef) -> &dyn Object {
        &*self.constant_pool[*const_ref]
    }
}
