use crate::{
    common::prelude::*,
    vm::{object::*, stack_frame::*, value::*},
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
    stack_frames: Vec<StackFrame>,
    stack: Vec<StackValue>,
    heap: Vec<HeapSlot>,
    constants_end: usize,
    return_register: Option<StackValue>,
}

impl Storage {
    pub fn new(constant_pool: Vec<ObjectValue>) -> Self {
        let constants_end = constant_pool.len();
        let mut heap: Vec<HeapSlot> = constant_pool.into_iter().map(From::from).collect();
        heap.resize_with(HEAP_INITIAL_SIZE + constants_end, Default::default);
        Storage {
            stack_frames: Default::default(),
            stack: Default::default(),
            heap,
            constants_end,
            return_register: None,
        }
    }

    pub fn stack_frame(&self) -> &StackFrame {
        self.stack_frames.last().expect("no stack frame")
    }

    pub fn stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack_frames.last_mut().expect("no stack frame")
    }

    pub fn stack_frames(&self) -> &Vec<StackFrame> {
        &self.stack_frames
    }

    pub fn stack_frames_mut(&mut self) -> &mut Vec<StackFrame> {
        &mut self.stack_frames
    }

    pub fn take_return_register(&mut self) -> Option<StackValue> {
        mem::replace(&mut self.return_register, None)
    }

    pub fn return_register(&self) -> Option<StackValue> {
        self.return_register
    }

    pub fn set_return_register(&mut self, return_register: StackValue) {
        self.return_register = Some(return_register);
    }

    pub fn unset_return_register(&mut self) {
        self.return_register = None;
    }

    pub fn stack(&self) -> &Vec<StackValue> {
        &self.stack
    }

    pub fn stack_mut(&mut self) -> &mut Vec<StackValue> {
        &mut self.stack
    }

    pub fn allocate(&mut self, value: impl Object + 'static) -> ValueRef {
        // TODO : faster allocation method
        // Find next open slot in the heap
        let first = self
            .heap
            .iter()
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
        ValueRef(index)
    }

    pub fn free(&mut self, value_ref: ValueRef) {
        let index: usize = *value_ref;
        if index < self.constants_end {
            panic!("attempted to free a constant value");
        }
        let old_value = mem::replace(&mut self.heap[*value_ref], None);
        assert!(
            old_value.is_some(),
            "attempted to free already-freed heap memory, ref: {:?}",
            value_ref
        );
    }

    /// Attempts to dereference and downcast a stack value to a more concrete object type.
    pub fn downcast_stack_value<O: Object + 'static>(&self, stack_value: StackValue) -> Option<&O> {
        self.deref_stack_value(stack_value)?
            .as_any()
            .downcast_ref::<O>()
    }

    /// Attempts to dereference a stack value ValueRef.
    pub fn deref_stack_value(&self, stack_value: StackValue) -> Option<&dyn Object> {
        match stack_value {
            StackValue::ValueRef(h) => Some(self.deref(h)),
            _ => None,
        }
    }

    /// Attempts to mutably dereference a stack value ValueRef.
    pub fn deref_stack_value_mut(&mut self, stack_value: StackValue) -> Option<&mut dyn Object> {
        match stack_value {
            StackValue::ValueRef(h) => Some(self.deref_mut(h)),
            _ => None,
        }
    }

    /// Gets the object behind a ValueRef.
    pub fn deref(&self, value_ref: ValueRef) -> &dyn Object {
        let value: &Box<dyn Object> = self.heap[*value_ref].as_ref().expect("heap ref");
        // the deref story here is a little weird but whatever
        &**value
    }

    pub fn deref_mut(&mut self, value_ref: ValueRef) -> &mut dyn Object {
        let value: &mut Box<dyn Object> = self.heap[*value_ref].as_mut().expect("heap ref");
        &mut **value
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

    pub fn peek_stack(&self) -> Option<StackValue> {
        self.stack().last().copied()
    }

    /// Loads the closest binding from the stack frame stack with the given binding.
    pub fn load_binding(&self, binding: Binding) -> Option<StackValue> {
        self.stack_frames
            .iter()
            .rev()
            .filter_map(|frame| frame.bindings.get(&binding))
            .next()
            .copied()
    }

    /// Stores the given value in the closest present binding in any stack frame.
    pub fn store_binding(&mut self, binding: Binding, value: StackValue) {
        for frame in self.stack_frames.iter_mut() {
            if let Some(slot) = frame.bindings.get_mut(&binding) {
                *slot = value;
                return;
            }
        }

        // otherwise, make a local binding
        self.store_binding_local(binding, value);
    }

    /// Stores the given value in the stack frame, either updating the given binding or creating
    /// it.
    pub fn store_binding_local(&mut self, binding: Binding, value: StackValue) {
        let frame = self.stack_frame_mut();
        frame.bindings.insert(binding, value);
    }
}
