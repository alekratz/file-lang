use crate::vm::{inst::Inst, value::StackValue};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StackFrame {
    /// The stack size before this frame was created.
    base: usize,

    /// The current instruction address.
    ip: usize,

    /// The code executing in this stack frame.
    code: Rc<Vec<Inst>>,

    /// The return value for this stack frame.
    return_value: Option<StackValue>,
    // TODO(exception) stack frame exception object
}
