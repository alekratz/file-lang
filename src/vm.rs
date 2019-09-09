pub type ConstantId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Addr(usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    //Real(f64),
    String(String),
    Addr(Addr),
}

#[derive(Debug, Clone)]
pub enum Op {
    PushConstant(ConstantId),
    Return,
    Halt,
}

#[derive(Debug, Clone)]
pub struct Vm {
    ops: Vec<Op>,
    stack: Vec<Value>,
    constants: Vec<Value>,
    ip: usize,
}

impl Vm {
    pub fn run(&mut self) -> Result<(), String> {
        loop {
            if self.ip >= self.ops.len() {
                break;
            }

            let op = self.ops[self.ip].clone();
            self.ip += 1;

            match op {
                Op::PushConstant(id) => {
                    let constant = self.constants[id].clone();
                    self.push(constant);
                }
                Op::Return => {
                    if let Some(Value::Addr(Addr(addr))) = self.pop() {
                        self.ip = addr;
                    } else {
                        panic!("could not return; stack is either empty or address not on top of stack");
                    }
                }
                Op::Halt => {
                    break;
                }
            }
        }
        Ok(())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value)
    }

    fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
    }
}
