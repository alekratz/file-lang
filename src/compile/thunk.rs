use crate::vm::inst::Inst;
use std::mem;

#[derive(Debug, Clone, Default)]
struct FlattenThunk {
    address: usize,
    entry_address: usize,
    exit_address: usize,
    break_address: usize,
    continue_address: usize,
}

impl FlattenThunk {
    fn flatten(&mut self, thunk: Thunk) -> Vec<Inst> {
        match thunk {
            Thunk::Block(body) => {
                self.address += body.len();
                body
            }
            Thunk::Chain(thunks) => {
                thunks.into_iter()
                    .flat_map(|thunk| self.flatten(thunk))
                    .collect()
            }
            Thunk::Condition { condition, thunk_true, thunk_false } => {
                let mut body = self.flatten(*condition);
                let old_exit_address = self.exit_address;
                self.exit_address = self.address + thunk_true.len() + thunk_false.len();

                let false_address = self.address + thunk_true.len();
                body.push(Inst::JumpFalse(false_address));
                body.extend(self.flatten(*thunk_true));

                assert_eq!(false_address, body.len(), "possible incorrect Thunk::len() implementation");
                body.push(Inst::Jump(self.exit_address));
                body.extend(self.flatten(*thunk_false));

                self.exit_address = old_exit_address;
                body
            }
            Thunk::Loop { condition, thunk, } => {
                let mut body = if let Some(condition) = condition {
                    self.flatten(*condition)
                } else {
                    Vec::new()
                };

                let old_exit_address = self.exit_address;
                let old_continue_address = self.continue_address;
                let old_break_address = self.break_address;
                self.continue_address = self.address;
                self.exit_address = self.address + thunk.len();
                self.break_address = self.exit_address;

                body.extend(self.flatten(*thunk));

                self.break_address = old_break_address;
                self.continue_address = old_continue_address;
                self.exit_address = old_exit_address;
                body
            }

            Thunk::Continue => vec![Inst::Jump(self.continue_address)],
            Thunk::Break => vec![Inst::Jump(self.break_address)],
        }
    }
}

#[derive(Debug, Clone)]
pub enum Thunk {
    Block(Vec<Inst>),
    Chain(Vec<Thunk>),
    Condition {
        condition: Box<Thunk>,
        thunk_true: Box<Thunk>,
        thunk_false: Box<Thunk>,
    },
    Loop {
        condition: Option<Box<Thunk>>,
        thunk: Box<Thunk>,
    },
    Continue,
    Break,
}

impl Default for Thunk {
    fn default() -> Self {
        Thunk::Block(Vec::new())
    }
}

impl Thunk {
    pub fn flatten(self) -> Vec<Inst> {
        FlattenThunk::default()
            .flatten(self)
    }

    pub fn extend(&mut self, other: Thunk) {
        // put an empty value in the "self" pointer; this is because we can't borrow `this` (as
        // ref) and `other`(as value) and still have a nice match.
        //
        // From Rust documentation, the Vec::new() (which Thunk::default() calls) "does not
        // allocate until elements are pushed onto it". [1] So this should not affect performance.
        //
        // [1] https://doc.rust-lang.org/std/vec/struct.Vec.html#method.new
        let this = mem::replace(self, Thunk::default());
        *self = match (this, other) {
            (Thunk::Block(mut head), Thunk::Block(tail)) => {
                head.extend(tail);
                Thunk::Block(head)
            }
            (Thunk::Chain(mut head), Thunk::Block(tail)) => {
                if let Some(Thunk::Block(head_inner)) = head.last_mut() {
                    head_inner.extend(tail);
                } else {
                    head.push(Thunk::Block(tail));
                }
                Thunk::Chain(head)
            }
            (Thunk::Chain(mut head), tail) => {
                head.push(tail);
                Thunk::Chain(head)
            }
            (head, Thunk::Chain(mut tail)) => {
                tail.insert(0, head);
                Thunk::Chain(tail)
            }
            (head, tail) => Thunk::Chain(vec![head, tail]),
        };
    }

    pub fn push(&mut self, inst: Inst) {
        self.extend(vec![inst].into())
    }

    pub fn len(&self) -> usize {
        match self {
            Thunk::Block(block) => block.len(),
            Thunk::Chain(thunks) => thunks.iter().map(Thunk::len).sum(),
            Thunk::Condition { condition, thunk_true, thunk_false } => {
                condition.len() + 
                    1 + // JumpFalse
                    thunk_true.len() +
                    1 + // Jump
                    thunk_false.len()
            }
                // TODO(branch) add the number of instructions needed to jump
            Thunk::Loop { condition, thunk } => condition
                .as_ref()
                .map(|c| c.len())
                .unwrap_or(0) + thunk.len(),
            Thunk::Continue => 1,
            Thunk::Break => 1,
        }
    }
}

impl From<Vec<Inst>> for Thunk {
    fn from(other: Vec<Inst>) -> Self {
        Thunk::Block(other)
    }
}

impl From<Thunk> for Vec<Inst> {
    fn from(other: Thunk) -> Self {
        other.flatten()
    }
}
