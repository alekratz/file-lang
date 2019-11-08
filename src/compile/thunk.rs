use crate::vm::inst::Inst;
use std::mem;

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
    Nop,
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
                    2 + // PopCmp, JumpFalse
                    thunk_true.len() +
                    1 + // Jump
                    thunk_false.len()
            }
                // TODO(branch) add the number of instructions needed to jump
            Thunk::Loop { condition, thunk } => condition
                .as_ref()
                .map(|c| c.len())
                .unwrap_or(0)
                + 1 // PopCmp
                + 1 // JumpFalse
                + 1 // Jump
                + thunk.len(),
            Thunk::Continue => 1,
            Thunk::Break => 1,
            Thunk::Nop => 0,
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

#[derive(Debug, Clone, Default)]
struct FlattenThunk {
    address: usize,
    entry_address: usize,
    exit_address: usize,
    break_address: usize,
    condition_depth: usize,
}

impl FlattenThunk {
    fn flatten(&mut self, thunk: Thunk) -> Vec<Inst> {
        let thunk_len = thunk.len();
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
                let old_exit_address = self.exit_address;
                if self.condition_depth == 0 {
                    self.exit_address = self.address + thunk_len;
                }

                let mut body = self.flatten(*condition);
                body.push(Inst::PopCmp);
                self.address += 1;
                self.condition_depth += 1;

                let false_address = self.address + thunk_true.len() + 2;
                body.push(Inst::JumpFalse(false_address));
                self.address += 1;
                body.extend(self.flatten(*thunk_true));
                body.push(Inst::Jump(self.exit_address));
                self.address += 1;
                body.extend(self.flatten(*thunk_false));

                self.exit_address = old_exit_address;
                self.condition_depth -= 1;
                body
            }
            Thunk::Loop { condition, thunk, } => {
                let old_exit_address = self.exit_address;
                let old_break_address = self.break_address;
                let old_entry_address = self.entry_address;

                self.entry_address = self.address;
                self.exit_address = self.address + thunk_len;
                self.break_address = self.exit_address;

                let mut body = if let Some(condition) = condition {
                    let mut body = self.flatten(*condition);
                    body.push(Inst::PopCmp);
                    self.address += 1;
                    body
                } else {
                    Vec::new()
                };

                body.push(Inst::JumpFalse(self.exit_address));
                self.address += 1;

                body.extend(self.flatten(*thunk));
                body.push(Inst::Jump(self.entry_address));
                self.address += 1;

                self.entry_address = old_entry_address;
                self.break_address = old_break_address;
                self.exit_address = old_exit_address;
                body
            }

            Thunk::Continue => {
                self.address += 1;
                vec![Inst::Jump(self.entry_address)]
            },
            Thunk::Break => {
                self.address += 1;
                vec![Inst::Jump(self.break_address)]
            },
            Thunk::Nop => vec![],
        }
    }
}

