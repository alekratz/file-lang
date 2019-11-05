mod binding;
mod builtins;
mod collect;
mod context;
pub mod error;
mod ir;
mod thunk;
mod translate;

use crate::compile::error::*;

pub fn compile(text: &str) -> Result<()> {
    let ir_ctx = translate::ast_to_ir(text)?;
    let _artifact = translate::ir_to_inst(ir_ctx)?;
    Ok(())
}
