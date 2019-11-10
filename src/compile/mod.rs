mod binding;
mod builtins;
mod collect;
mod constant;
mod context;
pub mod error;
mod ir;
mod thunk;
mod translate;

use crate::compile::error::*;

pub fn compile(text: &str) -> Result<()> {
    let mut ir_ctx = translate::ast_to_ir(text)?;
    let _artifact = translate::ir_to_inst(&mut ir_ctx);
    Ok(())
}
