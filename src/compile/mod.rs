mod binding;
mod collect;
mod context;
pub mod error;
mod ir;
mod object;
mod thunk;
mod translate;

use crate::compile::error::*;

pub fn compile(text: &str) -> Result<()> {
    let _ir_ctx = translate::ast_to_ir(text)?;
    Ok(())
}
