mod binding;
mod collect;
mod context;
pub mod error;
mod ir;
mod translate;

use crate::compile::error::*;

pub fn compile(text: &str) -> Result<()> {
    let _ir = translate::ast_to_ir(text)?;
    Ok(())
}
