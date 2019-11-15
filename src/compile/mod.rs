mod binding;
mod builtins;
mod collect;
mod constant;
mod context;
pub mod error;
mod ir;
mod thunk;
mod translate;

use crate::{
    compile::error::*,
    vm::artifact::Artifact,
};

pub fn compile(text: &str) -> Result<Artifact> {
    let mut ir_ctx = translate::ast_to_ir(text)?;
    let artifact = translate::ir_to_inst(&mut ir_ctx);
    Ok(artifact)
}
