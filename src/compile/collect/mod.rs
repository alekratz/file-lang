mod ast;

pub use self::ast::*;

use crate::compile::{
    context::SynCtx,
    builtins::BUILTIN_TYPES,
};

pub fn collect_builtins<'t>(ctx: &mut SynCtx<'t>) {
    for (name, _) in BUILTIN_TYPES.iter() {
        ctx.bindings_mut().create_builtin_binding(name.to_string());
    }

    // TODO : Collect builtin functions
}
