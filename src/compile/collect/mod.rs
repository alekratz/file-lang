mod ast;

pub use self::ast::*;

use crate::compile::{builtins::BUILTIN_TYPES, context::AstCtx};

pub fn collect_builtins<'t>(ctx: &mut AstCtx<'t>) {
    for (name, _) in BUILTIN_TYPES.iter() {
        ctx.bindings_mut().create_builtin_binding(name.to_string());
    }

    // TODO : Collect builtin functions
}
