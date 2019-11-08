use crate::{
    compile::{constant::*, context::*, error::*, ir::*, thunk::*},
    vm::{artifact::*, fun::UserFun, inst::Inst, value::*},
};
use std::collections::HashMap;

pub fn ir_to_inst<'t>(mut ctx: IrCtx<'t>) -> Result<Artifact> {
    IrToInst::new(ctx).translate()
}

/// A context for an expression to be used in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Push,
    Stmt,
    Return,
}

struct IrToInst<'t> {
    ctx: IrCtx<'t>,
    string_index: HashMap<String, ConstRef>,
}

impl<'t> IrToInst<'t> {
    fn new(ctx: IrCtx<'t>) -> Self {
        IrToInst {
            ctx,
            string_index: Default::default(),
        }
    }

    fn translate(mut self) -> Result<Artifact> {
        // Translate functions
        for (_, fun) in self.ctx.functions().iter() {
            let _user_fun = self.translate_fun(fun)?;
        }
        todo!()
    }

    fn translate_fun(&mut self, _fun_def: &FunDef) -> Result<UserFun> {
        todo!()
    }

    fn translate_body(&mut self, _body: &Vec<Stmt>) -> Result<Thunk> {
        todo!()
    }

    fn translate_stmt(&mut self, stmt: &Stmt) -> Result<Thunk> {
        match stmt {
            Stmt::Assign(_) => todo!(),
            Stmt::Expr(e) => self.translate_expr(e, ExprCtx::Stmt),
            Stmt::Retn(r) => self.translate_retn(r),
            Stmt::Branch(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::Ctu(_) => Ok(Thunk::Continue),
            Stmt::Brk(_) => Ok(Thunk::Break),
            Stmt::Nop(_) => Ok(Thunk::Nop),
        }
    }

    fn translate_expr(&mut self, expr: &Expr, ctx: ExprCtx) -> Result<Thunk> {
        match expr {
            Expr::Bin(b) => self.translate_bin_expr(b, ctx),
            Expr::Un(u) => self.translate_un_expr(u, ctx),
            Expr::Access(a) => self.translate_access(a, ctx),
            Expr::FunCall(f) => self.translate_fun_call(f, ctx),
            Expr::Atom(a) => self.translate_atom(a, ctx),
        }
    }

    fn translate_retn(&mut self, _retn: &Retn) -> Result<Thunk> {
        todo!()
    }

    fn translate_bin_expr(&mut self, _bin_expr: &BinExpr, _ctx: ExprCtx) -> Result<Thunk> {
        todo!()
    }

    fn translate_un_expr(&mut self, _un_expr: &UnExpr, _ctx: ExprCtx) -> Result<Thunk> {
        todo!()
    }

    fn translate_access(&mut self, _access: &Access, _ctx: ExprCtx) -> Result<Thunk> {
        todo!()
    }

    fn translate_fun_call(
        &mut self,
        FunCall { fun, args, .. }: &FunCall,
        ctx: ExprCtx,
    ) -> Result<Thunk> {
        let arg_count = args.len();
        let mut thunk = self.translate_expr(&fun, ExprCtx::Push)?;
        for arg in args.iter() {
            thunk.extend(self.translate_expr(arg, ExprCtx::Push)?);
        }
        thunk.push(Inst::PopCall(arg_count));

        match ctx {
            ExprCtx::Push => thunk.push(Inst::PushReturn),
            ExprCtx::Stmt => thunk.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* no-op - return value is already in the return register */ }
        }

        Ok(thunk)
    }

    fn translate_atom(&mut self, Atom { kind, .. }: &Atom, ctx: ExprCtx) -> Result<Thunk> {
        let mut body = Thunk::default();
        match kind {
            AtomKind::Ident(binding) => body.push(Inst::Load(*binding)),
            AtomKind::String(s) => {
                let ref_id = { self.get_or_register_string_constant(s) };
                body.push(Inst::PushValue(StackValue::ConstRef(ref_id)));
            }
            AtomKind::TaggedString { .. } => todo!("TODO(string) Tagged string behavior"),
            AtomKind::Int(i) => body.push(Inst::PushValue(StackValue::Int(*i as i64))),
            AtomKind::Real(f) => body.push(Inst::PushValue(StackValue::Float(*f))),
        }
        match ctx {
            ExprCtx::Push => { /* no-op - expression should remain on the stack */ }
            ExprCtx::Stmt => {
                body.push(Inst::Pop(1));
            }
            ExprCtx::Return => body.push(Inst::StoreReturn),
        }
        Ok(body)
    }

    fn get_or_register_string_constant(&mut self, s: &str) -> ConstRef {
        // can't do this with the "entry" API because it requires borrowing self mutably twice
        if let Some(const_ref) = self.string_index.get(s) {
            *const_ref
        } else {
            let const_ref = self.ctx.register_constant(s.into());
            self.string_index.insert(s.to_string(), const_ref);
            const_ref
        }
    }
}
