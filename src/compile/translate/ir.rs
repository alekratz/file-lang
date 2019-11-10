use crate::{
    compile::{builtins::*, constant::*, context::*, error::*, ir::*, thunk::*},
    vm::{artifact::*, fun::UserFun, inst::Inst, value::*},
};
use std::collections::HashMap;

pub fn ir_to_inst<'t, 'ctx>(ctx: &'ctx mut IrCtx<'t>) -> Artifact {
    IrToInst::new(ctx).translate()
}

/// A context for an expression to be used in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Push,
    Stmt,
    Return,
}

struct IrToInst<'t, 'ctx> {
    ctx: &'ctx mut IrCtx<'t>,
    string_index: HashMap<String, ConstRef>,
}

impl<'t, 'ctx> IrToInst<'t, 'ctx> {
    fn new(ctx: &'ctx mut IrCtx<'t>) -> Self {
        IrToInst {
            ctx,
            string_index: Default::default(),
        }
    }

    fn translate(mut self) -> Artifact {
        // Translate functions
        // TODO : register builtin functions as constants
        // TODO : register builtin types as constants
        // TODO : delve into user functions, register them
        for (_, fun) in self.ctx.functions().iter() {
            let _user_fun = self.translate_fun(fun);
        }
        // TODO : delve into user types, register them
        for (_, ty) in self.ctx.types().iter() {
            self.translate_type(ty);
        }
        let main_thunk = self.translate_body(&self.ctx.ir());
        let main_binding = self.ctx.bindings_mut().create_binding("__main__".to_string());
        let main_bindings: Vec<_> = self.ctx.bindings()
            .last_layer()
            .unwrap()
            .iter()
            .map(|(_, binding)| *binding)
            .collect();
        let main_fun = UserFun::new(main_binding, main_bindings, main_thunk.into(), 0);
        let _main_fun_ref = self.ctx.register_constant(ConstValue::UserFun(main_fun));
        todo!("TODO : convert constant values into live object values")
    }

    fn translate_type(&mut self, _type_def: &TypeDef) {
        // TODO this should be in its own pass, probably in compile::collect somewhere
        todo!()
    }

    fn translate_fun(&mut self, fun_def: &FunDef) -> UserFun {
        let body = {
            let mut translator = IrToInst::new(self.ctx);
            translator.translate_body(&fun_def.body)
        };
        todo!()
        //UserFun::new(fun_def.binding, 
    }

    fn translate_body(&mut self, body: &Vec<Stmt>) -> Thunk {
        body.iter()
            .map(|stmt| self.translate_stmt(stmt))
            .collect::<Vec<_>>()
            .into()
    }

    fn translate_stmt(&mut self, stmt: &Stmt) -> Thunk {
        match stmt {
            Stmt::Assign(a) => self.translate_assign(a),
            Stmt::Expr(e) => self.translate_expr(e, ExprCtx::Stmt),
            Stmt::Retn(r) => self.translate_retn(r),
            Stmt::Branch(_) => todo!(),
            Stmt::Loop(_) => todo!(),
            Stmt::Ctu(_) => Thunk::Continue,
            Stmt::Brk(_) => Thunk::Break,
            Stmt::Nop(_) => Thunk::Nop,
        }
    }

    fn translate_assign(&mut self, Assign { lhs, op, rhs, .. }: &Assign) -> Thunk {
        let mut thunk = Thunk::default();
        if let Some(op) = op {
            match lhs {
                LValue::Ident(_, binding) => thunk.push(Inst::Load(*binding)),
                LValue::Access(access) => {
                    thunk.extend(self.translate_access(access, ExprCtx::Push));
                }
                LValue::Complex(expr) => {
                    thunk.extend(self.translate_expr(expr, ExprCtx::Push));
                }
            }
            thunk.extend(self.translate_expr(rhs, ExprCtx::Push));
            thunk.extend(vec![Inst::Load(*op), Inst::PopCall(2)]);
        } else {
            match lhs {
                LValue::Ident(_, binding) => {
                    thunk.extend(self.translate_expr(rhs, ExprCtx::Push));
                    thunk.push(Inst::Store(*binding));
                }
                LValue::Access(access) => {
                    // TODO : Keep track of anonymous variables so that they may be reclaimed
                    let setattr_binding = self.ctx
                        .bindings_mut()
                        .create_anonymous_binding();
                    let setattr_ref = self.get_or_register_string_constant(SETATTR);
                    let attr_ref = self.get_or_register_string_constant(&access.tail);
                    thunk.extend(self.translate_expr(&access.head, ExprCtx::Push));
                    thunk.extend(vec![
                                 Inst::GetAttr(setattr_ref),
                                 Inst::Store(setattr_binding),
                                 Inst::PushValue(attr_ref.into()),
                    ]);
                    // RHS
                    thunk.extend(self.translate_expr(rhs, ExprCtx::Push));
                    thunk.extend(vec![
                                 Inst::Load(setattr_binding),
                                 Inst::PopCall(3),
                    ]);
                }
                LValue::Complex(expr) => {
                    thunk.extend(self.translate_expr(expr, ExprCtx::Push));
                    thunk.extend(self.translate_expr(rhs, ExprCtx::Push));
                    thunk.push(Inst::PopStore);
                }
            }
        }
        thunk
    }

    fn translate_expr(&mut self, expr: &Expr, ctx: ExprCtx) -> Thunk {
        match expr {
            Expr::Bin(b) => self.translate_bin_expr(b, ctx),
            Expr::Un(u) => self.translate_un_expr(u, ctx),
            Expr::Access(a) => self.translate_access(a, ctx),
            Expr::FunCall(f) => self.translate_fun_call(f, ctx),
            Expr::Atom(a) => self.translate_atom(a, ctx),
        }
    }

    fn translate_bin_expr(
        &mut self,
        BinExpr { lhs, op, rhs, .. }: &BinExpr,
        ctx: ExprCtx,
    ) -> Thunk {
        let mut thunk = self.translate_expr(lhs, ExprCtx::Push);
        thunk.extend(self.translate_expr(rhs, ExprCtx::Push));
        thunk.extend(vec![Inst::Load(*op), Inst::PopCall(2)]);

        match ctx {
            ExprCtx::Push => thunk.push(Inst::PushReturn),
            ExprCtx::Stmt => thunk.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* no-op - return value is already in the return register */ }
        }
        thunk
    }

    fn translate_un_expr(
        &mut self,
        UnExpr { op, expr, .. }: &UnExpr,
        ctx: ExprCtx,
    ) -> Thunk {
        // TODO : consider translating this to a function call in the IR, instead of here
        let mut thunk = self.translate_expr(expr, ExprCtx::Push);
        thunk.extend(vec![Inst::Load(*op), Inst::PopCall(1)]);

        match ctx {
            ExprCtx::Push => thunk.push(Inst::PushReturn),
            ExprCtx::Stmt => thunk.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* no-op - return value is already in the return register */ }
        }
        thunk
    }

    fn translate_access(
        &mut self,
        Access { head, tail, .. }: &Access,
        ctx: ExprCtx,
    ) -> Thunk {
        // TODO : consider translating this to a function call in the IR, instead of here
        let mut thunk = self.translate_expr(head, ExprCtx::Push);
        let name_ref = self.get_or_register_string_constant(tail);
        let getattr_ref = self.get_or_register_string_constant(GETATTR);
        thunk.extend(vec![
            Inst::PushValue(StackValue::ConstRef(name_ref)),
            Inst::GetAttr(getattr_ref),
            Inst::PopCall(2),
        ]);
        match ctx {
            ExprCtx::Push => thunk.push(Inst::PushReturn),
            ExprCtx::Stmt => thunk.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* no-op - return value is already in the return register */ }
        }
        thunk
    }

    fn translate_fun_call(
        &mut self,
        FunCall { fun, args, .. }: &FunCall,
        ctx: ExprCtx,
    ) -> Thunk {
        let arg_count = args.len();
        let mut thunk = self.translate_expr(&fun, ExprCtx::Push);
        for arg in args.iter() {
            thunk.extend(self.translate_expr(arg, ExprCtx::Push));
        }
        thunk.push(Inst::PopCall(arg_count));

        match ctx {
            ExprCtx::Push => thunk.push(Inst::PushReturn),
            ExprCtx::Stmt => thunk.push(Inst::DiscardReturn),
            ExprCtx::Return => { /* no-op - return value is already in the return register */ }
        }

        thunk
    }

    fn translate_atom(&mut self, Atom { kind, .. }: &Atom, ctx: ExprCtx) -> Thunk {
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
        body
    }

    fn translate_retn(&mut self, Retn { expr, .. }: &Retn) -> Thunk {
        let mut thunk = if let Some(expr) = expr {
            self.translate_expr(expr, ExprCtx::Return)
        } else {
            Thunk::Block(vec![])
        };
        thunk.push(Inst::Return);
        thunk
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
