use crate::{
    common::span::*,
    compile::{pool::Pool, Operators},
    syn::prelude::*,
    vm::prelude::*,
};

pub struct Translate<'text, 'pool> {
    text: &'text str,
    pool: &'pool mut Pool,
    bin_ops: &'pool mut Operators,
    un_ops: &'pool mut Operators,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprCtx {
    Stmt,
    Push,
    Lhs,
    Return,
}

impl<'text, 'pool> Translate<'text, 'pool> {
    pub fn translate(
        ast: Vec<Stmt>,
        text: &'text str,
        pool: &'pool mut Pool,
        bin_ops: &'pool mut Operators,
        un_ops: &'pool mut Operators,
    ) -> Vec<Inst> {
        let mut translate = Translate {
            text,
            pool,
            bin_ops,
            un_ops,
        };
        let mut body = translate.translate_body(ast);
        body.push(Inst::Halt);
        body
    }

    pub fn translate_body(&mut self, ast: Vec<Stmt>) -> Vec<Inst> {
        ast.into_iter()
            .flat_map(|stmt| self.translate_stmt(stmt))
            .collect()
    }

    fn translate_stmt(&mut self, stmt: Stmt) -> Vec<Inst> {
        match stmt {
            Stmt::Assign(assign) => self.translate_assign(assign),
            Stmt::Expr(e) => self.translate_expr(e, ExprCtx::Stmt),
            Stmt::FunDef(def) => {
                self.translate_fun_def(def);
                vec![]
            }
            Stmt::Retn(r) => {
                let mut body = Vec::new();
                if let Some(expr) = r.expr {
                    body.extend(self.translate_expr(expr, ExprCtx::Return));
                }
                body.push(Inst::Return);
                body
            }
        }
    }

    fn translate_assign(&mut self, assign: Assign) -> Vec<Inst> {
        let mut body = Vec::new();
        // evaluate rhs, lhs, and then find a place to store the value
        body.extend(self.translate_expr(assign.rhs, ExprCtx::Push));
        // TODO : augmented assign operators
        body.extend(self.translate_expr(assign.lhs, ExprCtx::Lhs));
        // PopStore, Store, or StoreReturn are all implemented by the expr translation functions
        body
    }

    fn translate_expr(&mut self, expr: Expr, ctx: ExprCtx) -> Vec<Inst> {
        match expr {
            Expr::FunCall(f) => self.translate_fun_call_expr(*f, ctx),
            Expr::Un(u) => self.translate_un_expr(*u, ctx),
            Expr::Bin(b) => self.translate_bin_expr(*b, ctx),
            Expr::Atom(a) => self.translate_atom_expr(*a, ctx),
        }
    }

    fn translate_fun_call_expr(&mut self, fun_call: FunCall, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        // evaluate expressions, evaluate function call, then try to call it
        for expr in fun_call.args.into_iter() {
            body.extend(self.translate_expr(expr, ExprCtx::Push));
        }
        body.extend(self.translate_expr(fun_call.fun, ExprCtx::Push));
        body.push(Inst::PopCall);
        body
    }

    fn translate_un_expr(&mut self, un: UnExpr, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        body.extend(self.translate_expr(un.expr, ExprCtx::Push));
        let op_binding = if let Some(binding) = self.un_ops.get(&un.op.kind) {
            *binding
        } else {
            unimplemented!("TODO: handle unknown un_ops");
        };
        match ctx {
            // discard top of stack result
            ExprCtx::Stmt => body.push(Inst::Pop(1)),
            // return value is stored on top of the stack, so we have a push already
            ExprCtx::Push => {}
            // return value is stored on top of the stack and LHS needs to be set
            ExprCtx::Lhs => body.push(Inst::PopStore),
            // pop and store return value
            ExprCtx::Return => body.push(Inst::StoreReturn),
        }
        body
    }

    fn translate_bin_expr(&mut self, bin: BinExpr, ctx: ExprCtx) -> Vec<Inst> {
        let mut body = Vec::new();
        body.extend(self.translate_expr(bin.lhs, ExprCtx::Push));
        body.extend(self.translate_expr(bin.rhs, ExprCtx::Push));
        let op_binding = if let Some(binding) = self.bin_ops.get(&bin.op.kind) {
            *binding
        } else {
            unimplemented!("TODO: handle unknown bin_ops");
        };
        body.push(Inst::Load(op_binding));
        body.push(Inst::PopCall);

        match ctx {
            // discard top of stack result
            ExprCtx::Stmt => body.push(Inst::Pop(1)),
            // return value is stored on top of the stack, so we have a push already
            ExprCtx::Push => {}
            // return value is stored on top of the stack and LHS needs to be set
            ExprCtx::Lhs => body.push(Inst::PopStore),
            // pop and store return value
            ExprCtx::Return => body.push(Inst::StoreReturn),
        }

        body
    }

    fn translate_atom_expr(&mut self, atom: Atom, ctx: ExprCtx) -> Vec<Inst> {
        let text = atom.text(self.text);
        let value = match atom.kind {
            AtomKind::Expr(e) => return self.translate_expr(e, ctx),
            AtomKind::Ident(binding) => match ctx {
                // idents are a little different
                ExprCtx::Stmt => return vec![],
                ExprCtx::Push => return vec![Inst::Load(binding)],
                ExprCtx::Return => return vec![Inst::Load(binding), Inst::StoreReturn],
                ExprCtx::Lhs => return vec![Inst::Store(binding)],
            },
            AtomKind::String => {
                let ref_id = self.pool.insert_const(Value::String(text.to_string()));
                CopyValue::ConstRef(ref_id)
            }
            AtomKind::DecInt => CopyValue::Int(text.parse().expect("invalid int text")),
            AtomKind::BinInt => {
                CopyValue::Int(i64::from_str_radix(&text[2..], 2).expect("invalid binary int text"))
            }
            AtomKind::OctInt => {
                CopyValue::Int(i64::from_str_radix(&text[2..], 8).expect("invalid octal int text"))
            }
            AtomKind::HexInt => {
                CopyValue::Int(i64::from_str_radix(&text[2..], 16).expect("invalid hex int text"))
            }
            AtomKind::Real => CopyValue::Real(text.parse().expect("invalid real number text")),
        };

        match ctx {
            ExprCtx::Stmt => vec![],
            ExprCtx::Push => vec![Inst::PushValue(value)],
            ExprCtx::Lhs => vec![Inst::PushValue(value), Inst::PopStore],
            ExprCtx::Return => vec![Inst::PushValue(value), Inst::StoreReturn],
        }
    }

    fn translate_fun_def(&mut self, fun_def: FunDef) -> FunRef {
        let name = fun_def.name.clone();
        let params: Vec<_> = fun_def
            .params
            .into_iter()
            .map(|(binding, _)| binding)
            .collect();
        let binding = fun_def.binding;
        let registers: CopyValuePool = fun_def
            .bindings
            .values()
            .map(|v| (*v, CopyValue::Empty))
            .collect();
        let mut code = self.translate_body(fun_def.body);
        if code.last().copied() != Some(Inst::Return) {
            code.push(Inst::Return);
        }
        let fun = UserFun::new(name, params, binding, code, registers);
        self.pool.insert_fun(Fun::User(fun))
    }
}
