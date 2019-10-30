use crate::{common::span::*, compile::{context::*, ir::*}, syn::ast};

struct CollectBindings<'t, 'ctx> {
    ctx: &'ctx mut SynCtx<'t>,
}

impl<'t, 'ctx> CollectBindings<'t, 'ctx> {
    pub fn new(ctx: &'ctx mut SynCtx<'t>) -> Self {
        CollectBindings { ctx, }
    }

    pub fn collect_bindings(mut self) {
        let ast = self.ctx.ast();
        self.collect_body(&*ast);
    }

    fn collect_body(&mut self, body: &Vec<ast::Stmt>) {
        for stmt in body.iter() {
            self.collect_stmt(stmt);
        }
    }

    fn collect_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::TypeDef(type_def) => self.collect_type_def(type_def),
            ast::Stmt::Assign(assign) => self.collect_assign(assign),
            ast::Stmt::Expr(_expr) => {}
            ast::Stmt::FunDef(fun_def) => self.collect_fun_def(fun_def),
            ast::Stmt::Retn(_retn) => {}
            ast::Stmt::If(if_) => self.collect_if(if_),
            ast::Stmt::While(while_) => self.collect_while(while_),
            ast::Stmt::Loop(loop_) => self.collect_loop(loop_),
            ast::Stmt::Ctu(_) | ast::Stmt::Brk(_) => { /* no-op */ }
        }
    }

    fn collect_type_def(&mut self, type_def: &ast::TypeDef) {
        self.ctx.bindings_mut().get_or_create_local_binding(&type_def.name);
    }

    fn collect_fun_def(&mut self, fun_def: &ast::FunDef) {
        self.ctx.bindings_mut().get_or_create_local_binding(&fun_def.name);
    }

    fn collect_assign(&mut self, assign: &ast::Assign) {
        self.collect_lvalue(&assign.lhs);
    }

    fn collect_lvalue(&mut self, lvalue: &ast::Expr) {
        // Collect lvalues
        match lvalue {
            ast::Expr::Atom(atom) => match &atom.kind {
                ast::AtomKind::Ident => {
                    let name = atom.text(self.ctx.text());
                    self.ctx.bindings_mut().get_or_create_local_binding(name);
                }
                ast::AtomKind::Expr(e) => self.collect_lvalue(e),
                _ => { /* no-op */ }
            },
            ast::Expr::Access(access) => {
                self.collect_lvalue(&access.head);
            }
            _ => { /* no-op */ }
        }
    }

    fn collect_if(&mut self, if_: &ast::If) {
        self.collect_body(&if_.condition_body.body);
        for elif in if_.elif_bodies.iter() {
            self.collect_body(&elif.body);
        }
        self.collect_body(&if_.else_body);
    }

    fn collect_while(&mut self, while_: &ast::While) {
        self.collect_body(&while_.condition_body.body);
    }

    fn collect_loop(&mut self, loop_: &ast::Loop) {
        self.collect_body(&loop_.body);
    }
}

pub fn collect_ast<'t, 'ctx>(ctx: &'ctx mut SynCtx<'t>) {
    CollectBindings::new(ctx)
        .collect_bindings();
        //.collect_constants();
}

/*
struct CollectConstants<'t, 'ctx> {
    ctx: &'ctx mut SynCtx<'t>
}

impl<'t, 'ctx> CollectConstants<'t, 'ctx> {
    pub fn new(ctx: &'ctx mut SynCtx<'t>) -> Self {
        CollectConstants { ctx }
    }

    pub fn collect_constants(mut self) {
        self.collect_body(&self.ctx.ast());
    }

    fn collect_body(&mut self, body: &Vec<ast::Stmt>) {
        body.iter().for_each(|stmt| self.collect_stmt(stmt));
    }

    fn collect_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt {
            ast::Stmt::TypeDef(type_def) => self.collect_type_def(type_def),
            ast::Stmt::Assign(assign) => self.collect_assign(assign),
            ast::Stmt::Expr(expr) => self.collect_expr(expr),
            ast::Stmt::FunDef(fun_def) => self.collect_fun_def(fun_def),
            ast::Stmt::Retn(retn) => self.collect_retn(retn),
            ast::Stmt::If(if_) => self.collect_if(if_),
            ast::Stmt::While(while_) => self.collect_while(while_),
            ast::Stmt::Loop(loop_) => self.collect_loop(loop_),
            ast::Stmt::Ctu(_) | ast::Stmt::Brk(_) => {}
        }
    }

    fn collect_type_def(&mut self, type_def: &ast::TypeDef) {
        unimplemented!("TODO: collect type defs")
    }

    fn collect_fun_def(&mut self, fun_def: &ast::FunDef) {
    }

    fn collect_assign(&mut self, assign: &ast::Assign) {
        self.collect_expr(&assign.lhs);
        self.collect_expr(&assign.rhs);
    }

    fn collect_expr(&mut self, expr: &ast::Expr) {
        match expr {
            ast::Expr::Bin(bin) => {
                self.collect_expr(&bin.lhs);
                self.collect_expr(&bin.rhs);
            }
            ast::Expr::Un(un) => self.collect_expr(&un.expr),
            ast::Expr::Access(access) => self.collect_access(access),
            ast::Expr::FunCall(fun_call) => self.collect_fun_call(fun_call),
            ast::Expr::Atom(atom) => self.collect_atom(atom),
        }
    }

    fn collect_access(&mut self, access: &ast::Access) {
        self.collect_expr(&access.head);
        self.collect_atom(&access.tail);
    }

    fn collect_fun_call(&mut self, fun_call: &ast::FunCall) {
        self.collect_expr(&fun_call.fun);
        for arg in fun_call.args.iter() {
            self.collect_expr(arg);
        }
    }

    fn collect_atom(&mut self, atom: &ast::Atom) {
        match &atom.kind {
            ast::AtomKind::Expr(e) => self.collect_expr(e),
            ast::AtomKind::String => { unimplemented!("TODO: collect strings"); }
            ast::AtomKind::RawString => { unimplemented!("TODO: collect strings"); }
            ast::AtomKind::TaggedString => { unimplemented!("TODO: collect strings"); }
            _ => {}
        }
    }

    fn collect_retn(&mut self, retn: &ast::Retn) {
        if let Some(expr) = &retn.expr {
            self.collect_expr(expr);
        }
    }

    fn collect_if(&mut self, if_: &ast::If) {
        self.collect_condition_body(&if_.condition_body);
        for elif in if_.elif_bodies.iter() {
            self.collect_condition_body(elif);
        }
        self.collect_body(&if_.else_body);
    }
    
    fn collect_while(&mut self, while_: &ast::While) {
        self.collect_condition_body(&while_.condition_body);
    }

    fn collect_loop(&mut self, loop_: &ast::Loop) {
        self.collect_body(&loop_.body);
    }

    fn collect_condition_body(&mut self, condition_body: &ast::ConditionBody) {
        self.collect_expr(&condition_body.condition);
        self.collect_body(&condition_body.body);
    }
}
*/
