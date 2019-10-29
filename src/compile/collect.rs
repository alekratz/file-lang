use crate::{common::span::*, compile::context::*, syn::ast};

struct CollectBindings<'ctx> {
    ctx: SynCtx<'ctx>,
}

impl<'ctx> CollectBindings<'ctx> {
    pub fn new(ctx: SynCtx<'ctx>) -> Self {
        CollectBindings { ctx, }
    }

    pub fn collect_bindings(mut self) -> CollectDefs<'ctx> {
        let ast = self.ctx.ast();
        self.collect_body(&*ast);
        CollectDefs { ctx: self.ctx }
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

    fn collect_fun_def(&mut self, fun_def: &ast::FunDef) {
        self.ctx.bindings_mut().get_or_create_local_binding(&fun_def.name);
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

struct CollectDefs<'ctx> {
    ctx: SynCtx<'ctx>
}

impl<'ctx> CollectDefs<'ctx> {
    pub fn new(ctx: SynCtx<'ctx>) -> Self {
        CollectDefs { ctx }
    }

    pub fn collect_defs(mut self) -> SynCtx<'ctx> {
        for stmt in self.ctx.ast().iter() {
            match stmt {
                ast::Stmt::TypeDef(type_def) => {
                    self.ctx.bindings_mut().get_or_create_local_binding(&type_def.name);
                }
                ast::Stmt::FunDef(fun_def) => {
                    self.ctx.bindings_mut().get_or_create_local_binding(&fun_def.name);
                }
                _ => {}
            }
        }
        self.ctx
    }
}

pub fn collect<'ctx>(text: &'ctx str, ast: Vec<ast::Stmt>) -> SynCtx<'ctx> {
    let ctx = SynCtx::new(text, ast);
    collect_bindings(ctx)
        .collect_defs()
}

fn collect_bindings<'ctx>(ctx: SynCtx<'ctx>) -> CollectDefs {
    CollectBindings::new(ctx).collect_bindings()
}
