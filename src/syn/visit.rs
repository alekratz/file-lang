use crate::{
    syn::ast::*,
    common::visit::*,
};

pub struct DumpAst {
    depth: usize,
}

impl Visit<Expr> for DumpAst {
    type Out = ();

    fn visit(&mut self, expr: &Expr) {
        println!("{}{:?}", " ".repeat(self.depth), expr);
    }
}
