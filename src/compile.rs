use crate::{
    common::{span::*, visit::*},
    syn::prelude::*,
    vm::{Op, Value},
};
use std::{
    collections::{HashMap, HashSet},
    convert::TryFrom,
};

// TODO:
// * constant pool with separate pools for different constant types

pub struct Compile<'text> {
    text: &'text str,
    constants: HashMap<Value, usize>,
    bind_count: usize,
    bindings: Vec<HashMap<String, usize>>,
}

impl<'text> Compile<'text> {
    pub fn new(text: &'text str) -> Self {
        Compile {
            text,
            constants: Default::default(),
            bind_count: Default::default(),
            bindings: vec![Default::default()],
        }
    }

    pub fn compile(&mut self) -> Result<Vec<Op>> {
        let ast = Parser::new(self.text)?.next_body()?;
        self.collect_constants(&ast);
        //self.flatten_expressions(&ast);
        self.create_bindings(&ast);
        unimplemented!()
    }

    fn create_bindings(&mut self, ast: &[Stmt]) {

    }

    /// Collect constants at the current lexical level.
    fn collect_constants(&mut self, ast: &[Stmt]) {
        CollectConstants::collect(self.text, ast, &mut self.constants);
    }
}

/// A visitor that collects constants for a given lexical scope.
struct CollectConstants<'compile, 'text> {
    constants: &'compile mut HashMap<Value, usize>,
    text: &'text str,
}

impl<'compile, 'text> CollectConstants<'compile, 'text> {
    fn collect(text: &'text str, ast: &[Stmt], constants: &'compile mut HashMap<Value, usize>) {
        let mut collector = CollectConstants {
            constants,
            text,
        };
        for stmt in ast {
            collector.visit(stmt);
        }
    }
}

impl Visit<Stmt> for CollectConstants<'_, '_> {
    type Out = ();
    fn visit(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(e) => self.visit(e),
            Stmt::FunDef(f) => {
                /* no-op - we want to stay at this scope level for constant collection */
            }
            Stmt::Retn(r) => {
                if let Some(expr) = r.expr.as_ref() {
                    self.visit(expr);
                }
            }
        }
    }
}

impl Visit<Expr> for CollectConstants<'_, '_> {
    type Out = ();
    fn visit(&mut self, expr: &Expr) {
        if let Expr::Atom(atom) = expr {
            let text = atom.text(self.text);
            let last = self.constants.len();
            match atom.kind {
                AtomKind::String => {
                    self.constants.insert(Value::String(text.to_string()), last);
                }
                AtomKind::DecInt => {
                    let int = text.parse::<i64>().unwrap();
                    self.constants.insert(Value::Int(int), last);
                }
                AtomKind::BinInt => {
                    let int = i64::from_str_radix(&text[2..], 2) // 2.. is ok since it's just ascii
                        .unwrap();
                    self.constants.insert(Value::Int(int), last);
                }
                AtomKind::OctInt => {
                    let int = i64::from_str_radix(&text[2..], 8) // 2.. is ok since it's just ascii
                        .unwrap();
                    self.constants.insert(Value::Int(int), last);
                }
                AtomKind::HexInt => {
                    let int = i64::from_str_radix(&text[2..], 16) // 2.. is ok since it's just ascii
                        .unwrap();
                    self.constants.insert(Value::Int(int), last);
                }
                AtomKind::Real => {
                    unimplemented!("TODO real number values");
                }
                _ => { /* no-op */ }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_collect_constants() {
        const TEXT: &str = r#"
        1;
        0b10;
        0o3;
        0xa;
        "#;
        const EXPECTED: &[Value] = &[Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(10)];
        let ast = Parser::new(TEXT).unwrap().next_body().unwrap();
        let mut compile = Compile::new(TEXT);
        compile.collect_constants(&ast);
        for e in EXPECTED {
            assert!(compile.constants.contains_key(e));
        }
        assert_eq!(EXPECTED.len(), compile.constants.len());
    }
}
