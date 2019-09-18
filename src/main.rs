#[macro_use] mod common;
#[macro_use] mod syn;
mod compile;
mod vm;

use crate::{
    common::span::*,
    compile::Compile,
    vm::{Inst, fun::Fun},
};
use syn::prelude::*;
use structopt::StructOpt;
use std::{fs, path::PathBuf};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug, StructOpt)]
#[structopt(about = "file language")]
enum Opt {
    #[structopt(about = "Transform the syntax in a file and print it to the command line")]
    Dump {
        #[structopt(parse(from_os_str))]
        file: PathBuf,

        #[structopt(subcommand, about = "What kind of dump to perform")]
        what: Dump,
    }
}

#[derive(Debug, Copy, Clone, StructOpt)]
enum Dump {
    #[structopt(about = "Dump the tokens from a file")]
    Tokens,

    #[structopt(about = "Dump translated bytecode")]
    Bytecode,
}

fn dump_tokens(text: &str) -> Result<()> {
    let mut lexer = Lexer::new(text);
    loop {
        let token = lexer.next_token()?;
        let span = token.span();
        println!("{: <15}line {}, index {}", token.kind().to_string(), span.start.line + 1, span.start.source);
        if token.kind() == TokenKind::Eof {
            break;
        }
    }
    Ok(())
}

fn dump_bytecode(text: &str) -> Result<()> {
    let mut compile = Compile::new();
    let main_fun = compile.compile(text)?;
    println!("= CONST POOL ===================================================================");
    for value in compile.pool().const_pool() {
        println!("{:?}", value);
    }
    println!();
    println!("= FUNCTIONS ====================================================================");
    for fun in compile.pool().fun_pool() {
        if let Fun::User(fun) = fun {
            println!("{}", fun.name());
            println!("--------------------------------------------------------------------------------");
            println!();
            for (i, inst) in fun.code().iter().enumerate() {
                let line = format!("{:03} {:?}", i, inst);
                match inst {
                    Inst::Load(binding)
                    | Inst::Store(binding) => {
                        let name = compile.pool().get_binding_name(*binding);
                        println!("{: <30}{: <15?} = {}", line, binding, name);
                    }
                    _ => println!("{}", line),
                }
            }
            println!();
        }
    }
    println!();
    println!("= MAIN FUNCTION ================================================================");
    println!();
    for (i, inst) in main_fun.iter().enumerate() {
        let line = format!("{:03} {:?}", i, inst);
        match inst {
            Inst::Load(binding)
            | Inst::Store(binding) => {
                let name = compile.pool().get_binding_name(*binding);
                println!("{: <30}{: <15?} = {}", line, binding, name);
            }
            _ => println!("{}", line),
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    match opt {
        Opt::Dump { file, what, } => {
            let text = fs::read_to_string(file)?;
            match what {
                Dump::Tokens => dump_tokens(&text)?,
                Dump::Bytecode => dump_bytecode(&text)?,
            }
        },
    }

    Ok(())
}
