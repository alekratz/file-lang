#[macro_use]
mod common;
#[macro_use]
mod syn;
mod compile;
mod vm;

use crate::{
    common::span::*,
    compile::Compile,
    vm::{fun::Fun, value::Value, Inst, Vm},
};
use matches::matches;
use std::{fs, path::PathBuf, rc::Rc};
use structopt::StructOpt;
use syn::prelude::*;

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
    },
    Run {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
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
        println!(
            "{: <15}line {}, index {}",
            token.kind().to_string(),
            span.start.line + 1,
            span.start.source
        );
        if token.kind() == TokenKind::Eof {
            break;
        }
    }
    Ok(())
}

fn dump_bytecode(text: &str) -> Result<()> {
    use vm::prelude::*;
    let (main_fun, pool) = Compile::new().compile(text)?;

    let (funs, const_values): (Vec<_>, Vec<_>) = pool
        .const_pool()
        .iter()
        .partition(|value| matches!(value, Value::Fun(_)));

    let funs: Vec<_> = funs
        .into_iter()
        .map(|fun| {
            if let Value::Fun(fun) = fun {
                fun
            } else {
                unreachable!()
            }
        })
        .collect();

    println!(
        "{} functions, {} constant values",
        funs.len(),
        const_values.len()
    );

    println!("= CONSTANTS ====================================================================");
    for c in const_values {
        println!("{:?}", c);
    }
    println!();

    println!("= FUNCTIONS ====================================================================");
    for fun in funs {
        println!("{}", pool.get_binding_name(fun.binding()));
    }
    println!();

    Ok(())
}

fn run_text(text: &str) -> Result<()> {
    use vm::prelude::*;
    let (main_fun, pool) = Compile::new().compile(text)?;
    let mut vm = Vm::new(pool);
    vm.main(&main_fun)?;
    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    match opt {
        Opt::Dump { file, what } => {
            let text = fs::read_to_string(file)?;
            match what {
                Dump::Tokens => dump_tokens(&text)?,
                Dump::Bytecode => dump_bytecode(&text)?,
            }
        }
        Opt::Run { file } => {
            let text = fs::read_to_string(file)?;
            run_text(&text)?;
        }
    }

    Ok(())
}
