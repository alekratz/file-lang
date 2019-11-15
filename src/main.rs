#[macro_use]
mod common;
#[macro_use]
mod syn;
mod compile;
mod vm;

use crate::{
    common::span::*,
    vm::state::State,
};
use std::{fs, path::PathBuf};
use structopt::StructOpt;
use syn::prelude::*;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

#[derive(Debug, StructOpt)]
#[structopt(about = "file language")]
struct Opt {
    #[structopt(skip)]
    dump: String,

    #[structopt(parse(from_os_str))]
    file: PathBuf,
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

fn dump_tree(text: &str) -> Result<()> {
    let mut parser = Parser::new(text)?;
    let body = parser.next_body()?;
    println!("{:#?}", body);
    Ok(())
}

fn run_text(text: &str) -> Result<()> {
    let artifact = compile::compile(&text)?;
    let mut vm_state = State::from(artifact);
    vm_state.execute();
    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    let text = fs::read_to_string(&opt.file)?;

    match opt.dump.as_str() {
        "" => {}
        "tokens" => dump_tokens(&text)?,
        "tree" => dump_tree(&text)?,
        bad => println!("WARNING: unknown dump option '{}'", bad),
    }

    run_text(&text)
}
