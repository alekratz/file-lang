#[macro_use] mod common;
mod syn;

use crate::common::span::*;
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
#[structopt(about = "Dump the tokens from a file")]
enum Dump {
    Tokens,
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

fn main() -> Result<()> {
    let opt = Opt::from_args();

    match opt {
        Opt::Dump { file, what, } => {
            let text = fs::read_to_string(file)?;
            match what {
                Dump::Tokens => dump_tokens(&text)?,
            }
        },
    }

    Ok(())
}
