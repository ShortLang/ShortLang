#![allow(non_snake_case, dead_code)]

use clap::Parser;
use std::fs;

use analyzer::Analyzer;
use logos::Logos;
use parser::{LogosToken, PParser};

use crate::vm::VM;

mod analyzer;
mod parser;
mod vm;

/// The arguments for the ShortLang compiler
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// The input file to use
    #[clap(name = "FILE", default_value = "main.sl")]
    file: String,

    /// Prints the AST of the input file
    #[clap(short, long)]
    ast: bool,

    /// Silences the analyzer warnings
    #[clap(short, long)]
    silent: bool,

    /// Formats the input file to be as short as possible
    #[clap(short, long)]
    format: bool,
}

fn main() {
    let args = Args::parse();
    let std_lib = include_str!("../std/std.sl").to_owned();
    let src = std_lib
        + &fs::read_to_string(&args.file).unwrap_or_else(|_| {
            println!("Error: Input file could not be read");
            std::process::exit(1);
        });

    let tokens = LogosToken::lexer(&src)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        })
        .collect::<Vec<_>>();

    let mut parser = PParser::new(&src, tokens);
    let mut ast = parser.parse();
    Analyzer::new(&src, &args, &mut ast).analyze();
    if args.ast {
        println!("{:?}", &ast);
    }
    let mut vm = VM::new(&src, ast);
    vm.compile();
    vm.run();
}
