#![allow(non_snake_case, dead_code)]

use std::ops::Range;
use std::{env, fs};

use analyzer::Analyzer;
use logos::Logos;
use miette::{miette, LabeledSpan};
use parser::{LogosToken, PParser};

use crate::vm::VM;

mod analyzer;
mod parser;
mod vm;

fn main() {
    let std_sl = std::str::from_utf8(include_bytes!("std.sl")).expect("Failed to read std.sl file");
    let src = std_sl.to_owned()
        + "\n"
        + &*fs::read_to_string(
            env::args()
                .collect::<Vec<_>>()
                .get(1)
                .unwrap_or(&String::from("main.sl")),
        )
        .unwrap_or_else(|_| {
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
    let ast = parser.parse();
    Analyzer::new(&src, ast.clone()).analyze();
    let mut vm = VM::new(&src, ast);
    vm.compile();
    vm.run();
}
