#![allow(non_snake_case)]
use std::ops::Range;
use std::{env, fs};

use chumsky::{input::Stream, prelude::*};
use lazy_static::lazy_static;
use logos::Logos;
use miette::{miette, LabeledSpan};
use parser::{parser, LogosToken};

use crate::vm::VM;

mod analyzer;
mod parser;
mod vm;

lazy_static! {
    pub static ref SRC: String = fs::read_to_string(
        &env::args()
            .collect::<Vec<_>>()
            .get(1)
            .unwrap_or(&String::from("main.sl"))
            .to_string()
    )
    .unwrap_or_else(|_| {
        println!("Error: Input file could not be read");
        std::process::exit(1);
    }) + "\n";
}

fn main() {
    let token_iter = LogosToken::lexer(&SRC)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());

    match parser().parse(token_stream).into_result() {
        Ok(stuff) => {
            analyzer::analyze(stuff.clone());
            let mut vm = VM::new(stuff);
            vm.compile();
        }
        Err(errs) => {
            for err in errs {
                let span: Range<usize> = (*err.span()).into();
                let reason = err.reason().to_string();
                println!(
                    "{:?}",
                    miette!(
                        labels = vec![LabeledSpan::at(span, reason)],
                        "Parsing error"
                    )
                    .with_source_code(SRC.clone())
                );
            }
        }
    };
}
