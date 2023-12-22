#![allow(non_snake_case, dead_code)]

use std::ops::Range;
use std::{env, fs};

use chumsky::{input::Stream, prelude::*};
use logos::Logos;
use miette::{miette, LabeledSpan};
use parser::{parser, LogosToken};

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

    let token_iter = LogosToken::lexer(&src)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((src.len()..src.len()).into());

    match parser().parse(token_stream).into_result() {
        Ok(parsed_exprs) => {
            // analyzer::Analyzer::new(&src, stuff.clone()).analyze();
            let mut vm = VM::new(&src, parsed_exprs);
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
                    .with_source_code(src.clone())
                );
            }
        }
    };
}
