#![allow(non_snake_case)]
use std::ops::Range;
use std::{env, fs};

use chumsky::{input::Stream, prelude::*};
use logos::Logos;
use miette::{miette, LabeledSpan};
use parser::{parser, LogosToken};
use regex::Regex;
mod analyzer;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let default_filename = String::from("main.sl");
    let filename = args.get(1).unwrap_or(&default_filename);

    let src = fs::read_to_string(filename).unwrap_or_else(|_| {
        println!("Error: Input file could not be read");
        std::process::exit(1);
    }) + "\n";
    let comment_regex = Regex::new(r"//.*").unwrap();
    if comment_regex.is_match(&src) {
        println!(
            "{:?}",
            miette!(
                severity = miette::Severity::Warning,
                "Input file contains comments, this is not recommended."
            )
        );
    }

    let token_iter = LogosToken::lexer(&src)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((src.len()..src.len()).into());

    match parser().parse(token_stream).into_result() {
        Ok(stuff) => analyzer::analyze(&src, stuff),
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
