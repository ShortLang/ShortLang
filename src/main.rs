#![allow(non_snake_case, dead_code)]

use clap::Parser;
use optimizer::Optimizer;
use std::fs;

mod optimizer;
use logos::Logos;
use miette::{miette, Severity};
use parser::{LogosToken, PParser};

use crate::vm::VM;

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

    /// Benchmarks the duration it takes for the program to run
    #[clap(short, long)]
    benchmark: bool,

    /// Formats the input file to be as short as possible
    #[clap(short, long)]
    format: bool,
}

fn format_duration(duration: std::time::Duration) -> String {
    let total_secs = duration.as_secs();
    let nano_secs = duration.subsec_nanos() as f64 / 1_000_000_000.0;

    if total_secs > 0 {
        if total_secs >= 60 {
            let mins = total_secs / 60;
            let secs = total_secs % 60;
            format!("{}m {}s", mins, secs)
        } else {
            format!("{:.2}s", total_secs as f64 + nano_secs)
        }
    } else if duration.subsec_millis() > 0 {
        format!(
            "{:.2}ms",
            duration.subsec_millis() as f64 + nano_secs * 1_000.0
        )
    } else if duration.subsec_micros() > 0 {
        format!(
            "{:.2}μs",
            duration.subsec_micros() as f64 + nano_secs * 1_000_000.0
        )
    } else {
        format!("{}ns", duration.subsec_nanos())
    }
}

fn main() {
    let args = Args::parse();
    let std_lib = include_str!("../std/std.sl").to_owned();
    let src = std_lib
        + "\n"
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
    ast = Optimizer::new(ast.clone()).optimize_all();
    if args.ast {
        println!(
            "{:?}",
            miette!(severity = Severity::Advice, "AST: {:?}", ast,)
        );
    }

    let mut vm = VM::new(&src, ast);
    if args.benchmark {
        let start = std::time::Instant::now();
        vm.compile();
        vm.run();
        let run_time = format_duration(start.elapsed());
        println!(
            "\n{:?}",
            miette!(
                severity = Severity::Advice,
                "Program finished in {}",
                run_time
            )
        );
    } else {
        vm.compile();
        vm.run();
    }
}
