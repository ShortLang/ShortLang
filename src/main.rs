#![allow(unused)]

use atty::Stream;
use clap::builder::TypedValueParser;
use clap::Parser;
use optimizer::Optimizer;
use std::io::Write;
use std::{fs, io};

use formatter::Formatter;
use logos::Logos;
use miette::{miette, Severity};
use parser::{LogosToken, PParser};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use shortlang::*;
use vm::VM;

/// The arguments for the ShortLang compiler
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// The input file to use
    #[clap(name = "FILE", default_value = "")]
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

    /// Specifies the format mode, higher means more aggressive
    #[clap(
        name = "mode",
        short,
        long,
        requires = "format",
        default_value = "2",
        value_parser = clap::builder::PossibleValuesParser::new(&["1", "2", "3"])
            .map(|s| s.parse::<usize>().unwrap())
    )]
    format_mode: usize,

    /// Generate docs for standard library
    #[clap(long)]
    doc: bool,
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

fn tokenize(input: &str) -> Vec<(LogosToken, std::ops::Range<usize>)> {
    LogosToken::lexer(input)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        })
        .collect::<Vec<_>>()
}

fn main() {
    let args = Args::parse();
    let std_lib = include_str!("../std/std.sl").to_owned();

    if args.doc {
        println!("# Functions\n");
        println!("{}", VM::get_fn_docs());
        println!("\n# Methods\n");
        println!("{}", VM::get_method_docs());
        return;
    }

    let src = if !args.file.is_empty() {
        fs::read_to_string(&args.file).unwrap_or_else(|_| {
            eprintln!("Error: Input file could not be read");
            std::process::exit(1);
        })
    } else {
        // No input file or stdin specified, start the repl
        let mut rl = DefaultEditor::new().unwrap();
        let mut lines = Vec::new();

        // Only print information if we aren't piping input
        if atty::is(Stream::Stdin) {
            println!(
                "ShortLang v{} on {}",
                env!("CARGO_PKG_VERSION"),
                std::env::consts::OS
            );
            println!("Type 'help', 'credits' or 'license' for more information");
        }

        loop {
            let readline = rl.readline(">>> ");
            match readline {
                Ok(input) => {
                    rl.add_history_entry(input.as_str()); // Add the line to the history
                    match input.trim() {
                        "" => continue,
                        "help" => {
                            println!(
                                "ShortLang v{} on {}",
                                env!("CARGO_PKG_VERSION"),
                                std::env::consts::OS
                            );
                            println!("Type 'help', 'credits' or 'license' for more information");
                            println!("Type 'exit()' or CTRL-C to exit");
                            continue;
                        }
                        "credits" => {
                            println!(
                                "ShortLang was created by {}",
                                env!("CARGO_PKG_AUTHORS").replace(":", ", ")
                            );
                            println!(
                                "For a full list of contributors, visit {}",
                                env!("CARGO_PKG_REPOSITORY")
                            );
                            continue;
                        }
                        "license" => {
                            println!(
                                "ShortLang is licensed under the {} license",
                                env!("CARGO_PKG_LICENSE")
                            );
                            println!("For more information, view the LICENSE file in the root directory of the project");
                            continue;
                        }
                        _ => lines.push(input),
                    }

                    let src = lines.join("\n");
                    let mut ast_std = PParser::new(&std_lib, tokenize(&std_lib)).parse();
                    let mut ast_src = PParser::new(&src, tokenize(&src)).parse();
                    ast_std.append(&mut ast_src);

                    let ast = Optimizer::new(ast_std).optimize_all();
                    let mut vm = VM::new(&src, ast);

                    vm.compile();
                    vm.run();
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    std::process::exit(0);
                }
                Err(ReadlineError::Eof) => {
                    if atty::is(Stream::Stdin) {
                        println!("CTRL-D");
                    }
                    std::process::exit(0)
                }
                Err(err) => {
                    eprintln!("Error: {:?}", err);
                    std::process::exit(1);
                }
            }
        }
    };

    if args.format {
        println!("Formatting: {}", args.file);
        let formatted = Formatter::new(&src, args.format_mode)
            .format_code()
            .unwrap();

        fs::File::create(&args.file)
            .unwrap()
            .write_all(formatted.as_bytes())
            .expect("Cannot write to file");

        println!("Done!");
        println!(
            "{} chars -> {} chars, {:.2}% decrease",
            src.len(),
            formatted.len(),
            100.0 * ((src.len() - formatted.len()) as f32 / src.len() as f32)
        );
        return;
    }

    let mut ast_std = PParser::new(&std_lib, tokenize(&std_lib)).parse();
    let mut ast_src = PParser::new(&src, tokenize(&src)).parse();
    ast_std.append(&mut ast_src);

    let ast = Optimizer::new(ast_std).optimize_all();
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
