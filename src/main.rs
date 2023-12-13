use chumsky::{
    input::{Stream, ValueInput},
    prelude::*,
};
use logos::Logos;
use parser::{parser, LogosToken};
mod parser;

fn main() {
    const SRC: &str = r##"1 + deez - ("Hello" * 6)9+8"##;
    let token_iter = LogosToken::lexer(SRC)
        .spanned()
        .map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (LogosToken::Error, span.into()),
        });

    let token_stream = Stream::from_iter(token_iter)
        .spanned::<LogosToken, SimpleSpan>((SRC.len()..SRC.len()).into());

    let ast = parser().parse(token_stream).unwrap();
    println!("{ast:?}")
}
