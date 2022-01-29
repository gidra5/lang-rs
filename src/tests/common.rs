use crate::common::logger::char_stream::{value::Value, CharStream, Logger, TokenStream};

use super::{
  expr::Expression,
  stmt::{Block, Statement},
  ASTNodeExt,
  Parseable,
};

pub fn str_parse<T: Parseable>(input: &str) -> Result<T, String> {
  let mut logger = Logger { logs: vec![] };
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream")?;

  T::parse(&mut stream)
}

pub fn stmt(input: &str) -> Result<Statement, String> { str_parse::<Statement>(input) }

pub fn expr(input: &str) -> Result<Expression, String> { str_parse::<Expression>(input) }
