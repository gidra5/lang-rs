use crate::{
  ast::{Expression, ParsingInput},
  common::{buffered_iterator::Buf, Buffered},
  itertools::Itertools,
  parseable::{Parseable, ParseableIterator, Parsed, ParsingContext},
  token::{Token, TokenizationInput},
};
use core::iter::Filter;

use super::str_parse;

pub fn expr(input: &str) -> Result<Expression, String> {
  let tokens: Buffered<Filter<Parsed<_, Token>, _>> =
    TokenizationInput::new(input.chars().buffered())
      .parsed()
      .filter(|token: &Token| token.clone() != Token::Skip)
      .buffered();
  let (i, o) = <Expression as Parseable<_>>::parse(ParsingInput {
    tokens,
    context: ParsingContext::new(),
    errors: vec![],
  });

  match &i.errors[..] {
    [] => match o {
      Some(o) => return Ok(o),
      None => return Err("failed to parse, with no errors".to_string()),
    },
    errors => return Err(errors.iter().map(|x| x.msg.clone()).join("\n")),
  }
}
