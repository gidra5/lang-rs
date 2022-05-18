#![allow(unused)]
pub use crate::{
  ast::{Expression, ExpressionParsingInput},
  common::{buffered_iterator::Buf, group_map_iterator::GroupMapTrait, Buffered},
  either::Either,
  itertools::Itertools,
  parse_operand,
  parse_tokens,
  parseable::{Parseable, ParsingContext},
  token::Token,
};
pub use fancy_regex::Regex;

// pub mod logger;
// pub use logger::*;

pub mod buffered_iterator;
pub use buffered_iterator::*;

pub mod group_map_iterator;
pub use group_map_iterator::*;

pub mod utils;
use owned_chars::OwnedChars;
pub use utils::*;

pub fn str_parse_file<T>(path: &str) -> Result<T::O, String>
where
  T: Parseable<OwnedChars>,
{
  std::fs::read(path)
    .map_err(|err| {
      use std::io::ErrorKind::*;
      (match err.kind() {
        NotFound => "No such file",
        PermissionDenied => {
          "Permission denied, maybe try running as administrator/sudo or add 'executable' flag"
        },
        _ => "Unexpected IO error",
      })
      .to_owned()
    })
    .and_then(|file| str_parse::<T>(String::from_utf8(file).expect("Failed to parse as utf8 text")))
}

pub fn str_parse<T>(input: String) -> Result<T::O, String>
where
  T: Parseable<OwnedChars>,
{
  T::parse(OwnedChars::from_string(input))
    .1
    .ok_or("Failed to parse".to_string())
}

pub fn expr(input: &str) -> Result<Expression, String> {
  let mut errors_t = vec![];
  let mut errors = vec![];
  let mut context = ParsingContext::new();

  let operands = input
    .chars()
    .buffered()
    .group_map(|iter| parse_tokens!(errors_t, iter))
    .filter(|token| token != &Token::Skip)
    .buffered()
    .group_map(|iter| parse_operand!(&mut errors, iter, &mut context))
    .buffered();
  let (i, o) = <Expression as Parseable<_>>::parse(ExpressionParsingInput {
    operands,
    context: ParsingContext::new(),
    errors: vec![],
  });

  match &i.errors[..] {
    [] => match o {
      Some(o) => return Ok(o),
      None => return Err("failed to parse, no errors".to_string()),
    },
    errors => return Err(errors.iter().map(|x| x.msg.clone()).join("\n")),
  }
}
