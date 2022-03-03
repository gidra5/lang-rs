use crate::{
  ast::{Expression, Parseable, ParsingContext},
  common::{value::Value, CharStream, Logger},
  map,
  token::TokenStream,
};

pub fn str_parse<T: Parseable>(input: &str) -> Result<T, String> {
  let mut logger = Logger { logs: vec![] };
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream")?;
  let mut context = ParsingContext::new();

  T::parse(&mut stream, &mut context).map_err(|err| format!("{}", err))
}

pub fn expr(input: &str) -> Result<Expression, String> { str_parse::<Expression>(input) }
