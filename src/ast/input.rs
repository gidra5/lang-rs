use crate::{common::Buffered, errors::ParsingError, parseable::ParsingContext, token::Token};

#[derive(Clone, Debug)]
pub struct ParsingInput<T: Iterator<Item = Token>> {
  pub tokens:  Buffered<T>,
  pub context: ParsingContext,
  pub errors:  Vec<ParsingError>,
}

impl<I: Iterator<Item = Token>> Iterator for ParsingInput<I> {
  type Item = Token;
  fn next(&mut self) -> Option<Self::Item> { self.tokens.next() }
}
