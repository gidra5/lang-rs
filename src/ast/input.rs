use crate::{common::Buffered, errors::ParsingError, parseable::ParsingContext, token::Token};

#[derive(Clone)]
pub struct ParsingInput<T: Iterator<Item = Token> + Clone> {
  pub tokens:  Buffered<T>,
  pub context: ParsingContext,
  pub errors:  Vec<ParsingError>,
}

impl<I: Iterator<Item = Token> + Clone> Iterator for ParsingInput<I> {
  type Item = Token;
  fn next(&mut self) -> Option<Self::Item> { self.tokens.next() }
}
