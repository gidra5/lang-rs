use crate::{common::Buffered, errors::ParsingError, parseable::ParsingContext, token::Token};


pub struct ParsingInput<T: Iterator<Item = Token> + Clone> {
  pub tokens:  Buffered<T>,
  pub context: ParsingContext,
  pub errors:  Vec<ParsingError>,
}
