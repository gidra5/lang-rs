use crate::{common::Buffered, errors::ParsingError, parseable::ParsingContext, token::Token};


pub struct ParsingInput<T: Iterator<Item = Token> + Clone> {
  tokens:  Buffered<T>,
  context: ParsingContext,
  errors:  Vec<ParsingError>,
}
