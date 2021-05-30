#![allow(unused)]
pub use crate::{ast::*, either::Either, token::*};
use fancy_regex::Regex;

#[path = "tests/common.rs"]
mod tests;

pub mod value;
pub use value::*;

pub mod logger;
pub use logger::*;

pub mod reversable_iterator;
pub use reversable_iterator::*;

pub mod char_stream;
pub use char_stream::*;

impl ReversableStream<TokenExt<'_>> {
  pub fn check2(&mut self, token: Token) -> bool {
    if self.peek().map(|t| t.token == token) == Some(true) {
      self.next();
      true
    } else {
      false
    }
  }
}
