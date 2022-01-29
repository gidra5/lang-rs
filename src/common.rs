#![allow(unused)]
pub use crate::{ast::*, either::Either, token::*};
use fancy_regex::Regex;

#[path = "tests/common.rs"]
pub mod tests;

pub mod value;
pub use value::*;

pub mod logger;
pub use logger::*;

pub mod reversable_iterator;
pub use reversable_iterator::*;

pub mod char_stream;
pub use char_stream::*;

pub mod utils;
pub use utils::*;

impl ReversableStream<TokenExt> {
  pub fn check2(&mut self, token: Token) -> bool {
    if self.peek().map(|t| t.token == token) == Some(true) {
      self.next();
      true
    } else {
      false
    }
  }
}
