#![allow(unused)]
pub use crate::either::Either;
use crate::{
  ast::{Parseable, ParsingContext},
  token::{Token, TokenExt, TokenStream},
};
pub use fancy_regex::Regex;

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

pub fn str_parse_file<T: Parseable>(path: &str) -> Result<T, String> {
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
    .and_then(|file| {
      str_parse::<T>(
        String::from_utf8(file)
          .expect("Failed to parse as utf8 text")
          .as_str(),
      )
    })
}

pub fn str_parse<T: Parseable>(input: &str) -> Result<T, String> {
  let mut logger = Logger { logs: vec![] };
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream")?;
  let mut context = ParsingContext::new();

  T::parse(&mut stream, &mut context).map_err(|err| format!("{}", err))
}
