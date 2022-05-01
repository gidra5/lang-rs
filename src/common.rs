#![allow(unused)]
use std::{mem, str::Chars};

pub use crate::either::Either;
use crate::{
  parseable::{Parseable, ParseableIterator, ParsingContext},
  token::Token,
};
pub use fancy_regex::Regex;

// #[path = "tests/common.rs"]
// pub mod tests;

// pub mod logger;
// pub use logger::*;

pub mod buffered_iterator;
pub use buffered_iterator::*;

pub mod utils;
pub use utils::*;

pub fn str_parse_file<'a, T, I>(path: &'a str) -> Result<T::O, String>
where
  T: Parseable<I, I = Chars<'a>>,
  I: Iterator<Item = char>,
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
    .and_then(|file| {
      str_parse::<T, I>(String::from_utf8(file).expect("Failed to parse as utf8 text"))
    })
}

pub fn str_parse<'a, T, I>(input: String) -> Result<T::O, String>
where
  T: Parseable<I, I = Chars<'a>>,
  I: Iterator<Item = char>,
{
  let chars = unsafe { mem::transmute(input.chars()) };
  T::parse(chars).1.ok_or("Failed to parse".to_string())
}
