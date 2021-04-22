#![allow(unused)]
pub use crate::either::Either;
use crate::regex::Regex;

#[path = "tests/common.rs"]
mod tests;

pub trait ReversableIterator {
  type Item;

  /// Returns next `size` items in iterator
  fn next_ext(&mut self, size: usize) -> Vec<Self::Item>;

  /// Returns next item
  fn next(&mut self) -> Option<Self::Item> {
    self.next_ext(1).pop()
  }

  /// Returns next `size` items without consuming iterator
  fn peek_ext(&mut self, size: usize) -> Vec<Self::Item>;

  /// Returns next item without consuming iterator
  fn peek(&mut self) -> Option<Self::Item> {
    self.peek_ext(1).pop()
  }

  // fn expect_next<U>(&mut self, item: Self::Item, err: U) -> Result<T, U> {
  //   self.check(|stream| {

  //   })
  // }

  fn check<T, U, F: FnOnce(&mut Self) -> Result<T, U>>(&mut self, f: F) -> Result<T, U> {
    self.remember();

    let res = f(self);

    match res {
      Ok(_) => self.accept(),
      Err(_) => self.decline(),
    };

    res
  }

  /// Remembers current state of iterator.
  /// If called several time in a row
  /// then remembers all of states with which it was called
  fn remember(&mut self);

  /// Forgets last remembered state
  fn accept(&mut self);

  /// Restores last remembered state
  fn decline(&mut self);
}

#[derive(Clone)]
pub struct Span {
  /// File which is spanned
  file: Box<std::path::Path>,

  /// Line in file at which span begins
  line: u64,

  /// Column in line at which span begins
  column: u64,

  /// Length of span in symbols
  length: u64,
}

pub struct ReversableStream<T: Clone + PartialEq> {
  data: Vec<T>,
  stack: Vec<usize>,
  pos: usize,
}

impl<T: Clone + PartialEq> ReversableStream<T> {
  pub fn data(&self) -> &Vec<T> {
    &self.data
  }

  pub fn pos(&self) -> usize {
    self.pos
  }

  pub fn new(data: Vec<T>) -> ReversableStream<T> {
    ReversableStream {
      data,
      stack: vec![],
      pos: 0,
    }
  }
}

impl<T: Clone + PartialEq> ReversableIterator for ReversableStream<T> {
  type Item = T;

  fn next_ext(&mut self, size: usize) -> Vec<Self::Item> {
    let next_tokens = self
      .data
      .iter()
      .skip(self.pos)
      .take(size)
      .cloned()
      .collect();

    self.pos += size;

    next_tokens
  }

  fn peek_ext(&mut self, size: usize) -> Vec<Self::Item> {
    self
      .data
      .iter()
      .skip(self.pos)
      .take(size)
      .cloned()
      .collect()
  }

  fn remember(&mut self) {
    self.stack.push(self.pos);
  }

  fn accept(&mut self) {
    self.stack.pop();
  }

  fn decline(&mut self) {
    self.pos = self
      .stack
      .pop()
      .expect("Declined before remembered, stack was empty");
  }
}

impl ReversableStream<char> {
  pub fn check(&mut self, regex: &str) -> Option<String> {
    let regex = Regex::new(regex).unwrap();
    let string = self.data.iter().skip(self.pos).cloned().collect::<String>();

    if let Some(mat) = regex.find(&string) {
      if mat.start() == 0 && mat.start() != mat.end() {
        self.pos += mat.end();

        Some(mat.as_str().to_string())
      } else {
        None
      }
    } else {
      None
    }
  }
  pub fn check_char(&mut self, c: char) -> bool {
    let c2 = self.data.get(self.pos).cloned();

    if c2 == Some(c) {
      self.pos += 1;

      true
    } else {
      false
    }
  }
  pub fn check_string(&mut self, s: &str) -> bool {
    let string = self
      .data
      .iter()
      .skip(self.pos)
      .take(s.len())
      .cloned()
      .collect::<String>();

    if s == string {
      self.pos += s.len();
      true
    } else {
      false
    }
  }
}

impl From<&str> for ReversableStream<char> {
  fn from(s: &str) -> Self {
    Self::new(s.chars().collect::<Vec<_>>())
  }
}

impl From<String> for ReversableStream<char> {
  fn from(s: String) -> Self {
    Self::from(s.as_str())
  }
}

pub trait Tokenizable
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self>;
}

// impl<T: Tokenizable> TokenStream<T> {
//   fn wrapped(&self, left: T, right: T) -> Option<Self> {
//     if self.data[0] == left {
//       let i = 1;
//       while self.data[i] != None
//     } else { None }
//   }

//   fn split(&self, separator: T) -> Vec<Self> {

//   }
// }

impl<T: Tokenizable> From<ReversableStream<char>> for ReversableStream<T> {
  fn from(mut char_stream: ReversableStream<char>) -> Self {
    let mut tokens = vec![];

    while let Some(token) = T::tokenize(&mut char_stream) {
      tokens.push(token);
      while char_stream.peek() == Some(' ') {
        char_stream.next();
      }
    }

    Self::new(tokens)
  }
}

impl<T: Tokenizable> From<String> for ReversableStream<T> {
  fn from(s: String) -> Self {
    Self::from(ReversableStream::<char>::from(s))
  }
}

impl<T: Tokenizable> From<&str> for ReversableStream<T> {
  fn from(s: &str) -> Self {
    Self::from(ReversableStream::<char>::from(s))
  }
}

// impl<T: Tokenizable, U: Tokenizable> Tokenizable for Either<T, U> {
//   fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
//     if let Some(token) = T::tokenize(stream) {
//       Some(Self::Left(token))
//     } else if let Some(token) = U::tokenize(stream) {
//       Some(Self::Right(token))
//     } else {
//       None
//     }
//   }
// }

pub trait Parseable<T: Tokenizable>
where
  Self: Sized,
{
  type ParsingError;

  fn parse(token_stream: &mut ReversableStream<T>) -> Result<Self, Self::ParsingError>;
}

impl<S, T, U> Parseable<S> for Either<T, U>
where
  S: Tokenizable,
  T: Parseable<S>,
  U: Parseable<S>,
{
  type ParsingError = (T::ParsingError, U::ParsingError);

  fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
    Err((
      match T::parse(token_stream) {
        Ok(res) => {
          return Ok(Self::Left(res));
        }
        Err(err) => err,
      },
      match U::parse(token_stream) {
        Ok(res) => {
          return Ok(Self::Right(res));
        }
        Err(err) => err,
      },
    ))
  }
}

impl<S, T> Parseable<S> for Vec<T>
where
  S: Tokenizable,
  T: Parseable<S>,
{
  type ParsingError = ();
  // type ParsingError = T::ParsingError;

  fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
    let mut vec = vec![];

    while let Ok(parsed) = T::parse(token_stream) {
      vec.push(parsed);
    }
    // loop {
    //   match T::parse(token_stream) {
    //     Ok(parsed) => vec.push(parsed),
    //     Err(err) => return Err(err),
    //   }
    // }

    Ok(vec)
  }
}

impl<S, T> Parseable<S> for Option<T>
where
  S: Tokenizable,
  T: Parseable<S>,
{
  type ParsingError = ();

  fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
    Ok(T::parse(token_stream).ok())
  }
}
