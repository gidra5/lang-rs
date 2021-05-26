#![allow(unused)]
pub use crate::either::Either;
pub use crate::token::*;
use fancy_regex::Regex;

#[path = "tests/common.rs"]
mod tests;

pub struct Logger;
impl Logger {
  pub fn log(msg: &str) {
    println!("Log: {}", msg);
  }

  pub fn warning(msg: &str) {
    println!("Warning: {}", msg);
  }

  pub fn error(msg: &str) {
    println!("Error: {}", msg);
  }

  pub fn error_token(err: TokenizationError<'_>) {
    println!("Error: {}\n{}", err.span, err.msg);
  }

  // pub fn error_parse(err: ParsingError) {
  //   println!("Error: {}\n{}", err.span, err.msg);
  // }
}

#[derive(Clone, Debug)]
pub struct CompilationError<T: Clone + std::fmt::Debug + ReversableIterator> {
  pub span: Span<T>,
  pub msg: String,
}

#[derive(Clone, Debug)]
pub struct Span<T: Clone + std::fmt::Debug + ReversableIterator> {
  /// Stream snapshot where occured error
  pub stream: T,

  /// Length of span in symbols
  pub length: usize,
}

impl<T: Clone + std::fmt::Debug + ReversableIterator> Span<T> {
  pub fn pos(&self) -> usize {
    self.stream.pos()
  }
}

impl<'a> std::fmt::Display for Span<CharStream<'a>> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let stream = &self.stream;
    let src = stream.to_string();
    let line = stream.line();
    let column = stream.column();
    let line_src = match src.lines().nth(line) {
      Some(src) => src,
      None => {
        Logger::error("Failed to read line");
        return Err(std::fmt::Error);
      }
    };
    let mut underscore = format!(
      "\u{001b}[{}C~",
      column + 4 + (line as f64).log10().ceil() as usize
    );
    for i in (1..self.length) {
      underscore += "\u{001b}[0.5C~";
    }

    match self.stream.file {
      "." => write!(
        f,
        "line {}, column {}: \n {}| {}\n{}",
        line + 1,
        column,
        line + 1,
        line_src,
        underscore
      ),
      file => write!(
        f,
        "line {}, column {} in file {}: \n {}| {}\n{}",
        line + 1,
        column + 1,
        file,
        line + 1,
        line_src,
        underscore
      ),
    }
  }
}

pub trait ReversableIterator {
  type Item: PartialEq + Clone;

  /// Returns previous `size` items in iterator
  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>>;

  /// Returns next `size` items in iterator
  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>>;

  /// Returns next `size` items without consuming iterator
  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>>;
  fn pos(&self) -> usize;

  /// Returns previous item
  fn prev(&self) -> Option<Self::Item> {
    self.prev_ext(1).pop().flatten()
  }

  /// Returns next item
  fn next(&mut self) -> Option<Self::Item> {
    self.next_ext(1).pop().flatten()
  }

  /// Returns next item without consuming iterator (aka peeks next item)
  fn peek(&self) -> Option<Self::Item> {
    self.peek_ext(1).pop().flatten()
  }

  fn check(&self, next: Self::Item) -> bool {
    self.peek() == Some(next)
  }

  fn is_next(&mut self, next: Self::Item) -> bool {
    if self.check(next) {
      self.next() != None
    } else {
      false
    }
  }

  fn is_not_next(&mut self, next: Self::Item) -> bool {
    if !self.check(next) {
      self.next() != None
    } else {
      false
    }
  }
}

macro_rules! check_any {
  ($self: expr, $next: expr, $($rest: expr),*) => {
    $self.check($next) || check_next_all!($self, $($rest),*)
  }
}

#[derive(Debug, Clone)]
pub struct ReversableStream<T: Clone + PartialEq> {
  data: Vec<T>,
  pos: usize,
}

impl<T: Clone + PartialEq> ReversableStream<T> {
  pub fn data(&self) -> &Vec<T> {
    &self.data
  }

  pub fn new(data: Vec<T>) -> ReversableStream<T> {
    ReversableStream { data, pos: 0 }
  }
}

impl<T: Clone + PartialEq> ReversableIterator for ReversableStream<T> {
  type Item = T;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> {
    let mut next_tokens_iter = self.data.iter().skip(self.pos).cloned();
    let next_tokens = (0..size).map(|_| next_tokens_iter.next()).collect();

    self.pos += size;

    next_tokens
  }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    let mut next_tokens_iter = self.data.iter().skip(self.pos).cloned();
    (0..size).map(|_| next_tokens_iter.next()).collect()
  }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    let mut prev_tokens_iter = self.data.iter().skip(self.pos - size).cloned();
    (0..size).map(|_| prev_tokens_iter.next()).collect()
  }

  fn pos(&self) -> usize {
    self.pos
  }
}

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

#[derive(Debug, Clone)]
pub struct CharStream<'a> {
  stream: ReversableStream<char>,
  pub file: &'a str,
}

impl CharStream<'_> {
  pub fn new(filename: &str) -> Result<CharStream, &str> {
    std::fs::read(filename)
      .map(|file| {
        let mut stream =
          CharStream::from_string(String::from_utf8(file).expect("Failed to parse as utf8 text"));
        stream.file = filename;
        stream
      })
      .map_err(|err| {
        use std::io::ErrorKind::*;
        match err.kind() {
          NotFound => "No such file",
          PermissionDenied => {
            "Permission denied, maybe try running as administrator/sudo or add 'executable' flag"
          }
          _ => "Unexpected IO error",
        }
      })
  }

  pub fn from_string<'a>(s: String) -> CharStream<'a> {
    CharStream {
      stream: ReversableStream::<char>::from(s),
      file: ".",
    }
  }

  pub fn from_str<'a>(s: &str) -> CharStream<'a> {
    CharStream {
      stream: ReversableStream::<char>::from(s),
      file: ".",
    }
  }

  pub fn stream(&self) -> &ReversableStream<char> {
    &self.stream
  }

  pub fn line(&self) -> usize {
    self
      .stream
      .data()
      .iter()
      .take(self.pos())
      .filter(|&&c| c == '\n')
      .count()
  }

  pub fn column(&self) -> usize {
    let mut col = 0;
    self
      .stream
      .data()
      .iter()
      .take(self.pos())
      .for_each(|&c| if c == '\n' { col = 0 } else { col += 1 });
    col
  }

  pub fn substring(&self, left: usize, right: usize) -> String {
    self
      .stream
      .data()
      .iter()
      .skip(left)
      .take(right - left)
      .collect::<std::string::String>()
  }

  pub fn check<F: FnOnce(char) -> bool>(&self, f: F) -> bool {
    self.stream.peek().map(f) == Some(true)
  }
  pub fn check_next<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
    if self.check(f) {
      self.next()
    } else {
      None
    }
  }
}

impl std::fmt::Display for CharStream<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.stream.data().iter().collect::<std::string::String>()
    )
  }
}

impl ReversableIterator for CharStream<'_> {
  type Item = char;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> {
    self.stream.next_ext(size)
  }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    self.stream.peek_ext(size)
  }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> {
    self.stream.prev_ext(size)
  }

  fn pos(&self) -> usize {
    self.stream.pos()
  }
}

pub trait Parseable<'a, T: Tokenizable<'a>>
where
  Self: Sized,
{
  type ParsingError;

  fn parse(token_stream: &mut ReversableStream<T>) -> Result<Self, Self::ParsingError>;
}

// impl<'a, S, T, U> Parseable<'a, S> for Either<T, U>
// where
//   S: Tokenizable<'a>,
//   T: Parseable<'a, S>,
//   U: Parseable<'a, S>,
// {
//   type ParsingError = (T::ParsingError, U::ParsingError);

//   fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
//     Err((
//       match T::parse(token_stream) {
//         Ok(res) => {
//           return Ok(Self::Left(res));
//         }
//         Err(err) => err,
//       },
//       match U::parse(token_stream) {
//         Ok(res) => {
//           return Ok(Self::Right(res));
//         }
//         Err(err) => err,
//       },
//     ))
//   }
// }

// impl<'a, S, T> Parseable<'a, S> for Vec<T>
// where
//   S: Tokenizable<'a>,
//   T: Parseable<'a, S>,
// {
//   type ParsingError = ();
//   // type ParsingError = T::ParsingError;

//   fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
//     let mut vec = vec![];

//     while let Ok(parsed) = T::parse(token_stream) {
//       vec.push(parsed);
//     }
//     // loop {
//     //   match T::parse(token_stream) {
//     //     Ok(parsed) => vec.push(parsed),
//     //     Err(err) => return Err(err),
//     //   }
//     // }

//     Ok(vec)
//   }
// }

// impl<'a, S, T> Parseable<'a, S> for Option<T>
// where
//   S: Tokenizable<'a>,
//   T: Parseable<'a, S>,
// {
//   type ParsingError = ();

//   fn parse(token_stream: &mut ReversableStream<S>) -> Result<Self, Self::ParsingError> {
//     Ok(T::parse(token_stream).ok())
//   }
// }

// macro_rules! parse_sequence {
//   ($stream:expr, $($list: expr)+) => {
//     $(
//       $list::parse($stream);
//     )+
//   }
// }
