#![allow(unused)]
use crate::regex::Regex;
use crate::either::Either;

pub trait ReversableIterator {
  type Item: PartialEq;

  /// Returns next `size` items in iterator
  fn next_ext(&mut self, size: usize) -> Vec<Self::Item>;

  /// Returns next item in iterator
  fn next(&mut self) -> Option<Self::Item> { self.next_ext(1).pop() }

  /// Returns next `size` items without consuming iterator
  fn peek_ext(&mut self, size: usize) -> Vec<Self::Item>;

  /// Returns next item without consuming iterator
  fn peek(&mut self) -> Option<Self::Item> { self.peek_ext(1).pop() }

  fn expect_next(&mut self, item: Self::Item, err_msg: String) {
    if self.next() != Some(item) {
      panic!(err_msg)
    }
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

trait Helper: ReversableIterator {
  fn check<T, U>(&mut self, f: fn(&mut Self) -> Result<T, U>) -> Result<T, U> {
    self.remember();

    let res = f(self);

    match res {
      Ok(_) => self.accept(),
      Err(_) => self.decline(),
    };

    res
  }
}

impl<T: ReversableIterator> Helper for T {}

#[derive(Clone)]
pub struct Span {
  /// File which is spanned
  file:   Box<std::path::Path>,

  /// Line in file at which span begins
  line:   u64,

  /// Column in line at which span begins
  column: u64,

  /// Length of span in symbols
  length: u64,
}

pub struct ReversableStream<T: Clone> {
  data:   Vec<T>,
  stack:  Vec<usize>,
  pos:    usize,
}

impl<T: Clone> ReversableIterator for ReversableStream<T> {
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

  fn remember(&mut self) { self.stack.push(self.pos); }

  fn accept(&mut self) { self.stack.pop(); }

  fn decline(&mut self) {
    self.pos = self
      .stack
      .pop()
      .expect("Declined before remembered, stack was empty");
  }
}

impl ReversableStream<char> {
  pub fn check(&mut self, regex: Regex) -> Option<String> {
    let string =  self
      .data
      .iter()
      .skip(self.pos)
      .cloned()
      .collect::<String>();

    match regex.find(&string) {
      Some(mat) => {
        if mat.start() != 0 {
          return None;
        }

        self.pos += mat.end();

        return Some(mat.as_str().to_string());
      },
      None => return None,
    }
  }
  pub fn check_char(&mut self, c: char) -> bool {
    let c2 = self.data.get(self.pos).cloned();

    if c2 == Some(c) {
      self.pos += 1;

      true
    } else { false }
  }
}

pub trait Tokenizable
where
  Self: Sized + Clone,
{
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self>;
}

type TokenStream<T: Tokenizable> = ReversableStream<T>;

impl<T: Tokenizable> TokenStream<T> {
  fn new(
    mut char_stream: ReversableStream<char>,
  ) -> Result<Self, T::TokenizationError> {
    let mut tokens = vec![];

    while char_stream.peek() != None {
      tokens.push(T::tokenize(&mut char_stream)?);
    }

    Ok(Self {
      data: tokens,
      stack: vec![],
      pos: 0,
    })
  }
}

impl<T: Tokenizable> Iterator for TokenStream<T> {
  type Item = T;

  fn next(&mut self) -> Option<Self::Item> { <TokenStream<T> as ReversableIterator>::next(self) }
}

impl<T: Tokenizable, U: Tokenizable> Tokenizable for Either<T, U> {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(token) = T::tokenize(stream) {
      Some(Self::Left(token))
    } else if let Some(token) = U::tokenize(stream) {
      Some(Self::Right(token))
    } else { None }
  }
}

pub trait Parseable<T: Tokenizable>
where Self: Sized
{
  type ParsingError;

  fn parse(token_stream: &mut TokenStream<T>) -> Result<Self, Self::ParsingError>;
}

impl<S, T, U> Parseable<S> for Either<T, U>
where
  S: Tokenizable,
  T: Parseable<S>,
  U: Parseable<S>
{
  type ParsingError = (T::ParsingError, U::ParsingError);

  fn parse(
    token_stream: &mut TokenStream<S>,
  ) -> Result<Self, Self::ParsingError> {
    Err((
      match T::parse(token_stream) {
        Ok(res) => { return Ok(Self::Left(res)); },
        Err(err) => err,
      },
      match U::parse(token_stream) {
        Ok(res) => { return Ok(Self::Right(res)); },
        Err(err) => err,
      },
    ))
  }
}

pub trait Transformator<S, T, U>
where
  S: Tokenizable,
  T: Parseable<S>,
  U: Parseable<S>
{
  fn transform(src: T) -> U;
}