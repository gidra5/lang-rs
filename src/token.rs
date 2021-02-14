#![allow(unused)]
use crate::itertools::{ multipeek, Either };

pub struct Span {
  file: Box<std::path::Path>,
  line: u64,
  column: u64,
  length: u64,
}

enum Status {
  Success,
  Fail,
  Computing
}
trait Try<I: Iterator>
  where Self: Sized
{
  //iterate computing function on each element until it fails or succseeds
  //if fails keep iterator as it was before call, else consume it
  fn try_foreach(&self, f: fn(&I::Item) -> Status) -> Self {


    todo!()
  }
}

pub trait TokenTrait<U: Iterator<Item = char>>
  where Self: Sized
{
  fn to_token(stream: &mut U) -> Option<(Self, Span)>;
}

pub struct TokenStream<U: Iterator<Item = char>, T: TokenTrait<U>> {
  char_stream: U,
  _marker: std::marker::PhantomData<T>,
}

impl<U: Iterator<Item = char>, T: TokenTrait<U>> TokenStream<U, T> {
  fn new(char_stream: U) -> Self {
    Self {
      char_stream,
      _marker: std::marker::PhantomData::default(),
    }
  }
}

impl<U: Iterator<Item = char>, T: TokenTrait<U>> Iterator for TokenStream<U, T> {
  type Item = (T, Span);

  fn next(&mut self) -> Option<Self::Item> {
    T::to_token(&mut self.char_stream)
  }
}

impl<V: Iterator<Item = char>, T: TokenTrait<V>, U: TokenTrait<V>> TokenTrait<V> for Either<T, U> {
  fn to_token(stream: &mut V) -> Option<(Self, Span)>
    {
    if let Some((res, span)) = T::to_token(stream) {
      return Some((Self::Left(res), span));
    }
    if let Some((res, span)) = U::to_token(stream) {
      return Some((Self::Right(res), span));
    }

    None
  }
}

enum Keyword {
  Let,
  Placeholder
}
struct Identifier(String);
struct Wrapped<U: Iterator<Item = char>, T: TokenTrait<U>> {
  template: String,
  inner: TokenStream<U, T>,
}

enum Token<U: Iterator<Item = char>> {
  Keyword(Keyword),
  Identifier(Identifier),
  Wrapped(Wrapped<U, Self>),
  Separator(String)
}

impl<U: Iterator<Item = char>> TokenTrait<U> for Token<U> {
  fn to_token(stream: &mut U) -> Option<(Self, Span)> {

    todo!()
  }
}