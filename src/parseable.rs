use either::Either;
use std::{marker::PhantomData, mem::MaybeUninit, ops::Range};

use crate::{enviroment::Enviroment, map, namespace::Namespace};

type Span = Range<usize>;

pub struct ParsingContext {
  pub namespace: Namespace,
}

impl ParsingContext {
  pub fn new() -> ParsingContext {
    ParsingContext {
      namespace: Namespace(map![]),
    }
  }
  pub fn from_env(_env: Enviroment) -> ParsingContext {
    ParsingContext {
      namespace: Namespace(map![]),
    }
  }
}

pub trait Parseable<T>: Sized {
  type I = T;
  type O = Self;

  /// parses Self from Self::I, returning rest of unprocessed input and result
  /// of parsing, if it succeeded
  fn parse(input: Self::I) -> (Self::I, Option<Self::O>);
}

// if parses did not succeed - we must get input stream on track again by
// synchronizing it (going out of invalid input state) as appropriate for given
// item
pub trait Synchronizable<I> {
  fn sync(input: I) -> I;
}

pub trait SyncParse<T>: Parseable<T, I = T> + Synchronizable<T> {
  fn parse_sync(input: Self::I) -> (Self::I, Option<Self::O>) {
    let (i, o) = Self::parse(input);
    match o {
      Some(_) => (i, o),
      None => (Self::sync(i), o),
    }
  }
}

impl<I, T: Parseable<I, I = I> + Synchronizable<I>> SyncParse<I> for T {}

impl<T, I> Parseable<I> for Option<T>
where
  T: Parseable<I, I = I>,
{
  type O = Option<T::O>;

  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    let (i, o) = T::parse(input);
    if let Some(_) = o {
      (i, Some(o))
    } else {
      (i, Some(o))
    }
  }
}

impl<T, I> Synchronizable<I> for Option<T>
where
  T: Parseable<I>,
{
  fn sync(input: I) -> I { input }
}

impl<T, I> Parseable<I> for Vec<T>
where
  T: Parseable<I>,
{
  type I = T::I;
  type O = Vec<T::O>;

  fn parse(mut input: Self::I) -> (Self::I, Option<Self::O>) {
    let mut res = vec![];
    loop {
      match T::parse(input) {
        (i, Some(o)) => {
          res.push(o);
          input = i
        },
        (i, None) => break (i, Some(res)),
      }
    }
  }
}

impl<T, I> Synchronizable<I> for Vec<T>
where
  T: Parseable<I> + Synchronizable<I>,
{
  fn sync(input: I) -> I { T::sync(input) }
}

impl<T, U, I> Parseable<I> for (T, U)
where
  T: Parseable<I, I = I>,
  U: Parseable<I, I = I>, // they must parse same inputs
{
  type O = (T::O, U::O);

  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    let (input, t_parsed) = T::parse(input);
    if let None = t_parsed {
      return (input, None);
    }
    let t_parsed = t_parsed.unwrap();

    let (input, u_parsed) = U::parse(input);
    if let None = u_parsed {
      return (input, None);
    }
    let u_parsed = u_parsed.unwrap();

    (input, Some((t_parsed, u_parsed)))
  }
}

impl<T, U, I> Synchronizable<I> for (T, U)
where
  T: Synchronizable<I>,
  U: SyncParse<I>,
{
  fn sync(input: I) -> I {
    let input = T::sync(input);
    U::parse_sync(input).0
  }
}

impl<I, T, U> Parseable<I> for Either<T, U>
where
  T: Parseable<I, I = I>,
  U: Parseable<I, I = I>,
{
  type O = Either<T::O, U::O>;
  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    let (input, o) = T::parse(input);
    if let Some(o) = o {
      return (input, Some(Either::Left(o)));
    }

    let (input, u_parsed) = U::parse(input);

    (input, u_parsed.map(|u| Either::Right(u)))
  }
}

pub struct Parsed<I, T: Parseable<I>> {
  source:    I,
  item_type: PhantomData<T>,
}

impl<I: Sized, T: Parseable<I, I = I>> Iterator for Parsed<I, T> {
  type Item = T::O;
  fn next(&mut self) -> Option<Self::Item> {
    let input = unsafe { std::mem::replace(&mut self.source, MaybeUninit::zeroed().assume_init()) };
    let (rest, parsed) = T::parse(input);
    self.source = rest;
    parsed
  }
}

pub trait Par<I, T: Parseable<I>> {
  fn parsed(self) -> Parsed<I, T>;
}

impl<I: Iterator, T: Parseable<I>> Par<I, T> for I {
  fn parsed(self) -> Parsed<I, T> {
    Parsed::<I, T> {
      source:    self,
      item_type: PhantomData,
    }
  }
}
