#![allow(unused)]
use crate::{common::*, enviroment::*, token::*, types::Namespace, *};
use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::HashMap,
  fmt::{Display, Formatter},
  hash::{Hash, Hasher},
  rc::Rc,
};

use itertools::Itertools;

pub mod expr;
pub use expr::*;

mod pattern;
pub use expr::*;

pub mod program;
pub use program::*;

pub mod inline;
pub use inline::*;

#[derive(Clone, Debug)]
pub struct ASTNodeExt<T> {
  pub node: T,
  pub span: Span<TokenStream>,
}

/// matches error productions
#[derive(Debug)]
pub enum ParsingError {
  Generic(String),

  Aggregate(Vec<ParsingError>),
}

#[macro_export]
macro_rules! parse_error {
  ($($rest: expr),+) => {
    ParsingError::Generic(
      format!($($rest),+)
    )
  };
}

pub struct ParsingContext {
  pub namespace: Namespace,
}

impl ParsingContext {
  pub fn new() -> ParsingContext {
    ParsingContext {
      namespace: Namespace(map![]),
    }
  }
  pub fn from_env(env: &Enviroment) -> ParsingContext {
    ParsingContext {
      namespace: Namespace(map![]),
    }
  }
}

pub trait Parseable
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError>;
  fn parse_ext(
    stream: &mut TokenStream,
    context: &mut ParsingContext,
  ) -> Result<ASTNodeExt<Self>, (ParsingError, Span<TokenStream>)> {
    let mut span = Span {
      stream: stream.clone(),
      length: 1,
    };
    let res = Self::parse(stream, context);

    span.length = stream.pos() - span.pos();

    match res {
      Ok(node) => Ok(ASTNodeExt { node, span }),
      Err(err) => Err((err, span)),
    }
  }
}

pub trait Synchronizable {
  fn synchronize(stream: &mut TokenStream) {
    stream.next();
    while !Self::sync_point(stream) && stream.peek().is_some() {
      stream.next();
    }
  }

  fn sync_point(stream: &mut TokenStream) -> bool { true }
}

pub trait Evaluatable {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value;
}

impl Display for ParsingError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Generic(msg) => write!(f, "{}", msg),
      Self::Aggregate(errs) => write!(f, "{}", errs.into_iter().join("\n")),
    }
  }
}


mod new {
  use either::Either;
  use std::ops::Range;

  type Span = Range<usize>;

  trait Parseable: Sized {
    type I;
    type E = Vec<(String, Span)>;
    fn parse(input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)>;
  }

  trait Synchronizable<I> {
    fn sync(input: I) -> I;
  }

  trait SyncParse: Parseable + Synchronizable<Self::I> {
    fn parse_sync(input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)> {
      Ok(match Self::parse(input) {
        Ok((i, o)) => (i, o),
        Err((i, e)) => return Err((Self::sync(i), e)),
      })
    }
  }

  impl<T: Parseable + Synchronizable<T::I>> SyncParse for T {}

  impl<T> Parseable for Option<T>
  where
    T: Parseable,
  {
    type I = T::I;
    type E = T::E;
    fn parse(input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)> {
      Ok(match T::parse(input) {
        Ok((i, o)) => (i, Some(o)),
        Err((i, e)) => (i, None),
      })
    }
  }

  impl<T> Synchronizable<T::I> for Option<T>
  where
    T: Parseable,
  {
    fn sync(input: T::I) -> T::I {
      match T::parse(input) {
        Ok((i, o)) => i,
        Err((i, e)) => i,
      }
    }
  }

  impl<T> Parseable for Vec<T>
  where
    T: Parseable,
  {
    type I = T::I;
    type E = T::E;
    fn parse(mut input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)> {
      let mut res = vec![];
      loop {
        match T::parse(input) {
          Ok((i, o)) => {
            res.push(o);
            input = i
          },
          Err((i, _)) => break Ok((i, res)),
        }
      }
    }
  }

  impl<T> Synchronizable<T::I> for Vec<T>
  where
    T: Parseable,
  {
    fn sync(mut input: T::I) -> T::I {
      loop {
        match T::parse(input) {
          Ok((i, _)) => input = i,
          Err((i, _)) => break i,
        }
      }
    }
  }

  impl<T, U> Parseable for (T, U)
  where
    T: Parseable,
    U: Parseable<I = T::I>,
  {
    type I = T::I;
    type E = Either<T::E, U::E>;
    fn parse(input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)> {
      match T::parse(input) {
        Err((i, e)) => Err((i, Either::Left(e))),
        Ok((i, o1)) => {
          match U::parse(i) {
            Err((i, e)) => Err((i, Either::Right(e))),
            Ok((i, o2)) => Ok((i, (o1, o2))),
          }
        },
      }
    }
  }

  impl<T, U, I> Synchronizable<I> for (T, U)
  where
    T: Synchronizable<I>,
    U: Synchronizable<I>,
  {
    fn sync(input: I) -> I {
      let input = T::sync(input);
      U::sync(input)
    }
  }

  impl<T, U> Parseable for Either<T, U>
  where
    T: Parseable,
    U: Parseable<I = T::I>,
  {
    type I = T::I;
    type E = (T::E, U::E);
    fn parse(input: Self::I) -> Result<(Self::I, Self), (Self::I, Self::E)> {
      match T::parse(input) {
        Ok((i, o)) => Ok((i, Self::Left(o))),
        Err((i, e1)) => {
          match U::parse(i) {
            Ok((i, o)) => Ok((i, Self::Right(o))),
            Err((i, e2)) => Err((i, (e1, e2))),
          }
        },
      }
    }
  }
}
