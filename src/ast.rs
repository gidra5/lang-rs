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
