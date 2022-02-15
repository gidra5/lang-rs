#![allow(unused)]
use crate::{common::*, enviroment::*, token::*, *};
use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::HashMap,
  fmt::{Display, Formatter},
  hash::{Hash, Hasher},
  rc::Rc,
};

pub mod expr;
pub use expr::*;

pub mod stmt;
use itertools::Itertools;
pub use stmt::*;

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

pub enum Type {
  String,
  Number,
  Char,
  Boolean,
  Void,
  Tuple(Vec<(String, Type)>),
}

pub enum Declaration {
  Variable(Type),
  Namespace(HashMap<String, Declaration>),
}

pub struct ParsingContext {
  declarations: HashMap<String, Declaration>,
}

impl ParsingContext {
  pub fn new() -> ParsingContext {
    ParsingContext {
      declarations: map![],
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
