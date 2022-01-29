#![allow(unused)]
use crate::{common::*, enviroment::Enviroment, *};
use std::{
  cell::RefCell,
  cmp::Ordering,
  fmt::{Display, Formatter},
  hash::{Hash, Hasher},
  rc::Rc,
};

pub mod expr;
pub use expr::*;

pub mod stmt;
pub use stmt::*;

pub mod program;
pub use program::*;

#[path = "tests/ast.rs"]
mod tests;

/// matches error productions
#[derive(Debug)]
pub enum ErrorType {
  Generic(String),
}

#[derive(Debug)]
pub struct ParsingError {
  pub error: ErrorType,
  pub span:  Span<TokenStream>,
}

pub trait Parseable
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream) -> Result<Self, String>;
  fn parse_ext(stream: &mut TokenStream) -> Result<ASTNodeExt<Self>, ParsingError> {
    let mut span = Span {
      stream: stream.clone(),
      length: 1,
    };
    let res = Self::parse(stream);

    span.length = stream.pos() - span.pos();

    match res {
      Ok(node) => Ok(ASTNodeExt { node, span }),
      Err(msg) => {
        Err(ParsingError {
          error: ErrorType::Generic(msg),
          span,
        })
      },
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
  fn evaluate<L: LoggerTrait>(self, env: &mut Enviroment, logger: &mut L) -> Value;
}

#[derive(Clone, Debug)]
pub struct ASTNodeExt<T> {
  pub node: T,
  pub span: Span<TokenStream>,
}
