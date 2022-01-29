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
pub struct ParsingError<'a> {
  pub error: ErrorType,
  pub span:  Span<TokenStream<'a>>,
}

pub trait Parseable<'a>
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String>;
  fn parse_ext(stream: &mut TokenStream<'a>) -> Result<ASTNodeExt<'a, Self>, ParsingError<'a>> {
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

pub trait Synchronizable<'a> {
  fn synchronize(stream: &mut TokenStream<'a>) {
    stream.next();
    while !Self::sync_point(stream) && stream.peek().is_some() {
      stream.next();
    }
  }

  fn sync_point(stream: &mut TokenStream<'a>) -> bool { true }
}

pub trait Evaluatable {
  fn evaluate<L: LoggerTrait>(self, env: &mut Enviroment, logger: &mut L) -> Value;
}

#[derive(Clone, Debug)]
pub struct ASTNodeExt<'a, T> {
  pub node: T,
  pub span: Span<TokenStream<'a>>,
}
