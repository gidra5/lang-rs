use std::{default, fmt::Display};

use itertools::Itertools;

use crate::token::Token;

use super::Expression;

#[derive(Clone, PartialEq, Debug)]
pub enum RecordPatternKey {
  None,
  Rest,
  Identifier(String),
  Value(PatternBinder),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordPatternItem {
  pub key:   RecordPatternKey,
  pub value: PatternBinder,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
  Bind { name: String },
  Value(Value),
  Record(Vec<RecordPatternItem>),
}

impl Parseable for Pattern {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    Ok(match stream.peek() {
      match_token!(Identifier) => {
        let name = if let match_token!({ src }, Identifier) = stream.peek() {
          stream.next();
          src
        } else {
          return parse_error!("");
        };

        Pattern::Bind { name }
      },
      _ => Self::from_expr(&Expression::parse(stream, context)?)?,
    })
  }
}

impl Pattern {
  pub fn from_expr(expr: &Expression) -> Result<Self, ParsingError> { todo!() }
  pub fn _match(&self, val: Value, env: &mut Enviroment) -> bool { todo!() }
  pub fn bind(&self, val: Value, env: &mut Enviroment) { todo!() }
  pub fn context_bind(&self, context: &mut ParsingContext, bound_expr: &Expression) { todo!() }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PatternBinder {
  pub pattern: Pattern,
  pub default: Option<Expression>,
  pub alias:   Option<String>,
}

impl Parseable for PatternBinder {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let pattern = Pattern::parse(stream, context)?;

    let alias = if let [Some(Token::At), Some(Token::Identifier(src))] = stream.peek_ext(2)[..] {
      stream.next_ext(2);
      Some(src)
    } else {
      None
    };
    let default = if let Some(Equal) = stream.peek() {
      stream.next();
      Some(Expression::parse(stream, context)?)
    } else {
      None
    };

    Ok(PatternBinder {
      pattern,
      default,
      alias,
    })
  }
}
