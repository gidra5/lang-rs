use std::{
  cell::RefCell,
  fmt::{Display, Formatter},
  rc::Rc,
};

use crate::{
  check_token,
  check_token_end,
  common::{reversable_iterator::ReversableIterator, value::RecordItem, LoggerTrait, Value},
  enviroment::Enviroment,
  punct_or_newline,
  scoped,
  skip,
  token::{Token, TokenExt, TokenStream},
};
use itertools::Itertools;

#[path = "../tests/stmt.rs"]
mod tests;

use super::{
  expr::{match_value, Expression, Op},
  Evaluatable,
  Parseable,
  ParsingContext,
  ParsingError,
};

/// Statement
///
/// Every statement has either a semicolon or newline at the end
///
/// Possible variants are:
///
/// Print: Prints value of evaluated expression, probably will be removed
///
/// Syntax:
/// "print" expr
///
/// Expression: evaluates expression, probably everything can be treated as this
/// statement
///
/// Syntax:
/// expr
///
/// Let: Creates variable in current scope
///
/// Syntax:
/// "let" ident ("=" expr)?
///
/// Block: Groups staements into one
///
/// Syntax:
/// "{" (stmt)* "}"
///
/// If: Conditionally executes statements
///
/// Syntax:
/// "if" expr(: | "\n") stmt "else" stmt
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
  Expression(Expression),
  Let(String, Option<Expression>),
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Expression(expr) => write!(f, "{}", expr),
      Self::Let(x, Some(expr)) => write!(f, "let {} = {}", x, expr),
      Self::Let(x, None) => write!(f, "let {}", x),
    }
  }
}

impl Parseable for Statement {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let res = {
      let TokenExt { token, src, span } = stream.peek().ok_or(ParsingError::Generic(
        "Unexpected end of statement.".to_string(),
      ))?;

      match token {
        Token::Identifier if src == "let" => {
          stream.next();

          let id = stream
            .next()
            .map(|token| {
              match token.token {
                Token::Identifier => Some(token.src),
                _ => None,
              }
            })
            .flatten()
            .ok_or(ParsingError::Generic(
              "Expected identifier at let statement.".to_string(),
            ))?;

          if check_token!(stream.peek(), Equal) {
            stream.next();
            while check_token!(stream.peek(), NewLine) {
              stream.next();
            }
            Self::Let(
              id,
              Some(match Expression::parse(stream, context) {
                Ok(expr) if expr == Expression::default() => {
                  return Err(ParsingError::Generic(
                    "Error at expression after '=' in let statement: No expression".to_string(),
                  ))
                },
                Ok(expr) => expr,
                Err(msg) => {
                  return Err(ParsingError::Generic(format!(
                    "Error at expression after '=' in let statement: {}",
                    msg
                  )))
                },
              }),
            )
          } else {
            Self::Let(id, None)
          }
        },
        _ => Self::Expression(Expression::parse(stream, context)?),
      }
    };

    skip!(stream, Semicolon | NewLine);

    Ok(res)
  }
}

impl Evaluatable for Statement {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    match self {
      Self::Expression(expr) => {
        return expr.evaluate(env, logger);
      },
      Self::Let(id, expr) => {
        let val = expr
          .clone()
          .map(|expr| expr.evaluate(env, logger))
          .unwrap_or_default();
        env.define(id.clone(), val)
      },
    };

    Value::None
  }
}
