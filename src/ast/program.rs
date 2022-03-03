use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{reversable_iterator::ReversableIterator, LoggerTrait, Value},
  enviroment::Enviroment,
  skip,
  token::{TokenExt, TokenStream},
};

use super::{Evaluatable, Expression, Parseable, ParsingContext, ParsingError};


#[derive(Clone, Debug, PartialEq)]
pub struct Script(pub Vec<Expression>);

impl Parseable for Script {
  fn parse(stream: &mut TokenStream, context: &mut ParsingContext) -> Result<Self, ParsingError> {
    let mut exprs = vec![];
    let mut errors = vec![];

    loop {
      if check_token_end!(stream) {
        if errors.len() == 0 {
          break Ok(Self(exprs));
        } else {
          break Err(ParsingError::Aggregate(errors));
        }
      }

      match Expression::parse(stream, context) {
        Ok(expr) if expr == Expression::default() => (),
        Ok(expr) => exprs.push(expr),
        Err(err) => errors.push(err),
      };

      skip!(stream, Semicolon | NewLine);
    }
  }
}

impl Evaluatable for Script {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    for expr in self.0.iter() {
      expr.evaluate(env, logger);
    }

    Value::None
  }
}
