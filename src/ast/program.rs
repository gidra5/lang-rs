use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{reversable_iterator::ReversableIterator, LoggerTrait, Value},
  enviroment::Enviroment,
  token::TokenStream,
};

use super::{stmt::Statement, Evaluatable, Expression, Parseable, ParsingContext, ParsingError};


#[derive(Clone, Debug, PartialEq)]
pub struct Script(pub Vec<Statement>);

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

      match Statement::parse(stream, context) {
        Ok(Statement::Expression(expr)) if expr == Expression::default() => (),
        Ok(expr) => exprs.push(expr),
        Err(err) => errors.push(err),
      };
    }
  }
}

impl Evaluatable for Script {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    for stmt in self.0.iter() {
      stmt.evaluate(env, logger);
    }

    Value::None
  }
}
