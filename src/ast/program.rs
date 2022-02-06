use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{LoggerTrait, Value},
  enviroment::Enviroment,
  parse_stmt_vec,
  token::TokenStream,
};

use super::{stmt::Statement, Evaluatable, Parseable};


#[derive(Clone, Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

impl Parseable for Program {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> {
    let res = parse_stmt_vec!(stream)?;

    Ok(Self(res))
  }
}

impl Evaluatable for Program {
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    for stmt in self.0.iter() {
      stmt.evaluate(env, logger);
    }

    Value::None
  }
}
