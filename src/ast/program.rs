use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{char_stream::TokenStream, logger::LoggerTrait, value::Value},
  enviroment::Enviroment,
  parse_stmt_vec,
};

use super::{stmt::Statement, Evaluatable, Parseable};


#[derive(Clone, Debug, PartialEq)]
pub struct Program(pub Vec<Statement>);

impl<'a> Parseable<'a> for Program {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let res = parse_stmt_vec!(stream)?;

    Ok(Self(res))
  }
}

impl Evaluatable for Program {
  fn evaluate<L: LoggerTrait>(self, env: &mut Rc<RefCell<Enviroment>>, logger: &mut L) -> Value {
    for stmt in self.0 {
      stmt.evaluate(env, logger);
    }

    Value::None
  }
}
