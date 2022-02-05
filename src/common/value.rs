use itertools::Itertools;

use crate::{ast::Expression, enviroment::Enviroment};
use std::{cell::RefCell, fmt::Display, rc::Rc};


#[derive(Clone, Debug, PartialEq)]
pub struct RecordItem {
  pub key:   Option<Value>,
  pub value: Value,
}

// #[derive(Clone, Debug, PartialEq)]
// pub enum Record {
//   Tuple(Vec<Value>),
//   Record(Vec<RecordItem>),
//   /* Record(HashMap<String, Value>),
//    * Map(HashMap<Value, Value>), */
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  Record(Vec<RecordItem>),
  Function(Box<Expression>, Box<Enviroment>, Box<Expression>),
  Unit,
  None,
}

impl Default for Value {
  fn default() -> Self { Value::None }
}

impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use crate::Value::*;
    match self {
      String(s) => write!(f, "\"{}\"", s),
      Number(num) => write!(f, "{}", num),
      Boolean(b) => write!(f, "{}", b),
      Char(c) => write!(f, "'{}'", c),
      Record(record_items) => {
        let mut x = record_items
          .iter()
          .map(|RecordItem { key, value }| {
            (
              key.as_ref().map(|value| format!("[{}]", value)),
              format!("{}", value),
            )
          })
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].0 != Option::None) {
          write!(
            f,
            "({})",
            x.iter()
              .map(|(name, expr)| {
                match name {
                  Some(x) => format!("{}: {}", x, expr),
                  Option::None => format!("{}", expr),
                }
              })
              .join(", ")
          )
        } else if x.len() == 1 {
          write!(f, "{}", x.pop().unwrap().1)
        } else {
          write!(f, "()")
        }
      },
      Function(pat, _, expr) => write!(f, "({} => {})", pat, expr),
      Unit => write!(f, "()"),
      None => write!(f, "None"),
    }
  }
}
