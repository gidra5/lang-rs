use itertools::Itertools;

use crate::{enviroment::Enviroment, errors::RuntimeError, types::Type};
use std::{collections::HashMap, fmt::Display};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  // primitives
  String(String),

  // 3 levels of composition (the most useful)
  Tuple(Vec<Value>),
  Record(HashMap<String, Value>),
  Map(HashMap<Value, Value>),

  // first class entities
  Type(Box<Type>),
}

impl std::hash::Hash for Value {
  // TODO: probably not correct
  // sit it will treat all instances of variant as equal
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
}

impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use self::Value::*;
    match self {
      String(s) => write!(f, "\"{}\"", s),
      Record(record_items) => {
        let x = record_items
          .iter()
          .map(|(key, value)| format!("{}: {}", key, value))
          .join(", ");
        if x.len() == 1 {
          write!(f, "{x}")
        } else {
          write!(f, "({x})")
        }
      },
      Tuple(tuple_items) => {
        let x = tuple_items.iter().join(", ");
        if x.len() == 1 {
          write!(f, "{x}")
        } else {
          write!(f, "({x})")
        }
      },
      Map(map_items) => {
        let x = map_items
          .iter()
          .map(|(key, value)| format!("[{}]: {}", key, value))
          .join(", ");
        if x.len() == 1 {
          write!(f, "{x}")
        } else {
          write!(f, "({x})")
        }
      },
      Type(t) => write!(f, "type {:?}", t),
    }
  }
}

impl Eq for Value {}

pub trait Evaluatable {
  type E = RuntimeError;
  fn evaluate(&self, env: Option<Enviroment>) -> (Result<Value, Self::E>, Option<Enviroment>);
}
