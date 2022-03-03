use itertools::Itertools;

use crate::{ast::Expression, enviroment::Enviroment, types::Type};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};


#[derive(Clone, Debug, PartialEq)]
pub struct RecordItem {
  pub key:   Option<Value>,
  pub value: Value,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  // Record(Vec<RecordItem>),
  Tuple(Vec<Value>),
  Record(HashMap<String, Value>),
  Map(HashMap<Value, Value>),
  EnumValue(String, Box<Value>),
  Function(Expression, Box<Enviroment>, Expression),
  Type(Box<Type>),
  None,
}

impl std::hash::Hash for Value {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
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
          .map(|(key, value)| format!("{}: {}", key, value))
          .join(", ");
        if x.len() == 1 {
          write!(f, "{}", x)
        } else {
          write!(f, "({})", x)
        }
      },
      Tuple(tuple_items) => {
        let mut x = tuple_items.iter().join(", ");
        if x.len() == 1 {
          write!(f, "{}", x)
        } else {
          write!(f, "({})", x)
        }
      },
      Map(map_items) => {
        let mut x = map_items
          .iter()
          .map(|(key, value)| format!("[{}]: {}", key, value))
          .join(", ");
        if x.len() == 1 {
          write!(f, "{}", x)
        } else {
          write!(f, "({})", x)
        }
      },
      EnumValue(variant_name, value) => write!(f, "{}({})", variant_name, value),
      Type(t) => write!(f, "type {:?}", t),
      Function(pat, _, expr) => write!(f, "({} => {})", pat, expr),
      None => write!(f, "None"),
    }
  }
}

impl Eq for Value {}
