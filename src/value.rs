use itertools::Itertools;

use crate::{enviroment::Enviroment, errors::RuntimeError};
use std::{collections::HashMap, fmt::Display};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  // primitives
  String(String),
  // an unique number identifying that symbol
  // used to non intrusively define interfaces
  // usize so that it can be used as pointer
  Symbol(usize),

  // 3 levels of composition (the most useful)
  Tuple(Vec<Value>),
  Record(HashMap<String, Value>),
  Map(HashMap<Value, Value>),
  // referenced values
  // should somehow simplify type probably to accomodate non mutability
  // Ref(Rc<RefCell<Value>>),
  // RefMut(Rc<RefCell<Value>>),

  // first class entities
  // Type(Box<Type>),
  // functions are first class
  // Function(Expression, Box<Enviroment>, Expression),
}

impl std::hash::Hash for Value {
  // TODO: probably bot correct
  // sit it will treat all instances of variant as equal
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
}

// impl Default for Value {
//   fn default() -> Self { Value::None }
// }

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
      // Type(t) => write!(f, "type {:?}", t),
      Symbol(t) => write!(f, "symbol {:?}", t),
      // Function(pat, _, expr) => write!(f, "({} => {})", pat, expr),
      // None => write!(f, "None"),
    }
  }
}

impl Eq for Value {}

pub trait Evaluatable {
  type E = RuntimeError;
  fn evaluate(&self, env: Enviroment) -> (Result<Value, Self::E>, Enviroment);
}
