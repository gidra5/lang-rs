use itertools::Itertools;

use crate::{
  ast::{Expression, PatternWithDefault},
  enviroment::Enviroment,
  types::Type,
};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  // primitives
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  Symbol(u32), // an unique number identifying that symbol

  Type(Box<Type>),                                   // types are first class
  Function(Expression, Box<Enviroment>, Expression), // functions are first class
  None,

  Ref(Rc<RefCell<Value>>), // should somehow simplify type probably to accomodate non mutability
  RefMut(Rc<RefCell<Value>>),

  // 3 levels of composition (the most useful)
  Tuple(Vec<Value>),
  Record(HashMap<String, Value>),
  Map(HashMap<Value, Value>),
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
      Type(t) => write!(f, "type {:?}", t),
      Symbol(t) => write!(f, "symbol {:?}", t),
      Function(pat, _, expr) => write!(f, "({} => {})", pat, expr),
      Ref(val_ref) | RefMut(val_ref) => write!(f, "{}", val_ref.borrow()),
      None => write!(f, "None"),
    }
  }
}

impl Eq for Value {}
