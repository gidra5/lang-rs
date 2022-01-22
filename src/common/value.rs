use crate::{enviroment::Enviroment, Token};
use std::{cell::RefCell, fmt::Display, rc::Rc};

use super::logger::char_stream::{Evaluatable, Expression};

#[derive(Clone, Debug, PartialEq)]
pub struct RecordItem {
  pub key:   Option<Value>,
  pub value: Value,
}

// #[derive(Clone, Debug, PartialEq)]
// pub struct RecordItem {
//   pub key:   Value,
//   pub value: Value,
// }

// #[derive(Clone, Debug, PartialEq)]
// pub enum Record {
//   Tuple(Vec<Value>),
//   Record(Vec<RecordItem>),
//   /* Record(HashMap<String, Value>),
//    * Map(HashMap<Value, Value>), */
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Identifier(String),
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  Operator(Token),
  Record(Vec<RecordItem>),
  Function(Box<Expression>, Rc<RefCell<Enviroment>>, Box<Expression>),
  Unit,
  Placeholder,
  None,
}

impl Default for Value {
  fn default() -> Self { Value::None }
}

impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use crate::Value::*;
    match self {
      Identifier(s) => write!(f, "{}", s),
      String(s) => write!(f, "\"{}\"", s),
      Number(num) => write!(f, "{}", num),
      Boolean(b) => write!(f, "{}", b),
      Char(c) => write!(f, "'{}'", c),
      Operator(op) => write!(f, "{:?}", op),
      Record(record_items) => write!(f, "(record)"),
      Function(_, _, _) => write!(f, "(function)"),
      Unit => write!(f, "()"),
      Placeholder => write!(f, "_"),
      None => write!(f, "None"),
    }
  }
}

fn fact(n: f64) -> f64 {
  if n <= 1. {
    1.
  } else {
    n * fact(n - 1.)
  }
}

impl Value {
  pub fn prefix(self, operand: Value) -> Value {
    match self {
      Value::Operator(operator) => {
        match (operator, operand) {
          (Token::Sub, Value::Number(num)) => Value::Number(-num),
          (Token::Bang, Value::Boolean(val)) => Value::Boolean(!val),
          (Token::Dec, Value::Number(val)) => Value::Number(val - 1.),
          (Token::Inc, Value::Number(val)) => Value::Number(val + 1.),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }
  pub fn postfix(self, operand: Value) -> Value {
    match self {
      Value::Operator(operator) => {
        match (operator, operand) {
          (Token::Bang, Value::Number(num)) => Value::Number(fact(num)),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }
  pub fn infix(self, left: Value, right: Value) -> Value {
    match self {
      Value::Operator(operator) => {
        match (left, operator, right) {
          (Value::Number(left), Token::Sub, Value::Number(right)) => Value::Number(left - right),
          (Value::Number(left), Token::Add, Value::Number(right)) => Value::Number(left + right),
          (Value::Number(left), Token::Mult, Value::Number(right)) => Value::Number(left * right),
          (Value::Number(left), Token::Div, Value::Number(right)) => Value::Number(left / right),
          (Value::Number(left), Token::Pow, Value::Number(right)) => {
            Value::Number(left.powf(right))
          },
          (Value::Number(left), Token::EqualEqual, Value::Number(right)) => {
            Value::Boolean((left - right).abs() < f64::EPSILON)
          },
          (Value::Number(left), Token::LessEqual, Value::Number(right)) => {
            Value::Boolean(left <= right)
          },
          (Value::Number(left), Token::GreaterEqual, Value::Number(right)) => {
            Value::Boolean(left >= right)
          },
          (Value::Number(left), Token::LAngleBracket, Value::Number(right)) => {
            Value::Boolean(left < right)
          },
          (Value::Number(left), Token::RAngleBracket, Value::Number(right)) => {
            Value::Boolean(left > right)
          },
          (Value::Record(left), Token::Period, Value::Identifier(right)) => {
            left
              .iter()
              .filter_map(|RecordItem { key, value }| {
                match key {
                  Some(Value::String(name) | Value::Identifier(name)) if name.clone() == right => {
                    Some(value.clone())
                  },
                  _ => None,
                }
              })
              .next()
              .unwrap_or_default()
          },
          (Value::Record(left), Token::LBrace, Value::Number(right))
            if right == (right as usize) as f64 && left[0].key == None =>
          {
            left[right as usize].value.clone()
          },
          (Value::Record(left), Token::LBrace, right) => {
            left
              .iter()
              .filter_map(|RecordItem { key, value }| {
                match key {
                  Some(key) if key.clone() == right => Some(value.clone()),
                  _ => None,
                }
              })
              .next()
              .unwrap_or_default()
          },
          _ => Value::None,
        }
      },
      Value::Identifier(operator) => {
        match (left, operator.as_str(), right) {
          (Value::Number(left), "mod", Value::Number(right)) => Value::Number(left % right),
          (Value::Boolean(left), "and", Value::Boolean(right)) => Value::Boolean(left && right),
          (Value::Boolean(left), "or", Value::Boolean(right)) => Value::Boolean(left || right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }
}
