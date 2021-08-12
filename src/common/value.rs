use crate::Token;
use std::fmt::Display;
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Identifier(String),
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  Operator(Token),
  None,
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
          _ => Value::None,
        }
      },
      Value::Identifier(operator) => {
        match (left, operator.as_str(), right) {
          (Value::Number(left), "mod", Value::Number(right)) => Value::Number(left % right),
          // (Value::Number(left), Token::Add, Value::Number(right)) => Value::Number(left+right),
          // (Value::Number(left), Token::Mult, Value::Number(right)) => Value::Number(left*right),
          // (Value::Number(left), Token::Div, Value::Number(right)) => Value::Number(left/right),
          _ => Value::None,
        }
      },
      _ => Value::None,
    }
  }
  // pub fn add(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left + right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn sub(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left - right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn mult(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left * right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn div(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left / right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn rem(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left % right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn pow(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Number(left) => {
  //       match right {
  //         Number(right) => Value::Number(left.powf(right)),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // /// Equal
  // pub fn e(self, right: Value) -> Value { Value::Boolean(self == right) }

  // /// Less or equal
  // pub fn le(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Number(left) => {
  //       match right {
  //         Value::Number(right) => Value::Boolean(left <= right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // /// Greater or equal
  // pub fn ge(self, right: Value) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Number(left) => {
  //       match right {
  //         Value::Number(right) => Value::Boolean(left >= right),
  //         _ => Value::None,
  //       }
  //     },
  //     _ => Value::None,
  //   }
  // }

  // pub fn neg(self) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Number(val) => Value::Number(-val),
  //     _ => Value::None,
  //   }
  // }

  // pub fn inv(self) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Boolean(val) => Value::Boolean(!val),
  //     _ => Value::None,
  //   }
  // }

  // pub fn dec(self) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Number(val) => Value::Number(val - 1.),
  //     _ => Value::None,
  //   }
  // }

  // pub fn inc(self) -> Value {
  //   use Value::*;
  //   match self {
  //     Value::Number(val) => Value::Number(val + 1.),
  //     _ => Value::None,
  //   }
  // }
}
