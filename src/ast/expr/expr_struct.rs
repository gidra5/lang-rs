use std::{
  fmt::{Display, Formatter},
  hash::Hash,
};

use itertools::Itertools;

use crate::token::Token;



#[derive(Clone, PartialEq, Debug)]
pub enum RecordKey {
  None,
  Identifier(String),
  Value(Expression),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordItem {
  pub key:   RecordKey,
  pub value: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
  Value(Token),
  Record(Vec<RecordItem>),

  Block(Vec<Expression>),
  Mixfix {
    op:       Vec<Token>,
    operands: Vec<Expression>,
  },
  Prefix {
    op:    Box<Expression>,
    right: Box<Expression>,
  },
  Postfix {
    left: Box<Expression>,
    op:   Box<Expression>,
  },
  Infix {
    left:  Box<Expression>,
    op:    Box<Expression>,
    right: Box<Expression>,
  },
}

impl Eq for Expression {}

impl Hash for Expression {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) { core::mem::discriminant(self).hash(state); }
}

impl Default for Expression {
  fn default() -> Expression { Expression::Value(Token::default()) }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Expression::Value(op) => write!(f, "{}", op),
      Expression::Record(record_items) => {
        let mut x = record_items
          .iter()
          .map(|RecordItem { key, value }| {
            (
              match key {
                RecordKey::Value(expr) => Some(format!("{}", expr)),
                RecordKey::Identifier(name) => Some(name.clone()),
                RecordKey::None => None,
              },
              format!("{}", value),
            )
          })
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].0 != None) {
          write!(
            f,
            "({})",
            x.iter()
              .map(|(name, expr)| {
                match name {
                  Some(x) => format!("{}: {}", x, expr),
                  None => format!("{}", expr),
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
      Expression::Block(stmts) => {
        write!(
          f,
          "{{\n{}\n}}",
          stmts.iter().map(|stmt| format!("\t{}", stmt)).join(";\n")
        )
      },
      Expression::Mixfix {
        op: operator,
        operands,
      } => {
        write!(f, "(mixfix {:?} {:?})", operator, operands)
      },
      Expression::Infix { left, op, right } => write!(f, "({} {} {})", op, left, right),
      Expression::Prefix { op, right } => write!(f, "({} {})", op, right),
      Expression::Postfix { left, op } => write!(f, "({} {})", op, left),
    }
  }
}

impl Expression {
  pub fn from_options(
    op: Expression,
    left: Option<Expression>,
    right: Option<Expression>,
  ) -> Expression {
    match (left, right) {
      (None, None) => op,
      (Some(expr), None) => Expression::Postfix {
        left: Box::new(expr),
        op:   Box::new(op),
      },
      (Some(left), Some(right)) => Expression::Infix {
        left:  Box::new(left),
        op:    Box::new(op),
        right: Box::new(right),
      },
      (None, Some(right)) => Expression::Prefix {
        op:    Box::new(op),
        right: Box::new(right),
      },
    }
  }
}
