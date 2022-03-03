use std::{
  cell::RefCell,
  cmp::Ordering,
  collections::HashMap,
  fmt::{Display, Formatter},
  hash::Hash,
  rc::Rc,
};

use super::expr_eval::*;
use either::Either;
use itertools::Itertools;

use crate::{
  check_token,
  check_token_end,
  common::{reversable_iterator::ReversableIterator, value, LoggerTrait, Span, Value},
  enviroment::Enviroment,
  map,
  match_token,
  punct_or_newline,
  scoped,
  skip,
  token::{self, TokenExt},
  token_pat,
};

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
  Value(TokenExt),
  Record(Vec<RecordItem>),

  Block(Vec<Expression>),
  If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
  For(Box<Expression>, Box<Expression>, Box<Expression>),

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
  fn default() -> Expression { Expression::Value(TokenExt::default()) }
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
      Expression::If(cond, t_b, Some(f_b)) => write!(f, "(if {}: {} else {})", cond, t_b, f_b),
      Expression::If(cond, t_b, None) => write!(f, "(if {}: {} else None)", cond, t_b),
      Expression::For(pat, iter, body) => write!(f, "(for {} in {}: {})", pat, iter, body),
      Expression::Block(stmts) => {
        write!(
          f,
          "{{\n{}\n}}",
          stmts.iter().map(|stmt| format!("\t{}", stmt)).join(";\n")
        )
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
      (Some(expr), None) => {
        Expression::Postfix {
          left: Box::new(expr),
          op:   Box::new(op),
        }
      },
      (Some(left), Some(right)) => {
        Expression::Infix {
          left:  Box::new(left),
          op:    Box::new(op),
          right: Box::new(right),
        }
      },
      (None, Some(right)) => {
        Expression::Prefix {
          op:    Box::new(op),
          right: Box::new(right),
        }
      },
    }
  }
}


// #[macro_export]
// macro_rules! record_key_pat {
//   ([$expr:tt]) => {
//     RecordKey::Value(expr_pat!($expr:tt))
//   };
//   ($name:ident) => {
//     RecordKey::Identifier(stringify!($name))
//   };
//   () => {
//     RecordKey::None
//   };
// }

// #[macro_export]
// macro_rules! expr_node_pat {
//   ({ $($inner:tt);* }) => {
//     Op::Block(vec![$($inner:tt),*])
//   };
//   (( $($($name:tt:)? $expr:tt);* )) => {
//     Op::Record([$(RecordItem { key: record_key_pat!($($name)?), value:
// $expr}),*])   };
//   (( $([$key:tt]: $expr:tt);* )) => {
//     Op::Record([$(RecordItem { key: record_key_pat!($key), value: $expr}),*])
//   };
// }

// #[macro_export]
// macro_rules! expr_pat {
//   ($node:tt $(, left $(: ident $left:ident)?)? $(, right $(: ident
// $right:ident)?)?) => {     Expression {
//       op: expr_node_pat!($node),
//       left $(: expr_pat!(sub ident $($left)?))?,
//       right $(: expr_pat!(sub ident $($right)?))?,
//     }
//   };
//   (value $($token_ident:ident $(: $($token_pat:ident)|+)? $(, src
// $src_ident:ident$(: $($src_pat:pat)|+)? )?)? $(, left $(: ident
// $left:ident)?)? $(, right $(: ident $right:ident)?)?) => {     Expression {
//       op: Op::Value(token_pat!($($token_ident $(: $($token_pat)|+)? $(,
// $src_ident $(: $($src_pat)|+)? )?)?)),       left $(: expr_pat!(sub $(ident
// $left)?))?,       right $(: expr_pat!(sub $(ident $right)?))?,
//     }
//   };
//   ($node:tt $(, left $(: $left:tt)?)? $(, right $(: $right:tt)?)?) => {
//     Expression {
//       op: expr_node_pat!($node),
//       left $(: expr_pat!(sub $($left)?))?,
//       right $(: expr_pat!(sub $($right)?))?,
//     }
//   };
//   (value $($token_ident:ident $(: $($token_pat:ident)|+)? $(, src
// $src_ident:ident$(: $($src_pat:pat)|+)? )?)? $(, left $(: $left:tt)?)? $(,
// right $(: $right:tt)?)?) => {     Expression {
//       op: Op::Value(token_pat!($($token_ident $(: $($token_pat)|+)? $(,
// $src_ident $(: $($src_pat)|+)? )?)?)),       left $(: expr_pat!(sub
// $($left)?))?,       right $(: expr_pat!(sub $($right)?))?,
//     }
//   };
//   (sub ident $ident:ident) => {
//     $ident
//   };
//   (sub _) => {
//     _
//   };
//   (sub) => {
//     None
//   };
//   (sub $node:tt) => {
//     Some(box expr_pat!($node))
//   };
// }
