#![allow(unused)]
use crate::{common::*, token::*};
use std::{
  collections::{HashMap, HashSet},
  fmt::Display,
  hash::{Hash, Hasher},
};

#[path = "tests/ast.rs"]
mod tests;

pub enum ErrorType {}

pub struct ParsingError<'a> {
  pub error_type: ErrorType,
  pub span:       Span<TokenStream<'a>>,
  pub msg:        String,
}

pub trait Parseable<'a>
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream<'a>) -> Result<ASTNodeExt<'a, Self>, ParsingError<'a>>;
}

/*
  Syntax definition:

  enter expression to evaluate it or i to enter interactive mode
*/

macro_rules! set {
  ($($item: expr),*) => {{
    let mut set = HashSet::new();
    $(
      set.insert($item);
    )*
    set
  }};
}

macro_rules! map {
  ($($key: expr => $value: expr),*) => {{
    let mut map = HashMap::new();
    $(
      map.insert($key, $value);
    )*
    map
  }};
}

#[derive(Clone, Debug)]
pub struct ASTNodeExt<'a, T> {
  pub node: T,
  pub span: Span<TokenStream<'a>>,
}

#[derive(Debug)]
pub enum Expression {
  // BinaryExpression(Option<Box<Expression>>, Token, Option<Box<Expression>>),
  BinaryExpression(
    Option<Box<Expression>>,
    Option<Token>,
    Option<Box<Expression>>,
  ),
  Literal(Value),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
  None,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Operator {
  fixity: Fixity,
  token:  Option<Token>,
}

impl Default for Operator {
  fn default() -> Operator {
    Operator {
      fixity: Fixity::None,
      token:  None,
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::BinaryExpression(Some(left), Some(op), Some(right)) => {
        write!(f, "({:?} {} {})", op, left, right)
      },
      Self::BinaryExpression(Some(left), None, Some(right)) => {
        write!(f, "(FnCall {} {})", left, right)
      },
      Self::BinaryExpression(left, None, right) => write!(f, "(FnCall {:?} {:?})", left, right),
      Self::BinaryExpression(left, op, right) => {
        write!(f, "({:?} {:?} {:?})", op, left, right)
      },
      Self::Literal(value) => write!(f, "{:?}", value),
    }
  }
}

fn compare(op1: &Operator, op2: &Operator) -> bool {
  match op2 {
    Operator {
      fixity: _,
      token: Some(Token::RParenthesis),
    } => return Some(false),
    _ => ()
  };

  match op1 {
    Operator {
      fixity: Fixity::Infix,
      token: Some(Token::Add),
    } => {
      match op2 {
        Operator {
          fixity: Fixity::Infix,
          token: Some(Token::Add),
        } => false,
        _ => false,
      }
    },
    Operator {
      fixity: Fixity::Infix,
      token: Some(Token::Sub),
    } => {
      match op2 {
        Operator {
          fixity: Fixity::Infix,
          token: Some(Token::Sub),
        } => true,
        _ => false,
      }
    },
    _ => true,
  }
}

// fn prefix_binding_power(op: char) -> ((), u8) {
//   match op {
//       '+' | '-' => ((), 9),
//       _ => panic!("bad op: {:?}", op),
//   }
// }

// fn postfix_binding_power(op: char) -> Option<(u8, ())> {
//   let res = match op {
//       '!' => (11, ()),
//       '[' => (11, ()),
//       _ => return None,
//   };
//   Some(res)
// }

// fn infix_binding_power(op: char) -> Option<(u8, u8)> {
//   let res = match op {
//       '=' => (2, 1),
//       '?' => (4, 3),
//       '+' | '-' => (5, 6),
//       '*' | '/' => (7, 8),
//       '.' => (14, 13),
//       _ => return None,
//   };
//   Some(res)
// }

impl Expression {
  fn parse2(
    token_stream: &mut TokenStream<'_>,
    parent_op: Operator,
  ) -> Result<Expression, &'static str> {
    use Token::*;

    let mut lhs = match token_stream.next() {
      Some(ref token_ext @ TokenExt { ref token, .. }) => {
        match token {
          Identifier | Number | String | Char | Boolean => Expression::Literal(token_ext.value()),
          LParenthesis => {
            let lhs = Self::parse2(token_stream, Operator::default())?;
            if let Some(TokenExt {
              token: RParenthesis,
              ..
            }) = token_stream.next()
            {
              lhs
            } else {
              return Err("no closing parenthesis");
            }
          },
          op => {
            let rhs = Self::parse2(token_stream, Operator {
              fixity: Fixity::Prefix,
              token:  Some(op.clone()),
            })?;
            Expression::BinaryExpression(None, Some(op.clone()), Some(Box::new(rhs)))
          },
        }
      },
      None => return Err("end of stream"),
    };

    loop {
      let (op, token) = match token_stream.peek() {
        None => break,
        Some(TokenExt { token, .. }) => {
          (
            Operator {
              fixity: Fixity::Infix,
              token:  Some(token.clone()),
            },
            token,
          )
        },
      };

      if !compare(&parent_op, &op) {
        break;
      }

      token_stream.next();
      let rhs = Self::parse2(token_stream, op)?;

      lhs = Expression::BinaryExpression(Some(Box::new(lhs)), Some(token), Some(Box::new(rhs)));
    }

    Ok(lhs)
  }

  pub fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, &'static str> {
    Self::parse2(token_stream, Operator::default())
  }
}
