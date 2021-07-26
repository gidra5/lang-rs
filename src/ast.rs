#![allow(unused)]
use crate::{common::*, token::*};
use std::{
  collections::{HashMap, HashSet},
  fmt::Display,
  hash::{Hash, Hasher},
};

#[path = "tests/ast.rs"]
mod tests;

// pub enum ErrorType {}

pub struct ParsingError<'a> {
  // pub error_type: ErrorType,
  pub span: Span<TokenStream<'a>>,
  pub msg:  String,
}

pub trait Parseable<'a>
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream<'a>) -> Result<ASTNodeExt<'a, Self>, ParsingError<'a>>;
}

pub trait Synchronizable<'a> {
  fn synchronize(stream: &mut TokenStream<'a>) {
    stream.next();
    while !Self::sync_point(stream) && stream.peek().is_some() {
      stream.next();
    }
  }

  fn sync_point(stream: &mut TokenStream<'a>) -> bool { false }
}

/*
  Syntax definition:

  enter expression to evaluate it or i to enter interactive mode
*/


#[derive(Clone, Debug)]
pub struct ASTNodeExt<'a, T> {
  pub node: T,
  pub span: Span<TokenStream<'a>>,
}

#[derive(Debug, Clone)]
pub struct Expression {
  left:  Option<Box<Expression>>,
  op:    Value,
  right: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
  None,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Operator {
  fixity: Fixity,
  value:  Value,
}

impl<'a> Default for Operator {
  fn default() -> Operator {
    Operator {
      value:  Value::None,
      fixity: Fixity::None,
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self {
        left: None,
        op,
        right: None,
      } => write!(f, "{}", op),
      Self {
        left: Some(left),
        op,
        right: None,
      } => {
        match op {
          Value::Operator(op) => write!(f, "({:?} {})", op, left),
          _ => unreachable!(),
        }
      },
      Self {
        left: Some(left),
        op,
        right: Some(right),
      } => {
        match op {
          Value::Operator(op) => write!(f, "({:?} {} {})", op, left, right),
          _ => unreachable!(),
        }
      },
      Self {
        left: None,
        op,
        right: Some(right),
      } => {
        match op {
          Value::Operator(op) => write!(f, "({:?} {})", op, right),
          _ => unreachable!(),
        }
      },
    }
  }
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

impl Expression {
  fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, &'static str> {
    let mut top = Frame {
      lhs:      None,
      operator: None,
    };
    let mut stack = Vec::new();
    loop {
      let token = token_stream.next();
      let operator = loop {
        let operator = token
          .clone()
          .map(|token| Operator::new(token, top.lhs.is_none()))
          .flatten();
        match operator {
          // Some(None) => return Err("No such operator"),
          // Some(t @ Some(op)) if top.operator <= t => break op,
          Some(op) if top.operator <= Some(op.clone()) => break op,
          _ => {
            let res = top;
            if let Some(Expression {
              op: Value::Operator(Token::LParenthesis),
              left: None,
              right: Some(_),
            }) = res.lhs
            {
              return Err("Expected closing parenthesis");
            }

            // println!(
            //   "a: {:?}, {:?}, {:?}, {:?}, {:?}",
            //   res.operator,
            //   operator,
            //   res.operator <= operator,
            //   token,
            //   stack
            // );

            top = match stack.pop() {
              Some(it) => it,
              None => return res.lhs.ok_or("No expression"),
            };

            // println!("b: {:?}, {:?}, {:?}", res, top, stack);

            top.lhs = Some(Expression {
              op:    res.operator.unwrap().value,
              left:  top.lhs.map(Box::new),
              right: res.lhs.map(Box::new),
            });
          },
        };
      };
      // println!("c: {:?}, {:?}, {:?}", operator, top, stack);
      if let Operator {
        value: Value::Operator(Token::RParenthesis),
        fixity: Fixity::Postfix,
      } = operator
      {
        if let Some(Operator {
          value: Value::Operator(Token::LParenthesis),
          fixity: Fixity::Prefix,
        }) = top.operator
        {
          let res = top;
          top = stack.pop().unwrap();
          top.lhs = res.lhs;
          continue;
        } else {
          return Err("Unexpected closing parenthesis");
        }
      }

      stack.push(top);
      top = Frame {
        lhs:      None,
        operator: Some(operator),
      };
    }
  }
}

impl<'a> Parseable<'a> for Expression {
  fn parse(stream: &mut TokenStream<'a>) -> Result<ASTNodeExt<'a, Expression>, ParsingError<'a>> {
    let mut span = Span {
      stream: stream.clone(),
      length: 1,
    };
    let res = Self::parse(stream);

    span.length = stream.pos() - span.pos();

    match res {
      Ok(node) => Ok(ASTNodeExt { node, span }),
      Err(msg) => {
        Err(ParsingError {
          msg: msg.to_string(),
          span,
        })
      },
    }
  }
}

impl Operator {
  pub fn new(token: TokenExt, prefix: bool) -> Option<Self> {
    let op = Operator {
      value:  token.value(),
      fixity: if prefix {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    };

    if op.exist() {
      return Some(op);
    }

    let op = Operator {
      fixity: if prefix {
        Fixity::None
      } else {
        Fixity::Postfix
      },
      ..op
    };

    if op.exist() {
      return Some(op);
    }

    None
  }

  fn exist(&self) -> bool {
    match self {
      Operator {
        value:
          Value::Identifier(_)
          | Value::String(_)
          | Value::Char(_)
          | Value::Number(_)
          | Value::Boolean(_),
        fixity: Fixity::None,
      } => true,
      Operator {
        value:
          Value::Operator(Token::LParenthesis)
          | Value::Operator(Token::Add)
          | Value::Operator(Token::Inc)
          | Value::Operator(Token::Dec)
          | Value::Operator(Token::Mult)
          | Value::Operator(Token::Sub),
        fixity: Fixity::Prefix,
      } => true,
      Operator {
        value: Value::Operator(Token::RParenthesis) | Value::Operator(Token::Bang),
        fixity: Fixity::Postfix,
      } => true,
      Operator {
        value:
          Value::Operator(Token::Period)
          | Value::Operator(Token::Equal)
          | Value::Operator(Token::EqualEqual)
          | Value::Operator(Token::Add)
          | Value::Operator(Token::Sub)
          | Value::Operator(Token::Mod)
          | Value::Operator(Token::Mult)
          | Value::Operator(Token::Div),
        fixity: Fixity::Infix,
      } => true,
      _ => false,
    }
  }

  fn bp(&self) -> Option<(u8, u8)> {
    Some(match self {
      Operator {
        value:
          Value::Identifier(_)
          | Value::String(_)
          | Value::Char(_)
          | Value::Number(_)
          | Value::Boolean(_),
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        value,
        fixity: Fixity::Prefix,
      } => {
        (99, match value {
          Value::Operator(token) => {
            match token {
              Token::LParenthesis => 0,
              Token::Add | Token::Sub => 9,
              Token::Inc | Token::Dec => 11,
              Token::Mult => 13,
              _ => return None,
            }
          },
          _ => return None,
        })
      },
      Operator {
        value,
        fixity: Fixity::Postfix,
      } => {
        (
          match value {
            Value::Operator(token) => {
              match token {
                Token::RParenthesis => 0,
                Token::Bang => 15,
                _ => return None,
              }
            },
            _ => return None,
          },
          100,
        )
      },
      Operator {
        value,
        fixity: Fixity::Infix,
      } => {
        match value {
          Value::Operator(token) => {
            match token {
              Token::Period => (18, 17),
              Token::Equal => (2, 1),
              Token::Add | Token::Sub => (5, 6),
              Token::Mult | Token::Div => (7, 8),
              Token::EqualEqual => (20, 19),
              _ => return None,
            }
          },
          _ => return None,
        }
      },
      _ => return None,
    })
  }
}

use std::cmp::Ordering;
impl PartialOrd for Operator {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    let (_, r_bp1) = self.bp()?;
    let (l_bp2, _) = other.bp()?;

    Some(match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
      (false, false) => Ordering::Equal,
      (true, false) => Ordering::Less,
      (false, true) => Ordering::Greater,
      _ => return None,
    })
  }
}

fn expr(input: &str) -> Result<Expression, &'static str> {
  let mut stream =
    TokenStream::new(CharStream::from_str(input)).ok_or("Failed to create TokenStream")?;

  Expression::parse(&mut stream)
}

#[test]
fn tests() {
  let s = expr("1").unwrap();
  assert_eq!(s.to_string(), "1");

  let s = expr("1 + 2 * 3").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");

  let s = expr("a + b * c * d + e").unwrap();
  assert_eq!(s.to_string(), "(Add (Add a (Mult (Mult b c) d)) e)");

  let s = expr("f . g . h").unwrap();
  assert_eq!(s.to_string(), "(Period f (Period g h))");

  let s = expr(" 1 + 2 + f . g . h * 3 * 4").unwrap();
  assert_eq!(
    s.to_string(),
    "(Add (Add 1 2) (Mult (Mult (Period f (Period g h)) 3) 4))"
  );

  let s = expr("--1 * 2").unwrap();
  assert_eq!(s.to_string(), "(Mult (Sub (Sub 1)) 2)");
  // assert_eq!(s.to_string(), "(Mult (Dec 1) 2)");

  let s = expr("--f . g").unwrap();
  assert_eq!(s.to_string(), "(Sub (Sub (Period f g)))");

  let s = expr("-9!").unwrap();
  assert_eq!(s.to_string(), "(Sub (Bang 9))");

  let s = expr("f . g !").unwrap();
  assert_eq!(s.to_string(), "(Bang (Period f g))");

  let s = expr("(((0)))").unwrap();
  assert_eq!(s.to_string(), "0");

  let s = expr("(1 + 2) * 3").unwrap();
  assert_eq!(s.to_string(), "(Mult (Add 1 2) 3)");

  let s = expr("1 + (2 * 3)").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");

  let s = expr("(1 + (2 * 3)").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");

  let s = expr("1 + (2 * 3))").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");

  let s = expr("1 + 2 * 3)").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");

  let s = expr("1 + (2 * 3").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");
}
