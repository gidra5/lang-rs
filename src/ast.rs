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
  BinaryExpression(
    Option<Box<Expression>>,
    Option<Value>,
    Option<Box<Expression>>,
  ),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
  None,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Operator<'a> {
  fixity: Fixity,
  token:  Option<TokenExt<'a>>,
}

impl<'a> Default for Operator<'a> {
  fn default() -> Operator<'a> {
    let token = TokenExt {
      token: Token::LParenthesis,
      src: "(".to_string(),
      span: Span {
        length: 1,
        stream: CharStream::<'a>::from_str("")
      }
    };
    Operator { token: Some(token), fixity: Fixity::Prefix }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::BinaryExpression(None, Some(op), None) => write!(f, "{}", op),
      Self::BinaryExpression(None, None, None) => write!(f, "None"),
      Self::BinaryExpression(Some(left), op, None) => {
        write!(f, "({:?} {})", op, left)
      },
      Self::BinaryExpression(Some(left), op, Some(right)) => {
        write!(f, "({:?} {} {})", op, left, right)
      },
      Self::BinaryExpression(None, op, Some(right)) => {
        write!(f, "({:?} {})", op, right)
      },
    }
  }
}

impl Expression {
  pub fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, &'static str> {
    expr_bp(token_stream)
  }
  // fn parse2(lexer: &mut TokenStream) -> Option<Expression> {
  //   let mut top = Frame {
  //     operator: None,
  //     lhs:      None,
  //   };
  //   let mut stack = Vec::new();

  //   loop {
  //     let operator = Operator {
  //       token:  lexer.next().map(|token_ext| token_ext.token),
  //       fixity: if top.lhs.is_none() {
  //         Fixity::Prefix
  //       } else {
  //         Fixity::Infix
  //       },
  //     };

  //     let operator = loop {
  //       match lookup(operator.clone()) {
  //         t @ Some(_) if top.operator <= t => break t.unwrap(),
  //         _ => {
  //           let res = top;
  //           top = match stack.pop() {
  //             Some(it) => it,
  //             None => return res.lhs,
  //           };

  //           top.lhs = Some(Expression::BinaryExpression(
  //             top.lhs.map(Box::new),
  //             res.operator.clone().map(|op| op.token).flatten(),
  //             res.lhs.map(Box::new),
  //           ));
  //         },
  //       };
  //     };

  //     if operator.token == Some(Token::RParenthesis) {
  //       assert_eq!(
  //         top.operator.as_ref().map(|op| op.token).flatten(),
  //         Some(Token::LParenthesis)
  //       );
  //       let res = top;
  //       top = stack.pop().unwrap();
  //       top.lhs = res.lhs;
  //       continue;
  //     }

  //     stack.push(top);
  //     top = Frame {
  //       operator: Some(operator),
  //       lhs:      None,
  //     };
  //   }
  // }

  // pub fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, &'static str> {
  //   match Self::parse2(token_stream) {
  //     Some(e) => Ok(e),
  //     None => Err(""),
  //   }
  // }
}

use std::cmp::Ordering;
impl PartialOrd for Operator<'_> {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    let (_, r_bp1) = bp(self.clone())?;
    let (l_bp2, _) = bp(other.clone())?;
    let res = match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
      (false, false) => { println!("{:?} = {:?}", self, other); Some(Ordering::Equal) },
      (true, false) => { println!("{:?} < {:?}", self, other); Some(Ordering::Less) },
      (false, true) => { println!("{:?} > {:?}", self, other); Some(Ordering::Greater) },
      _ => None,
    };
    println!("{:?}", res);
    res
  }
}

#[derive(Debug)]
struct Frame<'a> {
  operator: Option<Operator<'a>>,
  lhs:      Option<Expression>,
}

fn expr_bp(lexer: &mut TokenStream) -> Result<Expression, &'static str> {
  let mut top = Frame {
    operator: None,
    lhs:      None,
  };
  let mut stack = Vec::new();

  loop {
    let token = lexer.next();
    let operator = Operator {
      token:  token.clone(),
      fixity: if top.lhs.is_none() {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    };

    let operator = loop {
      match lookup(operator.clone()) {
        Some(t) if top.operator.clone().unwrap_or_default() <= t => break t,
        _ => {
          let res = top;
          top = match stack.pop() {
            Some(it) => it,
            None => return res.lhs.ok_or("No Expression"),
          };

          top.lhs = Some(Expression::BinaryExpression(
            top.lhs.map(Box::new),
            res
              .operator
              .clone()
              .map(|op| op.token.map(|op| op.value()))
              .flatten(),
            res.lhs.map(Box::new),
          ))
        },
      };
    };

    if let Some(TokenExt {
      token: Token::RParenthesis,
      ..
    }) = operator.token
    {
      if let Some(Operator {
        token: Some(TokenExt {
          token: Token::LParenthesis,
          ..
        }),
        ..
      }) = top.operator
      {
        let res = top;
        top = stack.pop().unwrap();
        top.lhs = res.lhs;
        continue;
      } else {
        return Err("Unexpected closing parenthesis");
      }
    } else if let Some(Operator {
      token: Some(TokenExt {
        token: Token::LParenthesis,
        ..
      }),
      ..
    }) = top.operator
    {
      return Err("Expected closing parenthesis");
    }

    stack.push(top);
    top = Frame {
      operator: Some(operator),
      lhs:      None,
    };
  }
}

fn bp(op: Operator) -> Option<(u8, u8)> { let res = binding_power2(op.clone()).map(|(_, bp)| bp); println!("{:?} {:?}", res, op); res }

fn lookup(op: Operator) -> Option<Operator> { let res = binding_power2(op.clone()).map(|(op, _)| op); println!("{:?} {:?}", res, op); res }

//   loop {
//     let token = lexer.next();

//     let (token, r_bp) = loop {
//       match binding_power(token.clone(), top.lhs.is_none()) {
//         Some((t, (l_bp, r_bp))) if top.min_bp <= l_bp => break (t, r_bp),
//         _ => {
//           let res = top;
//           top = match stack.pop() {
//             Some(it) => it,
//             None => return res.lhs,
//           };

//           top.lhs = Some(Expression::BinaryExpression(
//             top.lhs.map(Box::new),
//             res.token.clone().map(|op| op.value()),
//             res.lhs.map(Box::new),
//           ))
//         },
//       };
//     };

//     if token.token == Token::RParenthesis {
//       assert_eq!(
//         top.token.clone().map(|token| token.token),
//         Some(Token::LParenthesis)
//       );
//       let res = top;
//       top = stack.pop().unwrap();
//       top.lhs = res.lhs;
//       continue;
//     }

//     stack.push(top);
//     top = Frame {
//       min_bp: r_bp,
//       lhs:    None,
//       token:  Some(token),
//     };
//   }
// }

fn binding_power2(op: Operator) -> Option<(Operator, (u8, u8))> {
  use Token::*;
  let res = match op.clone() {
    Operator {
      fixity: Fixity::None,
      token: Some(TokenExt {
        token: Identifier | Number | Boolean | String | Char,
        ..
      }),
    } => (99, 100),
    Operator {
      fixity: Fixity::Postfix,
      token,
    } => {
      (
        match token? {
          TokenExt {
            token: RParenthesis,
            ..
          } => 0,
          TokenExt { token: Bang, .. } => 11,
          _ => return None,
        },
        100,
      )
    },
    Operator {
      fixity: Fixity::Prefix,
      token,
    } => {
      (99, match token? {
        TokenExt {
          token: LParenthesis,
          ..
        } => 0,
        TokenExt {
          token: Add | Sub, ..
        } => 9,
        token => {
          return binding_power2(Operator {
            fixity: Fixity::None,
            token:  Some(token),
          })
        },
      })
    },
    Operator {
      fixity: Fixity::Infix,
      token,
    } => {
      match token? {
        TokenExt { token: Equal, .. } => (2, 1),
        TokenExt {
          token: Add | Sub, ..
        } => (5, 6),
        TokenExt {
          token: Mult | Div, ..
        } => (7, 8),
        TokenExt { token: Period, .. } => (14, 13),
        token => {
          return binding_power2(Operator {
            fixity: Fixity::Postfix,
            token:  Some(token),
          })
        },
      }
    },
    _ => return None,
  };
  Some((op, res))
}
