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
    Option<Token>,
    // Option<Value>,
    Option<Box<Expression>>,
  ),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
  None,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Operator {
  fixity: Fixity,
  token:  Option<Token>,
}

impl Default for Operator {
  fn default() -> Operator {
    Operator {
      fixity: Fixity::None,
      token:  Some(Token::LParenthesis),
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::BinaryExpression(None, op, None) => write!(f, "{:?}", op),
      Self::BinaryExpression(Some(left), op, None) => write!(f, "({:?} {})", op, left),
      Self::BinaryExpression(Some(left), op, Some(right)) => {
        write!(f, "({:?} {} {})", op, left, right)
      },
      Self::BinaryExpression(None, op, Some(right)) => write!(f, "({:?} {})", op, right),
    }
    //   match self {
    //     Self::BinaryExpression(Some(left), Some(op), Some(right)) => {
    //       write!(f, "({:?} {} {})", op, left, right)
    //     },
    //     Self::BinaryExpression(Some(left), None, Some(right)) => {
    //       write!(f, "(FnCall {} {})", left, right)
    //     },
    //     Self::BinaryExpression(None, Some(op), Some(right)) => {
    //       write!(f, "({:?}| {})", op, right)
    //     },
    //     Self::BinaryExpression(None, Some(op), None) => {
    //       write!(f, "{:?}", op)
    //     },
    //     Self::BinaryExpression(Some(left), Some(op), None) => {
    //       write!(f, "(|{:?} {})", op, left)
    //     },
    //     Self::BinaryExpression(left, None, right) => write!(f, "(FnCall {:?}
    // {:?})", left, right),     Self::BinaryExpression(left, op, right) =>
    // {       write!(f, "({:?} {:?} {:?})", op, left, right)
    //     },
    //     Self::Literal(value) => write!(f, "{:?}", value),
    //   }
  }
}

/// return true if op_left < op_right
fn compare(
  Operator {
    fixity: fixity_left,
    token: token1,
  }: &Operator,
  Operator {
    fixity: fixity_right,
    token: token2,
  }: &Operator,
) -> Option<bool> {
  use Token::*;

  println!(
    "{:?}, {:?}",
    Operator {
      fixity: fixity_left.clone(),
      token:  token1.clone(),
    },
    Operator {
      fixity: fixity_right.clone(),
      token:  token2.clone(),
    }
  );

  let token_left = token1.as_ref()?;
  let token_right = token2.as_ref()?;

  Some(match (fixity_left, token_left, fixity_right, token_right) {
    // (_, _, Fixity::Postfix, RParenthesis) => true,
    (Fixity::Prefix, Add, fixity_left, token_left)
    | (Fixity::Prefix, Sub, fixity_left, token_left) => {
      match (fixity_right, token_right) {
        (Fixity::Infix, Add) | (Fixity::Infix, Sub) => false,
        _ => true,
      }
    },
    (Fixity::None, LParenthesis, _, _) | (Fixity::Prefix, LParenthesis, _, _) => true,
    (Fixity::Infix, Add, fixity_right, token_right)
    | (Fixity::Infix, Sub, fixity_right, token_right) => {
      match (fixity_right, token_right) {
        (Fixity::Infix, Add) | (Fixity::Infix, Sub) => false,
        _ => true,
      }
    },
    _ => return None,
  })
}

// fn binding_power(op: char, prefix: bool) -> Option<(u8, u8)> {
//   let res = match op {
//       '=' => (2, 1),
//       '+' | '-' if prefix => (99, 9),
//       '+' | '-' => (5, 6),
//       '*' | '/' => (7, 8),
//       '!' => (11, 100),
//       '.' => (14, 13),
//       _ => return None,
//   };
//   Some(res)
// }

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
  // fn parse2(
  //   token_stream: &mut TokenStream<'_>,
  //   parent_op: Operator,
  // ) -> Result<Expression, &'static str> {
  //   use Token::*;

  //   // let mut lhs = Err("no expr");

  //   // loop {
  //   //   let token = match token_stream.peek() {
  //   //     Some(t) => t,
  //   //     None => return lhs,
  //   //   };

  //   //   let op = Operator {
  //   //     fixity: if lhs.is_err() {
  //   //       Fixity::Prefix
  //   //     } else {
  //   //       Fixity::Infix
  //   //     },
  //   //     token:  Some(token.clone().token),
  //   //   };
  //   //   if compare(&parent_op, &op).unwrap_or(true) {
  //   //     return lhs;
  //   //   }

  //   //   token_stream.next();

  //   //   let rhs = Self::parse2(token_stream, op)?;

  //   //   lhs = Ok(Self::BinaryExpression(
  //   //     lhs.ok().map(|x| Box::new(x)),
  //   //     Some(token.token),
  //   //     Some(Box::new(rhs)),
  //   //   ))
  //   // }

  //   //   fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> Option<S> {
  //   //     let mut lhs = None;

  //   //     loop {
  //   //         let token = match lexer.peek() {
  //   //             Some(token) => token,
  //   //             None => return lhs,
  //   //         };

  //   //         let r_bp = match binding_power(token, lhs.is_none()) {
  //   //             Some((l_bp, r_bp)) if min_bp <= l_bp => r_bp,
  //   //             _ => return lhs,
  //   //         };

  //   //         lexer.next();

  //   //         let rhs = expr_bp(lexer, r_bp);
  //   //         if token == '(' {
  //   //             assert_eq!(lexer.next(), Some(')'));
  //   //             lhs = rhs;
  //   //             continue;
  //   //         }

  //   //         let mut args = Vec::new();
  //   //         args.extend(lhs);
  //   //         args.extend(rhs);
  //   //         lhs = Some(S::Cons(token, args));
  //   //     }
  //   // }

  //   let mut lhs = match token_stream.peek() {
  //     Some(TokenExt { token, .. }) => {
  //       match token {
  //         Identifier | Number | String | Char | Boolean => {
  //           // Ok(Expression::Literal(token_stream.next().unwrap().value()))
  //           Err("")
  //         },
  //         LParenthesis => {
  //           token_stream.next();
  //           let lhs = Self::parse2(token_stream, Operator::default())?;
  //           if let Some(TokenExt {
  //             token: RParenthesis,
  //             ..
  //           }) = token_stream.next()
  //           {
  //             Ok(lhs)
  //           } else {
  //             return Err("no closing parenthesis");
  //           }
  //         },
  //         _ => Err("no expr"),
  //       }
  //     },
  //     None => Err("no expr"),
  //   };

  //   loop {
  //     let token = match token_stream.peek() {
  //       None => break,
  //       Some(TokenExt { token, .. }) => token,
  //     };

  //     // let op = Operator {
  //     //   fixity: Fixity::Postfix,
  //     //   token:  Some(token.clone()),
  //     // };
  //     // if let Some(true) = compare(&parent_op, &op) {
  //     //   token_stream.next();
  //     //   lhs = Expression::BinaryExpression(Some(Box::new(lhs)), Some(token),
  // None);     //   continue;
  //     // }

  //     let op = Operator {
  //       fixity: if lhs.is_err() {
  //         Fixity::Prefix
  //       } else {
  //         Fixity::Infix
  //       },
  //       token:  Some(token.clone()),
  //     };
  //     // if let None = compare(&parent_op, &op)

  //     if let Some(true) = compare(&parent_op, &op) {
  //       token_stream.next();
  //       let rhs = Self::parse2(token_stream, op)?;

  //       lhs = Ok(Expression::BinaryExpression(
  //         lhs.ok().map(Box::new),
  //         Some(token),
  //         Some(Box::new(rhs)),
  //       ));
  //       continue;
  //     }

  //     break;
  //   }

  //   lhs
  // }

  pub fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, &'static str> {
    // Self::parse2(token_stream, Operator::default())
    match expr_bp(token_stream) {
      Some(e) => Ok(e),
      None => Err(""),
    }
  }
}


// #[derive(Debug)]
// struct Frame {
//   operator: Option<Operator>,
//   min_bp:   u8,
//   lhs:      Option<Expression>,
//   // token:    Option<TokenExt<'a>>,
// }

// fn expr_bp(lexer: &mut TokenStream) -> Option<Expression> {
//   let mut top = Frame {
//     min_bp:   0,
//     operator: None,
//     lhs:      None,
//     // token:    None,
//   };
//   let mut stack = Vec::new();

//   loop {
//     let token = lexer.next();

//     let (operator, r_bp) = loop {
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
//             res.operator.clone().map(|op| op.token).flatten(),
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
//       operator: Some(operator),
//       min_bp:   r_bp,
//       lhs:      None,
//       // token:    Some(token),
//     };
//   }
// }


#[derive(Debug)]
struct Frame<'a> {
  min_bp: u8,
  lhs:    Option<Expression>,
  token:  Option<TokenExt<'a>>,
}

fn expr_bp(lexer: &mut TokenStream) -> Option<Expression> {
  let mut top = Frame {
    min_bp: 0,
    lhs:    None,
    token:  None,
  };
  let mut stack = Vec::new();

  loop {
    let token = lexer.next();

    let (token, r_bp) = loop {
      match binding_power(token.clone(), top.lhs.is_none()) {
        Some((t, (l_bp, r_bp))) if top.min_bp <= l_bp => break (t, r_bp),
        _ => {
          let res = top;
          top = match stack.pop() {
            Some(it) => it,
            None => return res.lhs,
          };

          top.lhs = Some(Expression::BinaryExpression(
            top.lhs.map(Box::new),
            res.token.clone().map(|op| op.token),
            res.lhs.map(Box::new),
          ))
        },
      };
    };

    if token.token == Token::RParenthesis {
      assert_eq!(
        top.token.clone().map(|token| token.token),
        Some(Token::LParenthesis)
      );
      let res = top;
      top = stack.pop().unwrap();
      top.lhs = res.lhs;
      continue;
    }

    stack.push(top);
    top = Frame {
      min_bp: r_bp,
      lhs:    None,
      token:  Some(token),
    };
  }
}

fn binding_power2(op: Option<Operator>) -> Option<(Operator, (u8, u8))> {
  use Token::*;
  let op = op?;
  let res = match op.clone() {
    Operator {
      fixity: Fixity::None,
      token: _,
    } => (99, 100),
    Operator {
      fixity: Fixity::Postfix,
      token,
    } => {
      (
        match token? {
          RParenthesis => 0,
          Bang => 11,
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
        LParenthesis => 0,
        Add | Sub => 9,
        _ => return None,
      })
    },
    Operator {
      fixity: Fixity::Infix,
      token,
    } => {
      match token? {
        Equal => (2, 1),
        Add | Sub => (5, 6),
        Mult | Div => (7, 8),
        Period => (14, 13),
        _ => return None,
      }
    },
    _ => return None,
  };
  Some((op, res))
}

fn binding_power(op: Option<TokenExt>, prefix: bool) -> Option<(TokenExt, (u8, u8))> {
  use Token::*;
  let op = op?;
  let res = match op.token {
    Identifier | Number | String | Char | Boolean => (99, 100),
    LParenthesis => (99, 0),
    RParenthesis => (0, 100),
    Equal => (2, 1),
    Add | Sub if prefix => (99, 9), //prefix
    Add | Sub => (5, 6),
    Mult | Div => (7, 8),
    Bang => (11, 100), // postfix
    Period => (14, 13),
    _ => return None,
  };
  Some((op, res))
}
