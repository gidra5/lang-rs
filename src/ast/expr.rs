use std::{
  cell::RefCell,
  cmp::Ordering,
  fmt::{Display, Formatter},
  rc::Rc,
};

use itertools::Itertools;

use crate::{
  common::{
    char_stream::{value::Value, Token, TokenExt, TokenStream},
    logger::char_stream::LoggerTrait,
    reversable_iterator::ReversableIterator,
  },
  enviroment::Enviroment,
};

use super::{Evaluatable, Parseable};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  Binary {
    left:  Option<Box<Expression>>,
    op:    Box<Expression>,
    right: Option<Box<Expression>>,
  },
  Tuple(Vec<Expression>),
  Value(Value),
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
  expr:   Expression,
}

impl Default for Expression {
  fn default() -> Expression {
    Expression::Binary {
      left:  None,
      op:    Box::new(Expression::Value(Value::None)),
      right: None,
    }
  }
}

impl Default for Operator {
  fn default() -> Operator {
    Operator {
      expr:   Expression::Value(Value::None),
      fixity: Fixity::None,
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Binary {
        left: None,
        op,
        right: None,
      } => write!(f, "{}", op),
      Self::Binary {
        left: Some(left),
        op,
        right: None,
      } => write!(f, "({} {})", op, left),
      Self::Binary {
        left: None,
        op,
        right: Some(right),
      } => write!(f, "({} {})", op, right),
      Self::Binary {
        left: Some(left),
        op,
        right: Some(right),
      } => write!(f, "({} {} {})", op, left, right),
      Self::Tuple(exprs) => write!(f, "({})", exprs.iter().join(", ")),
      Self::Value(value) => write!(f, "{}", value),
    }
  }
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(self, env: &mut Rc<RefCell<Enviroment>>, logger: &mut L) -> Value {
    match self {
      Expression::Binary { left, op, right } => {
        match (left, (*op).evaluate(env, logger), right) {
          (None, Value::Identifier(id), None) => env.borrow().get(id).unwrap_or_default(),

          (None, op, None) => op,

          (Some(left), op, None) => op.postfix((*left).evaluate(env, logger)),

          (None, op, Some(right)) => op.prefix((*right).evaluate(env, logger)),

          (Some(left), op @ Value::Operator(Token::Equal), Some(right)) => {
            let left = (*left).evaluate(env, logger);
            let right = (*right).evaluate(env, logger);

            if let Value::Identifier(left) = left {
              env.borrow_mut().set(left, right.clone());
              // right
            } /* else {
                op.infix(left, right)
              } */
            right
          },

          (Some(left), op, Some(right)) => {
            op.infix(
              (*left).evaluate(env, logger),
              (*right).evaluate(env, logger),
            )
          },
        }
      },
      Expression::Tuple(exprs) => {
        Value::Tuple(
          exprs
            .into_iter()
            .map(|expr| expr.evaluate(env, logger))
            .collect_vec(),
        )
      },
      Expression::Value(value) => value,
    }
  }
}

impl<'a> Parseable<'a> for Expression {
  fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
    let mut top = Frame {
      lhs:      None,
      operator: None,
    };
    let mut stack = Vec::new();
    loop {
      let token = token_stream.peek();
      println!("3 {:?}, {:?}, {:?}", stack, top, token);
      let mut operator = loop {
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
            // println!("4 {:?}, {:?}, {:?}", stack, res.clone(), token);

            top = match stack.pop() {
              Some(it) => it,
              None => return Ok(res.lhs.unwrap_or_default()),
            };

            top.lhs = Some(Expression::Binary {
              op:    Box::new(res.operator.unwrap().expr),
              left:  top.lhs.map(Box::new),
              right: res.lhs.map(Box::new),
            });
          },
        };
      };
      token_stream.next();
      println!(
        "{:?}, {:?}, {:?}, {:?}",
        token_stream.peek(),
        operator,
        stack,
        top
      );

      // if let Operator {
      //   value: Value::Operator(Token::LParenthesis),
      //   fixity: Fixity::Prefix,
      // } = operator
      // {
      //   let mut exprs = vec![];
      //   let TokenExt {
      //     token: first_token, ..
      //   } = token_stream.peek().ok_or("Unexpected end of expression")?;

      //   match first_token {
      //     Token::RParenthesis => {},
      //     _ => {

      //       // exprs.push(Expression::parse(token_stream)?);
      //     },
      //   }

      //   // println!("7 {:?}", token_stream.peek());

      //   while let Some(t @ TokenExt { token, .. }) = token_stream.peek() {
      //     match token {
      //       Token::Comma => {
      //         token_stream.next();

      //         if matches!(
      //           token_stream.peek(),
      //           Some(TokenExt {
      //             token: Token::RParenthesis,
      //             src:   _,
      //             span:  _,
      //           })
      //         ) {
      //           break;
      //         }

      //         exprs.push(Expression::parse(token_stream)?);
      //       },
      //       Token::RParenthesis => {
      //         break;
      //       },
      //       _ => return Err("Expected closing parenthesis or comma".to_string()),
      //     }
      //   }

      //   // println!("6 {:?}", token_stream.peek());

      //   if !matches!(
      //     token_stream.next(),
      //     Some(TokenExt {
      //       token: Token::RParenthesis,
      //       src:   _,
      //       span:  _,
      //     })
      //   ) {
      //     return Err("Expected closing parenthesis".to_string());
      //   }

      //   // println!("5 {:?}", token_stream.peek());

      //   top.lhs = Some(Expression::Tuple(exprs));
      //   // stack.push(top);
      //   // top = Frame {
      //   //   lhs:      Some(Expression::Tuple(exprs)),
      //   //   operator: None,
      //   // };
      //   continue;
      // }

      // println!("2 {:?}, {:?}, {:?}", operator, stack, top);
      stack.push(top);
      top = Frame {
        lhs:      None,
        operator: Some(operator),
      };
    }
  }
}

impl Operator {
  pub fn new(token: TokenExt, prefix: bool) -> Option<Self> {
    let op = Operator {
      expr:   token.expr(),
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
        expr:
          Expression::Value(
            Value::Identifier(_)
            | Value::String(_)
            | Value::Char(_)
            | Value::Number(_)
            | Value::Boolean(_),
          ),
        fixity: Fixity::None,
      } => true,
      Operator {
        expr: Expression::Tuple(exprs),
        fixity: Fixity::None,
      } if exprs.len() == 0 => true,
      Operator {
        expr:
          Expression::Value(
            Value::Operator(Token::Add)
            | Value::Operator(Token::Inc)
            | Value::Operator(Token::Dec)
            | Value::Operator(Token::Mult)
            | Value::Operator(Token::Sub),
          ),
        fixity: Fixity::Prefix,
      } => true,
      Operator {
        expr:
          Expression::Value(
            /* Value::Operator(Token::RParenthesis) | */ Value::Operator(Token::Bang),
          ),
        fixity: Fixity::Postfix,
      } => true,
      Operator {
        expr:
          Expression::Value(
            Value::Operator(Token::Period)
            | Value::Operator(Token::Equal)
            | Value::Operator(Token::EqualEqual)
            | Value::Operator(Token::Add)
            | Value::Operator(Token::Sub)
            | Value::Operator(Token::Mod)
            | Value::Operator(Token::Mult)
            | Value::Operator(Token::Div),
          ),
        fixity: Fixity::Infix,
      } => true,
      Operator {
        expr: Expression::Value(Value::Identifier(id)),
        fixity: Fixity::Infix,
      } => matches!(id.as_str(), "mod" | "and" | "or"),
      _ => false,
    }
  }

  fn bp(&self) -> Option<(u8, u8)> {
    Some(match self {
      Operator {
        expr:
          Expression::Value(
            Value::Identifier(_)
            | Value::String(_)
            | Value::Char(_)
            | Value::Number(_)
            | Value::Boolean(_),
          ),
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        expr: Expression::Value(value),
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
        expr: Expression::Value(value),
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
        expr: Expression::Value(value),
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
          Value::Identifier(id) if id == "mod" => (22, 21),
          Value::Identifier(id) if id == "and" => (24, 23),
          Value::Identifier(id) if id == "or" => (25, 26),
          _ => return None,
        }
      },
      _ => return None,
    })
  }
}

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
