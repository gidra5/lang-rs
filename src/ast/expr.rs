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
    op:    Value,
    right: Option<Box<Expression>>,
  },
  Tuple(Vec<Expression>),
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

impl Default for Expression {
  fn default() -> Expression {
    Expression::Binary {
      left:  None,
      op:    Value::None,
      right: None,
    }
  }
}

impl Default for Operator {
  fn default() -> Operator {
    Operator {
      value:  Value::None,
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
      Expression::Binary {
        left: None,
        op: Value::Identifier(id),
        right: None,
      } => env.borrow().get(id).unwrap_or_default(),
      Expression::Binary {
        left: None,
        op,
        right: None,
      } => op,
      Expression::Binary {
        left: Some(left),
        op,
        right: None,
      } => op.postfix((*left).evaluate(env, logger)),
      Expression::Binary {
        left: None,
        op,
        right: Some(right),
      } => op.prefix((*right).evaluate(env, logger)),
      Expression::Binary {
        left: Some(left),
        op: op @ Value::Operator(Token::Equal),
        right: Some(right),
      } => {
        let left = *left;
        let right = (*right).evaluate(env, logger);
        if let Expression::Binary {
          left: None,
          op: Value::Identifier(left),
          right: None,
        } = left
        {
          env.borrow_mut().set(left, right.clone());
          println!("{:?}", env);
          right
        } else {
          op.infix(left.evaluate(env, logger), right)
        }
      },
      Expression::Binary {
        left: Some(left),
        op,
        right: Some(right),
      } => {
        op.infix(
          (*left).evaluate(env, logger),
          (*right).evaluate(env, logger),
        )
      },
      Expression::Tuple(exprs) => {
        Value::Tuple(
          exprs
            .into_iter()
            .map(|expr| expr.evaluate(env, logger))
            .collect_vec(),
        )
      },
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
            // if let Some(Expression::Binary {
            //   op: Value::Operator(Token::LParenthesis),
            //   left: None,
            //   right: Some(_),
            // }) = res.lhs
            // {
            //   return Err("Expected closing parenthesis".to_string());
            // }

            top = match stack.pop() {
              Some(it) => it,
              None => return Ok(res.lhs.unwrap_or_default()),
            };

            top.lhs = Some(Expression::Binary {
              op:    res.operator.unwrap().value,
              left:  top.lhs.map(Box::new),
              right: res.lhs.map(Box::new),
            });
          },
        };
      };
      token_stream.next();

      if let Operator {
        value: Value::Operator(Token::LParenthesis),
        fixity: Fixity::Prefix,
      } = operator
      {
        let mut exprs = vec![];

        if let Some(TokenExt { token, .. }) = token_stream.peek() {
          match token {
            // Token::Comma => {
            //   token_stream.next();
            //   if let Some(
            //     token @ TokenExt {
            //       token: Token::RParenthesis,
            //       ..
            //     },
            //   ) = token_stream.peek()
            //   {
            //     operator = Operator::new(token, false).unwrap();
            //   } else {
            //     exprs.push(Expression::parse(token_stream)?);
            //   }
            // },
            Token::RParenthesis => {
              token_stream.next();
            },
            _ => {
              exprs.push(Expression::parse(token_stream)?);
            },
          }
        }
        while let Some(TokenExt { token, .. }) = token_stream.peek() {
          match token {
            Token::Comma => {
              token_stream.next();
              if let Some(
                token
                @
                TokenExt {
                  token: Token::RParenthesis,
                  ..
                },
              ) = token_stream.peek()
              {
                operator = Operator::new(token, false).unwrap();
              } else {
                exprs.push(Expression::parse(token_stream)?);
              }
            },
            Token::RParenthesis => {
              token_stream.next();
            },
            _ => return Err(format!("Unexpected token {:?}", token)),
          }
        }
      }

      // if let Operator {
      //   value: Value::Operator(Token::RParenthesis),
      //   fixity: Fixity::Postfix,
      // } = operator
      // {
      //   if let Some(Operator {
      //     value: Value::Operator(Token::LParenthesis),
      //     fixity: Fixity::Prefix,
      //   }) = top.operator
      //   {
      //     let res = top;
      //     top = stack.pop().unwrap();
      //     top.lhs = res.lhs;
      //     continue;
      //   } else {
      //     return Err("Unexpected closing parenthesis".to_string());
      //   }
      // }

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
      Operator {
        value: Value::Identifier(id),
        fixity: Fixity::Infix,
      } => matches!(id.as_str(), "mod" | "and" | "or"),
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
