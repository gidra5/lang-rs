use std::{
  cell::RefCell,
  cmp::Ordering,
  fmt::{Display, Formatter},
  rc::Rc,
};

use either::Either;
use itertools::Itertools;

use crate::{
  check_token,
  check_token_end,
  common::{
    char_stream::{value::Value, Token, TokenExt, TokenStream},
    logger::char_stream::LoggerTrait,
    reversable_iterator::ReversableIterator,
  },
  enviroment::Enviroment,
  map,
  match_token,
  punct_or_newline,
  skip,
  token::{self, char_stream::value},
};

use super::{stmt::Statement, Evaluatable, Parseable};

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
pub enum Op {
  Value(Value),
  Record(Vec<RecordItem>),
  Block(Vec<Statement>),
  /* If(Box<Expression>, Box<Expression>, Box<Expression>),
   * For(Box<Expression>, Box<Expression>, Box<Expression>), */
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
  left:   Option<Box<Expression>>,
  pub op: Op,
  right:  Option<Box<Expression>>,
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
    Expression {
      left:  None,
      op:    Op::Value(Value::None),
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
    let Self { left, op, right } = self;
    match op {
      Op::Value(op) => {
        match (left, right) {
          (None, None) => write!(f, "{}", op),
          (Some(left), None) => write!(f, "({} {})", op, left),
          (None, Some(right)) => write!(f, "({} {})", op, right),
          (Some(left), Some(right)) => write!(f, "({} {} {})", op, left, right),
        }
      },
      Op::Record(record_items) => {
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
      Op::Block(stmts) => {
        // write!(f, "block")
        write!(
          f,
          "{{\n{}\n}}",
          stmts.iter().map(|stmt| format!("\t{:?}", stmt)).join(";\n")
        )
      },
    }
  }
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

pub fn match_value<L: LoggerTrait>(
  bind: bool,
  val: Value,
  pat: Op,
  env: &mut Rc<RefCell<Enviroment>>,
  logger: &mut L,
) -> bool {
  if let Op::Value(Value::Placeholder) = pat {
    return true;
  } else if let Op::Value(Value::Identifier(ident)) = pat {
    if bind {
      (*env).borrow_mut().define(ident, val);
    }
    return true;
  } else if let Op::Value(value) = pat {
    return val == value;
  } else if let Op::Record(pat_rec) = pat {
    if let Value::Record(val_rec) = val {
      return pat_rec.len() == val_rec.len()
        && pat_rec.into_iter().zip(val_rec).all(
          |(
            RecordItem {
              key: pat_key,
              value: Expression { op, .. },
            },
            value::RecordItem {
              key: val_key,
              value: val,
            },
          )| {
            (match (op.clone(), pat_key, val_key) {
              (_, RecordKey::None, None) => true,
              (Op::Value(Value::Identifier(x)), RecordKey::None, Some(Value::Identifier(y))) => {
                x == y
              },
              (_, RecordKey::Identifier(x), Some(Value::Identifier(y))) => *x == y,
              (_, RecordKey::Value(x), Some(y)) => x.evaluate(env, logger) == y,
              _ => false,
            }) && match_value(bind, val, op, env, logger)
          },
        );
    } else if let Value::Unit = val {
      return pat_rec.len() == 0;
    }
  }
  false
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(self, env: &mut Rc<RefCell<Enviroment>>, logger: &mut L) -> Value {
    match self {
      Expression {
        left: None,
        op: Op::Value(Value::Identifier(id)),
        right: None,
      } => env.borrow().get(id).unwrap_or_default(),
      Expression {
        left: None,
        op: Op::Value(op),
        right: None,
      } => op,
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: None,
      } => op.postfix((*left).evaluate(env, logger)),
      Expression {
        left: None,
        op: Op::Value(op),
        right: Some(right),
      } => op.prefix((*right).evaluate(env, logger)),
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Arrow)),
        right: Some(right),
      } => {
        let x = Value::Function(left, env.clone(), right);
        (*env).borrow_mut().define("self".to_string(), x.clone());
        x
      },
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Is)),
        right: Some(right),
      } => {
        let left = (*left).evaluate(env, logger);

        Value::Boolean(match_value(false, left, (*right).op, env, logger))
      },
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Apply)),
        right: Some(right),
      } => {
        let left = (*left).evaluate(env, logger);

        if let Value::Function(pat, fn_env, expr) = left {
          let mut new_env = Rc::new(RefCell::new(Enviroment::new(Some(fn_env.clone()))));

          match_value(
            true,
            (*right).evaluate(env, logger),
            (*pat).op,
            &mut new_env,
            logger,
          );

          expr.evaluate(&mut new_env, logger)
        } else {
          Value::None
        }
      },
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Equal)),
        right: Some(right),
      } => {
        let left = *left;
        let right = (*right).evaluate(env, logger);
        if let Expression {
          left: None,
          op: Op::Value(Value::Identifier(left)),
          right: None,
        } = left
        {
          env.borrow_mut().set(left, right.clone());
          right
        } else {
          op.infix(left.evaluate(env, logger), right)
        }
      },
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Period)),
        right:
          Some(box Expression {
            left: None,
            op: Op::Value(id @ Value::Identifier(_)),
            right: None,
          }),
      } => op.infix((*left).evaluate(env, logger), id),
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: Some(right),
      } => {
        op.infix(
          (*left).evaluate(env, logger),
          (*right).evaluate(env, logger),
        )
      },
      Expression {
        left: _,
        op: Op::Block(statements),
        right: _,
      } => {
        let mut new_env = Rc::new(RefCell::new(Enviroment::new(Some(env.clone()))));
        let mut iter = statements.into_iter().peekable();

        loop {
          if let Some(stmt) = iter.next() {
            if let Some(_) = iter.peek() {
              stmt.evaluate(&mut new_env, logger);
            } else {
              break stmt.evaluate(&mut new_env, logger);
            }
          }
        }
      },
      Expression {
        left: _,
        op: Op::Record(op),
        right: _,
      } => {
        let mut x = op
          .into_iter()
          .map(|RecordItem { key, value }| {
            value::RecordItem {
              key:   match key {
                RecordKey::Identifier(name) => Some(Value::Identifier(name)),
                RecordKey::Value(expr) => Some(expr.evaluate(env, logger)),
                RecordKey::None => None,
              },
              value: value.evaluate(env, logger),
            }
          })
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].key != None) {
          Value::Record(x)
        } else if x.len() == 1 {
          x.pop().unwrap().value
        } else {
          Value::Unit
        }
      },
    }
  }
}

fn parse_braces(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), Token::LBrace) {
    Err("Expected opening brace".to_string())
  } else {
    token_stream.next();

    let expr = Expression::parse(token_stream)?;

    if !check_token!(token_stream.next(), Token::RBrace) {
      Err("Expected closing brace".to_string())
    } else {
      Ok(expr)
    }
  }
}

fn parse_block(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), Token::LBracket) {
    Err("Expected opening bracket".to_string())
  } else {
    token_stream.next();

    let x = {
      let mut res = vec![];

      loop {
        if !check_token!(token_stream.peek(), Token::RBracket) && !check_token_end!(token_stream) {
          match Statement::parse(token_stream) {
            Ok(Statement::Expression(expr)) if expr == Expression::default() => (),
            Ok(stmt) => res.push(stmt),
            Err(msg) => return Err(msg),
          };
        } else {
          break res;
        }
      }
    };

    if !check_token!(token_stream.next(), Token::RBracket) {
      Err("Expected closing bracket".to_string())
    } else {
      Ok(Expression {
        left:  None,
        right: None,
        op:    Op::Block(x),
      })
    }
  }
}

fn parse_parens(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), Token::LParenthesis) {
    Err("Expected opening parenthesis".to_string())
  } else {
    token_stream.next();
    let mut x = vec![];

    if check_token!(token_stream.peek(), Token::RParenthesis) {
      token_stream.next();

      return Ok(Expression {
        left:  None,
        right: None,
        op:    Op::Record(x),
      });
    }

    while !check_token!(token_stream.peek(), Token::RParenthesis) {
      if check_token!(token_stream.peek(), Token::NewLine) {
        token_stream.next();
        continue;
      }
      let y = token_stream.peek_ext(2);
      let mut key = RecordKey::None;


      if check_token!(y[0], Token::Identifier) && check_token!(y[1], Token::Colon) {
        key = RecordKey::Identifier(token_stream.next().unwrap().src);
        token_stream.next();
      } else if check_token!(y[0], Token::LBrace) {
        token_stream.next();
        key = RecordKey::Value(parse_expr(token_stream, true)?);
        let y = token_stream.peek_ext(2);

        if !(check_token!(y[0], Token::RBrace) && check_token!(y[1], Token::Colon)) {
          return Err("Expected closing brace and colon".to_string());
        } else {
          token_stream.next_ext(2);
        }
      }

      let expr = parse_expr(token_stream, true)?;

      skip!(token_stream, Token::Comma);

      if check_token_end!(token_stream) {
        return Err("Unexpected end of input".to_string());
      }

      x.push(RecordItem { key, value: expr });
    }
    token_stream.next();

    Ok(Expression {
      left:  None,
      right: None,
      op:    Op::Record(x),
    })
  }
}

fn parse_expr(token_stream: &mut TokenStream<'_>, in_parens: bool) -> Result<Expression, String> {
  let mut top = Frame {
    lhs:      None,
    operator: None,
  };
  let mut stack = Vec::new();

  loop {
    if check_token!(token_stream.peek(), Token::NewLine) && in_parens {
      token_stream.next();
      continue;
    }

    let mut token = token_stream.peek();

    match token {
      // match_token!(Token::For) if !in_parens => {
      //   return Err("Unexpected closing parenthesis".to_string());
      // },
      // match_token!(Token::If) if !in_parens => {
      //   return Err("Unexpected closing parenthesis".to_string());
      // },
      match_token!(Token::RParenthesis) if !in_parens => {
        return Err("Unexpected closing parenthesis".to_string());
      },
      match_token!(Token::LParenthesis) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            value:  Value::Operator(Token::Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(Value::Operator(Token::Apply)),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_parens(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_parens(token_stream)?);
        }

        if let Some(
          op @ Operator {
            fixity: Fixity::None,
            ..
          },
        ) = top.operator.clone()
        {
          let res = top;

          top = match stack.pop() {
            Some(it) => it,
            None => return Ok(res.lhs.unwrap_or_default()),
          };

          top.lhs = Some(Expression {
            op:    Op::Value(Value::Operator(Token::Apply)),
            left:  Some(Box::new(Expression {
              op:    Op::Value(op.value),
              right: None,
              left:  None,
            })),
            right: res.lhs.map(Box::new),
          });
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            value:  Value::Operator(Token::Skip),
          }),
        };
        continue;
      },
      match_token!(Token::LBracket) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            value:  Value::Operator(Token::Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(Value::Operator(Token::Apply)),
            left:  Some(Box::new(lhs)),
            right: Some(Box::new(parse_block(token_stream)?)),
          });
        } else {
          top.lhs = Some(parse_block(token_stream)?);
        }

        stack.push(top);
        top = Frame {
          lhs:      None,
          operator: Some(Operator {
            fixity: Fixity::None,
            value:  Value::Operator(Token::Skip),
          }),
        };
        continue;
      },
      match_token!(Token::LBrace) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            value:  Value::Operator(Token::Skip),
          })
        ) {
          top = stack.pop().unwrap();
        }

        top.lhs = Some(Expression {
          op:    Op::Value(token.unwrap().value()),
          right: Some(Box::new(parse_braces(token_stream)?)),
          left:  Some(Box::new(match stack.pop() {
            None => top.lhs.ok_or("Unexpected indexing position")?,
            Some(Frame { lhs, operator }) => {
              let op = Op::Value(top.operator.ok_or("Unexpected indexing position")?.value);
              top.operator = operator;


              Expression {
                op,
                left: lhs.map(Box::new),
                right: top.lhs.map(Box::new),
              }
            },
          })),
        });
        continue;
      },
      _ => (),
    };
    // println!(
    //   "1 ===\n\n{:?}\n\n{:?}\n\n{:?}\n\n{:?}",
    //   stack, top, token, in_parens
    // );

    let operator = loop {
      let mut operator = token
        .clone()
        .map(|token| Operator::new(token, top.lhs.is_none()))
        .flatten();

      match operator {
        Some(op) if top.operator <= Some(op.clone()) => break op,
        x => {
          // println!("2 ===\n\n{:?}\n\n{:?}\n\n{:?}", stack, top, x);
          if matches!(
            x,
            Some(Operator {
              fixity: Fixity::None,
              value:  _,
            })
          ) && matches!(
            token.clone().map(|token| Operator::new(token, false)),
            Some(None)
          ) {
            token_stream.backtrack(1);
            token = token.map(|t| {
              TokenExt {
                token: Token::Apply,
                ..t
              }
            });
          } else {
            let res = top;

            top = match stack.pop() {
              Some(it) => it,
              None => return Ok(res.lhs.unwrap_or_default()),
            };

            if !matches!(
              res.operator,
              Some(Operator {
                fixity: Fixity::None,
                value:  Value::Operator(Token::Skip),
              })
            ) {
              top.lhs = Some(Expression {
                op:    Op::Value(res.operator.unwrap().value),
                left:  top.lhs.map(Box::new),
                right: res.lhs.map(Box::new),
              });
            }
          }
        },
      };
    };
    token_stream.next();

    stack.push(top);
    top = Frame {
      lhs:      None,
      operator: Some(operator),
    };
  }
}

impl<'a> Parseable<'a> for Expression {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> { parse_expr(stream, false) }
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

    if op.exists() {
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

    if op.exists() {
      return Some(op);
    }

    None
  }

  fn exists(&self) -> bool { self.bp().is_some() }

  fn bp(&self) -> Option<(u8, u8)> {
    Some(match self {
      Operator {
        value:
          Value::Identifier(_)
          | Value::String(_)
          | Value::Placeholder
          | Value::Char(_)
          | Value::Number(_)
          | Value::Boolean(_)
          | Value::Operator(Token::String),
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        value,
        fixity: Fixity::Prefix,
      } => {
        (99, match value {
          Value::Operator(token) => {
            match token {
              Token::Add | Token::Sub => 9,
              Token::Inc | Token::Dec => 11,
              Token::Mult => 13,
              _ => return None,
            }
          },
          Value::Identifier(id) if id == "not" => 30,
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
              Token::LBrace => (26, 27),
              Token::Period => (24, 23),
              Token::Equal => (2, 1),
              Token::Mod => (28, 29),
              Token::Add | Token::Sub => (5, 6),
              Token::Mult | Token::Div => (7, 8),
              Token::EqualEqual => (20, 19),
              Token::LAngleBracket => (20, 19),
              Token::RAngleBracket => (20, 19),
              Token::LessEqual => (20, 19),
              Token::GreaterEqual => (20, 19),
              Token::Arrow => (31, 0),
              Token::Apply => (34, 35),
              Token::Is => (32, 33),
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
