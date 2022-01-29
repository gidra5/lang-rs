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
  scoped,
  skip,
  token::{
    self,
    char_stream::{value, Span},
  },
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
  Value(TokenExt),
  Record(Vec<RecordItem>),
  Block(Vec<Statement>),
  /* If(Box<Expression>, Box<Expression>, Box<Expression>),
   * For(Box<Expression>, Box<Expression>, Box<Expression>), */
}

impl Default for Op {
  fn default() -> Op { Op::Value(TokenExt::default()) }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Expression {
  pub left:  Option<Box<Expression>>,
  pub op:    Op,
  pub right: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Default, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,

  #[default]
  None,
}

#[derive(PartialEq, Default, Debug, Clone)]
pub struct Operator {
  fixity: Fixity,
  token:  TokenExt,
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let Self { left, op, right } = self;
    match op {
      Op::Value(op) => {
        match (left, right) {
          (None, None) => write!(f, "{}", op.value()),
          (Some(left), None) => write!(f, "({} {})", op.value(), left),
          (None, Some(right)) => write!(f, "({} {})", op.value(), right),
          (Some(left), Some(right)) => write!(f, "({} {} {})", op.value(), left, right),
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
  bind: usize,
  val: Value,
  pat: Expression,
  env: &mut Enviroment,
  logger: &mut L,
) -> bool {
  match pat {
    Expression {
      left: Some(pat),
      op:
        Op::Value(TokenExt {
          token: Token::Equal,
          src: _,
          span: _,
        }),
      right: Some(right),
    } => {
      let res = match_value(bind, val, *pat.clone(), env, logger);
      if bind == 1 && !res {
        match_value(bind, (*right).evaluate(env, logger), *pat, env, logger)
      } else {
        res
      }
    },
    Expression {
      left: None,
      op: pat,
      right: None,
    } => {
      if let Op::Value(TokenExt {
        token: Token::Placeholder,
        src: ident,
        span: _,
      }) = pat
      {
        return true;
      } else if let Op::Value(TokenExt {
        token: Token::Identifier,
        src: ident,
        span: _,
      }) = pat
      {
        if bind == 1 {
          env.define(ident, val);
        } else if bind == 2 {
          env.set(ident, val);
        }
        return true;
      } else if let Op::Value(token) = pat {
        return val == token.value();
      } else if let Op::Value(TokenExt {
        token: Token::Equal,
        src: _,
        span: _,
      }) = pat
      {
      } else if let Op::Record(pat_rec) = pat {
        if let Value::Record(val_rec) = val {
          return pat_rec.len() == val_rec.len()
            && pat_rec.into_iter().zip(val_rec).all(
              |(
                RecordItem {
                  key: pat_key,
                  value: pat,
                },
                value::RecordItem {
                  key: val_key,
                  value: val,
                },
              )| {
                (match (pat.op.clone(), pat_key, val_key) {
                  (_, RecordKey::None, None) => true,
                  (
                    Op::Value(TokenExt {
                      token: Token::Identifier,
                      src: x,
                      span: _,
                    }),
                    RecordKey::None,
                    Some(Value::Identifier(y)),
                  ) => x == y,
                  (_, RecordKey::Identifier(x), Some(Value::Identifier(y))) => *x == y,
                  (_, RecordKey::Value(x), Some(y)) => x.evaluate(env, logger) == y,
                  _ => false,
                }) && match_value(bind, val, pat, env, logger)
              },
            );
        } else if let Value::Unit = val {
          return pat_rec.len() == 0;
        }
      }
      false
    },
    _ => false,
  }
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(self, env: &mut Enviroment, logger: &mut L) -> Value {
    match self {
      Expression {
        left: None,
        op:
          Op::Value(TokenExt {
            token: Token::Identifier,
            src: id,
            span: _,
          }),
        right: None,
      } => env.get(&id).unwrap_or_default(),
      Expression {
        left: None,
        op: Op::Value(op),
        right: None,
      } => op.value(),
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: None,
      } => op.value().postfix((*left).evaluate(env, logger)),
      Expression {
        left: None,
        op: Op::Value(op),
        right: Some(right),
      } => op.value().prefix((*right).evaluate(env, logger)),
      Expression {
        left: Some(left),
        op:
          Op::Value(TokenExt {
            token: Token::Arrow,
            src: _,
            span: _,
          }),
        right: Some(right),
      } => Value::Function(left, Box::new(env.clone()), right),
      Expression {
        left: Some(left),
        op:
          Op::Value(TokenExt {
            token: Token::Is,
            src: _,
            span: _,
          }),
        right: Some(right),
      } => {
        let left = (*left).evaluate(env, logger);

        Value::Boolean(match_value(0, left, (*right), env, logger))
      },
      Expression {
        left: Some(left),
        op:
          Op::Value(TokenExt {
            token: Token::Apply,
            src: _,
            span: _,
          }),
        right: Some(right),
      } => {
        let mut left = (*left).evaluate(env, logger);

        if let Value::Function(pat, ref mut fn_env, expr) = left.clone() {
          scoped!(fn_env, {
            fn_env.define("self".to_string(), left);

            match_value(1, (*right).evaluate(env, logger), (*pat), fn_env, logger);

            expr.evaluate(fn_env, logger)
          })
        } else {
          Value::None
        }
      },
      Expression {
        left: Some(pat),
        op:
          Op::Value(TokenExt {
            token: Token::Equal,
            src: _,
            span: _,
          }),
        right: Some(right),
      } => {
        let right = (*right).evaluate(env, logger);
        match_value(2, right.clone(), *pat, env, logger);
        right
      },
      Expression {
        left: Some(left),
        op:
          Op::Value(
            op @ TokenExt {
              token: Token::Period,
              src: _,
              span: _,
            },
          ),
        right:
          Some(box Expression {
            left: None,
            op:
              Op::Value(
                id @ TokenExt {
                  token: Token::Identifier,
                  src: _,
                  span: _,
                },
              ),
            right: None,
          }),
      } => op.value().infix((*left).evaluate(env, logger), id.value()),
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: Some(right),
      } => {
        op.value().infix(
          (*left).evaluate(env, logger),
          (*right).evaluate(env, logger),
        )
      },
      Expression {
        left: _,
        op: Op::Block(statements),
        right: _,
      } => {
        scoped!(env, {
          let mut iter = statements.into_iter().peekable();

          loop {
            if let Some(stmt) = iter.next() {
              if let Some(_) = iter.peek() {
                stmt.evaluate(env, logger);
              } else {
                break stmt.evaluate(env, logger);
              }
            } else {
              break Value::None;
            }
          }
        })
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

fn parse_braces(token_stream: &mut TokenStream) -> Result<Expression, String> {
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

fn parse_block(token_stream: &mut TokenStream) -> Result<Expression, String> {
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

fn parse_parens(token_stream: &mut TokenStream) -> Result<Expression, String> {
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

fn parse_expr(token_stream: &mut TokenStream, in_parens: bool) -> Result<Expression, String> {
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
            token:  TokenExt {
              token: Token::Skip,
              src:   _,
              span:  _,
            },
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
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
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
            left:  Some(Box::new(Expression {
              op:    Op::Value(op.token),
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
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(Token::LBracket) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   _,
              span:  _,
            },
          })
        ) {
          top = stack.pop().unwrap();
        }

        if let Some(lhs) = top.lhs {
          top.lhs = Some(Expression {
            op:    Op::Value(TokenExt {
              token: Token::Apply,
              src:   "".to_string(),
              span:  Span::default(),
            }),
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
            token:  TokenExt {
              token: Token::Skip,
              src:   "".to_string(),
              span:  Span::default(),
            },
          }),
        };
        continue;
      },
      match_token!(Token::LBrace) => {
        if matches!(
          top.operator,
          Some(Operator {
            fixity: Fixity::None,
            token:  TokenExt {
              token: Token::Skip,
              src:   _,
              span:  _,
            },
          })
        ) {
          top = stack.pop().unwrap();
        }

        top.lhs = Some(Expression {
          op:    Op::Value(token.unwrap()),
          right: Some(Box::new(parse_braces(token_stream)?)),
          left:  Some(Box::new(match stack.pop() {
            None => top.lhs.ok_or("Unexpected indexing position")?,
            Some(Frame { lhs, operator }) => {
              let op = Op::Value(top.operator.ok_or("Unexpected indexing position")?.token);
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
              token:  _,
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
                token:  TokenExt {
                  token: Token::Skip,
                  src:   _,
                  span:  _,
                },
              })
            ) {
              top.lhs = Some(Expression {
                op:    Op::Value(res.operator.unwrap().token),
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

impl Parseable for Expression {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> { parse_expr(stream, false) }
}

impl Operator {
  pub fn new(token: TokenExt, prefix: bool) -> Option<Self> {
    let op = Operator {
      token,
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
        token:
          TokenExt {
            token:
              Token::Identifier
              | Token::String
              | Token::Placeholder
              | Token::Char
              | Token::Number
              | Token::Boolean,
            src: _,
            span: _,
          },
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        token: TokenExt {
          token,
          src,
          span: _,
        },
        fixity: Fixity::Prefix,
      } => {
        (99, match token {
          Token::Add | Token::Sub => 9,
          Token::Inc | Token::Dec => 11,
          Token::Mult => 13,
          Token::Identifier if src == "not" => 30,
          _ => return None,
        })
      },
      Operator {
        token: TokenExt {
          token,
          src,
          span: _,
        },
        fixity: Fixity::Postfix,
      } => {
        (
          match token {
            Token::Bang => 15,
            _ => return None,
          },
          100,
        )
      },
      Operator {
        token: TokenExt {
          token,
          src,
          span: _,
        },
        fixity: Fixity::Infix,
      } => {
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

          Token::Identifier if src == "mod" => (22, 21),
          Token::Identifier if src == "and" => (24, 23),
          Token::Identifier if src == "or" => (25, 26),
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
