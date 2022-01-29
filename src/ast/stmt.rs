use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{
    char_stream::{value::Value, Token, TokenExt, TokenStream},
    logger::char_stream::{value::RecordItem, Logger, LoggerTrait},
    reversable_iterator::ReversableIterator,
  },
  enviroment::Enviroment,
  punct_or_newline,
  scoped,
};

use super::{
  expr::{match_value, Expression, Op},
  Evaluatable,
  Parseable,
};

/// Statement
///
/// Every statement has either a semicolon or newline at the end
///
/// Possible variants are:
///
/// Print: Prints value of evaluated expression, probably will be removed
///
/// Syntax:
/// "print" expr
///
/// Expression: evaluates expression, probably everything can be treated as this
/// statement
///
/// Syntax:
/// expr
///
/// Let: Creates variable in current scope
///
/// Syntax:
/// "let" ident ("=" expr)?
///
/// Block: Groups staements into one
///
/// Syntax:
/// "{" (stmt)* "}"
///
/// If: Conditionally executes statements
///
/// Syntax:
/// "if" expr(: | "\n") stmt "else" stmt
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
  Print(Expression),
  Parse(Expression),
  Expression(Expression),
  Let(String, Option<Expression>),
  If(Expression, Vec<Statement>, Vec<Statement>),
  For(String, Expression, Vec<Statement>),
}

#[macro_export]
macro_rules! parse_stmt_vec {
  ($stream:ident $($(, { $src:ident })?, $pattern:pat $(if $cond:expr)?)?) => {{
    use crate::{check_token, check_token_end,
      ast::expr::Expression, common::{
        char_stream::{Token, TokenExt, TokenStream},
        reversable_iterator::ReversableIterator,
      }, enviroment::Enviroment, punct_or_newline};
    let mut res = vec![];
    let mut err = None;

    while $(!check_token!($stream.peek() $(, { $src })?, $pattern $(if $cond)?) &&)? !check_token_end!($stream) {
      match Statement::parse($stream) {
        Ok(Statement::Expression(expr)) if expr == Expression::default() => (),
        Ok(stmt) => res.push(stmt),
        Err(msg) => {
          err = Some(msg);
          break;
        },
      }
    }

    if let Some(msg) = err {
      Err(msg)
    } else {
      Ok(res)
    }
  }};
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<Statement>);

impl Parseable for Block {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> {
    let res = parse_stmt_vec!(stream, Token::RBracket)?;

    if check_token!(stream.next(), Token::RBracket) {
      Ok(Self(res))
    } else {
      Err("Missing closing bracket".to_string())
    }
  }
}

impl Parseable for Statement {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> {
    let res = {
      let TokenExt { token, src, span } = stream
        .peek()
        .ok_or_else(|| "Unexpected end of statement.".to_string())?;

      match token {
        Token::Identifier if src == "print" => {
          stream.next();
          Self::Print(Expression::parse(stream)?)
        },
        Token::Identifier if src == "parse" => {
          stream.next();
          Self::Parse(Expression::parse(stream)?)
        },
        Token::For => {
          stream.next();

          if !check_token!(stream.peek(), Token::Identifier) {
            return Err("Expected identifier".to_string());
          }
          let var = stream.next().unwrap();

          if !check_token!(stream.next(), { src }, Token::In) {
            return Err("Expected 'in' after identifier".to_string());
          }

          let iterator = Expression::parse(stream)?;

          if !punct_or_newline!(stream.next(), Colon) {
            return Err("Missing colon after expression in for statement".to_string());
          }

          let body = if check_token!(stream.peek(), Token::LBracket) {
            stream.next();
            Block::parse(stream)?.0
          } else if !punct_or_newline!(stream.peek(), Semicolon) {
            vec![Statement::parse(stream)?]
          } else {
            return Err("Empty body in for statement".to_string());
          };

          return Ok(Self::For(var.src, iterator, body));
        },
        Token::If => {
          stream.next();
          let expr = Expression::parse(stream)?;

          if !punct_or_newline!(stream.next(), Colon) {
            return Err("Missing colon after condition in if statement".to_string());
          }

          let true_block = if check_token!(stream.peek(), Token::LBracket) {
            stream.next();
            Block::parse(stream)?.0
          } else if !check_token!(stream.peek(), Token::Else) {
            vec![Statement::parse(stream)?]
          } else {
            return Err("Empty true branch in if statement".to_string());
          };

          let false_block = if check_token!(stream.peek(), Token::Else) {
            stream.next();
            if check_token!(stream.peek(), Token::LBracket) {
              stream.next();
              Block::parse(stream)?.0
            } else if !check_token_end!(stream) {
              vec![Statement::parse(stream)?]
            } else {
              return Err("Empty false branch in if statement".to_string());
            }
          } else {
            vec![]
          };
          return Ok(Self::If(expr, true_block, false_block));
        },
        Token::Identifier if src == "let" => {
          stream.next();

          let id = stream
            .next()
            .map(|token| {
              match token.token {
                Token::Identifier => Some(token.src),
                _ => None,
              }
            })
            .flatten()
            .ok_or_else(|| "Expected identifier at let statement.".to_string())?;

          if check_token!(stream.peek(), Token::Equal) {
            stream.next();
            while check_token!(stream.peek(), Token::NewLine) {
              stream.next();
            }
            Self::Let(
              id,
              Some(match Expression::parse(stream) {
                Ok(expr) if expr == Expression::default() => {
                  return Err(
                    "Error at expression after '=' in let statement: No expression".to_string(),
                  )
                },
                Ok(expr) => expr,
                Err(msg) => {
                  return Err(format!(
                    "Error at expression after '=' in let statement: {}",
                    msg
                  ))
                },
              }),
            )
          } else {
            Self::Let(id, None)
          }
        },
        _ => Self::Expression(Expression::parse(stream)?),
      }
    };

    if punct_or_newline!(stream.next(), Semicolon) {
      Ok(res)
    } else {
      Err("Missing semicolon at the end of statement".to_string())
    }
  }
}

impl Evaluatable for Statement {
  fn evaluate<L: LoggerTrait>(self, env: &mut Enviroment, logger: &mut L) -> Value {
    match self {
      Self::Expression(expr) => {
        return expr.evaluate(env, logger);
      },
      Self::Print(expr) => {
        let x = expr.evaluate(env, logger);
        logger.write(format!("{}", x))
        // logger.write(format!("{:?}", x))
      },
      Self::Parse(expr) => logger.write(format!("{}", expr)),
      Self::Let(id, expr) => {
        let val = expr
          .map(|expr| expr.evaluate(env, logger))
          .unwrap_or_default();
        env.define(id, val)
      },
      Self::If(condition, true_branch, false_branch) => {
        return if let Value::Boolean(true) = condition.evaluate(env, logger) {
          Expression {
            left:  None,
            right: None,
            op:    Op::Block(true_branch),
          }
          .evaluate(env, logger)
          // Statement::Block(Block(true_branch)).evaluate(env, logger)
        } else {
          Expression {
            left:  None,
            right: None,
            op:    Op::Block(false_branch),
          }
          .evaluate(env, logger)
          // Statement::Block(Block(false_branch)).evaluate(env, logger)
        };
      },
      Self::For(var, iterator, body) => {
        let mut iterator = iterator.evaluate(env, logger);
        let mut accumulator = Value::Unit;

        return loop {
          let mut stmts = body.iter().cloned().peekable();

          if let Value::Function(pat, ref mut fn_env, expr) = iterator {
            let next = scoped!(fn_env, {
              match_value(1, accumulator, (*pat), fn_env, logger);

              expr.evaluate(fn_env, logger)
            });

            if let Value::Record(vec) = next {
              let RecordItem { value: val, .. } = &vec[0];
              let RecordItem { value: iter, .. } = &vec[1];

              scoped!(env, {
                env.define(var.clone(), val.clone());

                iterator = iter.clone();

                accumulator = loop {
                  if let Some(stmt) = stmts.next() {
                    if let Some(_) = stmts.peek() {
                      stmt.evaluate(env, logger);
                    } else {
                      break stmt.evaluate(env, logger);
                    }
                  }
                }
              })
            } else {
              env.define(var.clone(), next);
              break scoped!(env, {
                loop {
                  if let Some(stmt) = stmts.next() {
                    if let Some(_) = stmts.peek() {
                      stmt.evaluate(env, logger);
                    } else {
                      break stmt.evaluate(env, logger);
                    }
                  }
                }
              });
            }
          } else {
            env.define(var.clone(), iterator);
            break scoped!(env, {
              loop {
                if let Some(stmt) = stmts.next() {
                  if let Some(_) = stmts.peek() {
                    stmt.evaluate(env, logger);
                  } else {
                    break stmt.evaluate(env, logger);
                  }
                }
              }
            });
          }
        };
      },
    };

    Value::None
  }
}
