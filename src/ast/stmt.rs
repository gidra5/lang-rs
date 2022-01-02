use std::{cell::RefCell, rc::Rc};

use crate::{
  check_token,
  check_token_end,
  common::{
    char_stream::{value::Value, Token, TokenExt, TokenStream},
    logger::char_stream::{Logger, LoggerTrait},
    reversable_iterator::ReversableIterator,
  },
  enviroment::Enviroment,
  punct_or_newline,
};

use super::{expr::Expression, Evaluatable, Parseable};

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
  Expression(Expression),
  Let(String, Option<Expression>),
  Block(Block),
  If(Expression, Vec<Statement>, Vec<Statement>),
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

impl<'a> Parseable<'a> for Block {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let res = parse_stmt_vec!(stream, Token::RBracket)?;

    if check_token!(stream.next(), Token::RBracket) {
      Ok(Self(res))
    } else {
      Err("Missing closing bracket".to_string())
    }
  }
}

impl<'a> Parseable<'a> for Statement {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let res = {
      let TokenExt { token, src, span } = stream
        .peek()
        .ok_or_else(|| "Unexpected end of statement.".to_string())?;

      match token {
        Token::Identifier if src == "print" => {
          stream.next();
          Self::Print(Expression::parse(stream)?)
        },
        Token::Identifier if src == "if" => {
          stream.next();
          let expr = Expression::parse(stream)?;

          if !punct_or_newline!(stream.next(), Colon) {
            return Err("Missing colon after condition in if statement".to_string());
          }

          let true_block = if check_token!(stream.peek(), Token::LBracket) {
            stream.next();
            Block::parse(stream)?.0
          } else if !check_token!(stream.peek(), {src}, Token::Identifier if src == "else") {
            vec![Statement::parse(stream)?]
          } else {
            return Err("Empty true branch in if statement".to_string());
          };

          let false_block = if check_token!(stream.peek(), {src}, Token::Identifier if src == "else")
          {
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
        Token::LBracket => {
          stream.next();

          return Ok(Self::Block(Block::parse(stream)?));
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
  fn evaluate<L: LoggerTrait>(self, env: &mut Rc<RefCell<Enviroment>>, logger: &mut L) -> Value {
    match self {
      Self::Expression(expr) => {
        return expr.evaluate(env, logger);
      },
      Self::Print(expr) => {
        let x = expr.evaluate(env, logger);
        logger.write(format!("{}", x))
      },
      Self::Let(id, expr) => {
        let val = expr.map_or(Value::None, |expr| expr.evaluate(env, logger));
        env.borrow_mut().define(id, val)
      },
      Self::If(condition, true_branch, false_branch) => {
        if let Value::Boolean(true) = condition.evaluate(env, logger) {
          Statement::Block(Block(true_branch)).evaluate(env, logger)
        } else {
          Statement::Block(Block(false_branch)).evaluate(env, logger)
        };
      },
      Self::Block(Block(statements)) => {
        let mut new_env = Enviroment::new();
        new_env.set_enclosing(env.clone());
        let mut new_env = Rc::new(RefCell::new(new_env));

        for stmt in statements {
          stmt.evaluate(&mut new_env, logger);
        }
      },
    };

    Value::None
  }
}