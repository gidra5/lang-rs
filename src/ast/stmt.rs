use std::{cell::RefCell, rc::Rc, fmt::{Display, Formatter}};

use itertools::Itertools;
use crate::{
  check_token,
  check_token_end,
  common::{reversable_iterator::ReversableIterator, value::RecordItem, LoggerTrait, Value},
  enviroment::Enviroment,
  punct_or_newline,
  scoped, token::{TokenStream, TokenExt, Token}, skip,
};

#[path = "../tests/stmt.rs"]
mod tests;

use super::{
  expr::{
    Expression, Op,
    match_value,
  },
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
/// 
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
  Expression(Expression),
  Let(String, Option<Expression>),
}

impl Display for Statement {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Expression(expr) => write!(f, "{}", expr),
      Self::Let(x, Some(expr)) => write!(f, "let {} = {}", x, expr),
      Self::Let(x, None) => write!(f, "let {}", x),
    }
  }
}

#[macro_export]
macro_rules! parse_stmt_vec {
  ($stream:ident $($(, { $src:ident })?, $pattern:ident $(if $cond:expr)?)?) => {{
    use crate::{
      check_token, 
      check_token_end,
      ast::expr::expr_struct::Expression, 
      common::{
        reversable_iterator::ReversableIterator,
      }, 
      enviroment::Enviroment, 
      punct_or_newline
    };
    
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

impl Parseable for Statement {
  fn parse(stream: &mut TokenStream) -> Result<Self, String> {
    let res = {
      let TokenExt { token, src, span } = stream
        .peek()
        .ok_or_else(|| "Unexpected end of statement.".to_string())?;

      match token {
        // Token::For => {
        //   stream.next();

        //   if !check_token!(stream.peek(), Identifier) {
        //     return Err("Expected identifier".to_string());
        //   }
        //   let var = stream.next().unwrap();

        //   if !check_token!(stream.next(), { src }, In) {
        //     return Err("Expected 'in' after identifier".to_string());
        //   }

        //   let iterator = Expression::parse(stream)?;

        //   if !punct_or_newline!(stream.next(), Colon) {
        //     return Err("Missing colon after expression in for statement".to_string());
        //   }

        //   let body = if check_token!(stream.peek(), LBracket) {
        //     stream.next();
        //     let res = parse_stmt_vec!(stream, RBracket)?;

        //     if check_token!(stream.next(), RBracket) {
        //       res
        //     } else {
        //       return Err("Missing closing bracket".to_string())
        //     }
        //   } else if !punct_or_newline!(stream.peek(), Semicolon) {
        //     vec![Statement::parse(stream)?]
        //   } else {
        //     return Err("Empty body in for statement".to_string());
        //   };

        //   return Ok(Self::For(var.src, iterator, body));
        // },
        // Token::If => {
        //   stream.next();
        //   let expr = Expression::parse(stream)?;

        //   if !punct_or_newline!(stream.next(), Colon) {
        //     return Err("Missing colon after condition in an if statement".to_string());
        //   }

        //   let true_block = if check_token!(stream.peek(), LBracket) {
        //     stream.next();
        //     let res = parse_stmt_vec!(stream, RBracket)?;

        //     if check_token!(stream.next(), RBracket) {
        //       res
        //     } else {
        //       return Err("Missing closing bracket".to_string())
        //     }
        //   } else if !check_token!(stream.peek(), Else) {
        //     vec![Statement::parse(stream)?]
        //   } else {
        //     return Err("Empty true branch in if statement".to_string());
        //   };

        //   let false_block = if check_token!(stream.peek(), Else) {
        //     stream.next();
        //     if check_token!(stream.peek(), LBracket) {
        //       stream.next();
        //       let res = parse_stmt_vec!(stream, RBracket)?;
  
        //       if check_token!(stream.next(), RBracket) {
        //         res
        //       } else {
        //         return Err("Missing closing bracket".to_string())
        //       }
        //     } else if !check_token_end!(stream) {
        //       skip!(stream, NewLine);
        //       vec![Statement::parse(stream)?]
        //     } else {
        //       return Err("Empty false branch in if statement".to_string());
        //     }
        //   } else {
        //     vec![]
        //   };
        //   return Ok(Self::If(expr, true_block, false_block));
        // },
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

          if check_token!(stream.peek(), Equal) {
            stream.next();
            while check_token!(stream.peek(), NewLine) {
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
  fn evaluate<L: LoggerTrait>(&self, env: &mut Enviroment, logger: &mut L) -> Value {
    match self {
      Self::Expression(expr) => {
        return expr.evaluate(env, logger);
      },
      Self::Let(id, expr) => {
        let val = expr.clone()
          .map(|expr| expr.evaluate(env, logger))
          .unwrap_or_default();
        env.define(id.clone(), val)
      },
    };

    Value::None
  }
}
