#![allow(unused)]
use crate::{common::*, token::*};
use std::{
  collections::{HashMap, HashSet},
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
      set.insert($key, $value);
    )*
    map
  }};
}

fn precedence_tokens(index: usize) -> HashSet<Token> {
  use Token::*;
  vec![
    set![LParenthesis, Identifier, Number, String, Char, Boolean],
    set![Identifier],
    set![Bang, Sub],
    set![Dec, Inc],
    set![Pow, Mod],
    set![Div, Mult],
    set![Add, Sub],
    set![Equal, EqualEqual, LessEqual, GreaterEqual],
  ].swap_remove(7 - index)
}

#[derive(Clone, Debug)]
pub struct ASTNodeExt<'a, T> {
  pub node: T,
  pub span: Span<TokenStream<'a>>,
}

// pub struct Program;
#[derive(Debug)]
pub enum Expression {
  BinaryExpression(Box<Expression>, Token, Box<Expression>),
  UnaryPrefixExpression(Token, Box<Expression>),
  UnaryPostfixExpression(Token, Box<Expression>),
  FunctionCallExpression(Box<Expression>, Box<Expression>),
  Literal(Value),
}

impl Expression {
  pub fn parse(
    token_stream: &mut TokenStream<'_>,
    precedence: usize,
  ) -> Result<Expression, &'static str> {
    use Token::*;
    let operators = precedence_tokens(precedence);

    if let Some(token) = token_stream.peek() {
      match precedence {
        7 => if operators.contains(&token.token) {
          match token_stream.next().unwrap().token {
            Token::LParenthesis => {
              let inner = Self::parse(token_stream, 0)?;
  
              if !token_stream.stream.check2(Token::RParenthesis) {
                return Err("Missing closing parenthesis");
              }
  
              Ok(inner)
            }, 
            _ => Ok(Self::Literal(token_stream.prev().unwrap().value()))
          }
        } else {
          Logger::error(&format!(
            "Expected one of {:?}, got {:?}",
            operators, token.token
          ));
          Err("")
        },
        6 => if operators.contains(&token.token) {
          match token_stream.next().unwrap().token {
            Token::Identifier => {
              let mut left = Self::Literal(token_stream.prev().unwrap().value());
      
              while let Ok(expr) = Expression::parse(token_stream, 7) {
                left = Self::FunctionCallExpression(Box::new(left), Box::new(expr));
              }
      
              Ok(left)
            }
            _ => unreachable!()
          }
        } else { Ok(Self::Literal(token_stream.next().unwrap().value())) },
        5 | 4 => {
          if operators.contains(&token.token) {
            Ok(Self::UnaryPrefixExpression(
              token_stream.stream.next().unwrap().token,
              Box::new(Self::parse(token_stream, precedence + 1)?),
            ))
          } else {
            Self::parse(token_stream, precedence + 1)
          }
        },
        0..=3 => {
          let mut left = Self::parse(token_stream, precedence + 1)?;
  
          while token_stream.stream.peek() != None
            && operators.contains(&token_stream.stream.peek().unwrap().token)
          {
            left = Self::BinaryExpression(
              Box::new(left),
              token_stream.stream.next().unwrap().token,
              Box::new(Self::parse(token_stream, precedence + 1)?),
            );
          }
  
          Ok(left)
        }
        _=> Err("Precedance greater than 6")
      } 
    } else {
      Err("Unexpected end of expression")
    }
  }
}