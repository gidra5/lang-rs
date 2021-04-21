#![allow(unused)]

use crate::common::*;
use crate::regex::Regex;

#[path = "tests/token.rs"]
mod tests;

pub type Number = String;
pub type Identifier = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Literal {
  Number(Number),
  Boolean(bool),
  Char(char),
  String(String),
}

#[derive(Clone, PartialEq, Debug)]
pub enum BracketSide {
  Left,
  Right,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Operator {
  Add,
  Sub,
  Mult,
  Div,
  Pow,
  Mod,
  Equal
}

#[derive(Clone, PartialEq, Debug)]
pub enum Punct {
  MultilineComment(BracketSide),
  AngleBracket(BracketSide),
  Parenthesis(BracketSide),
  Bracket(BracketSide),
  Brace(BracketSide),
  EndOfLine,
  Semicolon,
  Comment,
  Period,
  Colon,
  Comma,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Keyword {
  Let,
  Return,
  Entry,
  Placeholder,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
  Identifier(Identifier),
  Operator(Operator),
  Keyword(Keyword),
  Literal(Literal),
  Punct(Punct),
}

impl Tokenizable for Token {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(token) = Keyword::tokenize(stream) {
      Some(Self::Keyword(token))
    } else if let Some(token) = Literal::tokenize(stream) {
      Some(Self::Literal(token))
    } else if let Some(token) = Punct::tokenize(stream) {
      Some(Self::Punct(token))
    } else if let Some(token) = Operator::tokenize(stream) {
      Some(Self::Operator(token))
    } else if let Some(token) = stream.check(r"[A-Za-z0-9_]+") {
      Some(Self::Identifier(token))
    } else { None }
  }
}

impl Tokenizable for Operator {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if stream.check_char('+') {
      Some(Self::Add)
    } else if stream.check_char('-') {
      Some(Self::Sub)
    } else if stream.check_char('*') {
      Some(Self::Mult)
    } else if stream.check_char('/') {
      Some(Self::Div)
    } else if stream.check_char('^') {
      Some(Self::Pow)
    } else if stream.check_char('%') {
      Some(Self::Mod)
    } else if stream.check_char('=') {
      Some(Self::Equal)
    } else { None }
  }
}

impl Tokenizable for Keyword {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if stream.check(r"\blet\b") != None {
      Some(Self::Let)
    } else if stream.check(r"\breturn\b") != None {
      Some(Self::Return)
    } else if stream.check(r"\bentry\b") != None {
      Some(Self::Entry)
    } else if stream.check(r"\b_\b") != None {
      Some(Self::Placeholder)
    } else { None }
  }
}

impl Tokenizable for Literal {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(token) = stream.check(r"[1-9][0-9]*(\.[0-9]*)?") {
      Some(Self::Number(token))
    } else if let Some(token) = stream.check(r"true|false") {
      if token == "true" {
        Some(Self::Boolean(true))
      } else {
        Some(Self::Boolean(false))
      }
    } else if let Some(token) = stream.check(r"'.'") {
      Some(Self::Char(token.chars().skip(1).next().unwrap()))
    } else if let Some(token) = stream.check(r#"".*""#) {
      Some(Self::String(token[1..token.len() - 1].to_string()))
    } else { None }
  }
}

impl Tokenizable for Punct {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if stream.check_string("/*") {
      Some(Self::MultilineComment(BracketSide::Left))
    } else if stream.check_string("*/") {
      Some(Self::MultilineComment(BracketSide::Right))
    } else if stream.check_char('<') {
      Some(Self::AngleBracket(BracketSide::Left))
    } else if stream.check_char('>') {
      Some(Self::AngleBracket(BracketSide::Right))
    } else if stream.check_char('(') {
      Some(Self::Parenthesis(BracketSide::Left))
    } else if stream.check_char(')') {
      Some(Self::Parenthesis(BracketSide::Right))
    } else if stream.check_char('{') {
      Some(Self::Bracket(BracketSide::Left))
    } else if stream.check_char('}') {
      Some(Self::Bracket(BracketSide::Right))
    } else if stream.check_char('[') {
      Some(Self::Brace(BracketSide::Left))
    } else if stream.check_char(']') {
      Some(Self::Brace(BracketSide::Right))
    } else if stream.check_char('\n') {
      Some(Self::EndOfLine)
    } else if stream.check_char(';') {
      Some(Self::Semicolon)
    } else if stream.check_string("//") {
      Some(Self::Comment)
    } else if stream.check_char('.') {
      Some(Self::Period)
    } else if stream.check_char(':') {
      Some(Self::Colon)
    } else if stream.check_char(',') {
      Some(Self::Comma)
    } else { None }
  }
}