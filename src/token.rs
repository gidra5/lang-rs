#![allow(unused)]

use crate::common::*;
use crate::regex::Regex;

#[derive(Clone)]
pub enum Literal {
  Float(f64),
  Integer(i64),
  BigInteger(String),
  Boolean(bool),
  Char(char),
  String(String),
}

#[derive(Clone)]
pub enum BracketSide {
  Left,
  Right,
}

#[derive(Clone)]
pub enum Operator {
  Add,
  Sub,
  Mult,
  Div,
  Pow,
  Mod,
}

#[derive(Clone)]
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

#[derive(Clone)]
pub enum Keyword {
  Let,
  Quit,
  Entry,
  Placeholder,
}

#[derive(Clone)]
pub enum Token {
  Identifier(String),
  Operator(Operator),
  Keyword(Keyword),
  Literal(Literal),
  Punct(Punct),
}

impl Tokenizable for Token {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(token) = Keyword::tokenize(stream) {
      Some(Self::Keyword(token))
    } else if let Some(token) = Operator::tokenize(stream) {
      Some(Self::Operator(token))
    } else if let Some(token) = Literal::tokenize(stream) {
      Some(Self::Literal(token))
    } else if let Some(token) = Punct::tokenize(stream) {
      Some(Self::Punct(token))
    } else if let Some(token) = stream.check(Regex::new(r"\b[^\b]+\b").unwrap()) {
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
    } else { None }
  }
}

impl Tokenizable for Keyword {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(_) = stream.check(Regex::new(r"let").unwrap()) {
      Some(Self::Let)
    } else if let Some(_) = stream.check(Regex::new(r"quit").unwrap()) {
      Some(Self::Quit)
    } else if let Some(_) = stream.check(Regex::new(r"entry").unwrap()) {
      Some(Self::Entry)
    } else if let Some(_) = stream.check(Regex::new(r"_").unwrap()) {
      Some(Self::Placeholder)
    } else { None }
  }
}

impl Tokenizable for Literal {
  fn tokenize(stream: &mut ReversableStream<char>) -> Option<Self> {
    if let Some(token) = stream.check(Regex::new(r"[+\-]?[[:digit:]]{1,10}").unwrap()) {
      // todo convert to int
      Some(Self::Integer(token))
    } else if let Some(token) = stream.check(Regex::new(r"[+\-]?[[:digit:]]+").unwrap()) {
      Some(Self::BigInteger(token))
    } else if let Some(token) = stream.check(Regex::new(r"[+\-]?([[:digit:]]+.[[:digit:]]*|[[:digit:]]+f)").unwrap()) {
      // todo convert to float
      Some(Self::Float(token))
    } else if let Some(token) = stream.check(Regex::new(r"true|false").unwrap()) {
      if token == "true" {
        Some(Self::Boolean(true))
      } else {
        Some(Self::Boolean(false))
      }
    } else if let Some(token) = stream.check(Regex::new(r"'.'").unwrap()) {
      Some(Self::Char(token.chars().next().unwrap()))
    } else if let Some(token) = stream.check(Regex::new(r"\".*\"").unwrap()) {
      Some(Self::String(token))
    } else if stream.check_char('%') {
      Some(Self::Mod)
    } else { None }
  }
}

impl Tokenizable for Punct {
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
    } else { None }
  }
}