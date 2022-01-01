#[cfg(test)]
use crate::common::char_stream::{CharStream, Logger, TokenExt, TokenStream};

use super::Token;

fn tokens(input: &str) -> Result<Vec<(Token, String)>, String> {
  use crate::common::char_stream::{CharStream, Logger, TokenExt, TokenStream};

  use super::Token;

  let mut stream = CharStream::from_str(input);
  let mut logger = Logger { logs: vec![] };

  Ok(
    TokenStream::new(CharStream::from_str(input), &mut logger)
      .ok_or("Failed to create TokenStream")?
      .stream
      .data()
      .iter()
      .cloned()
      .map(|TokenExt { token, src, .. }| (token, src))
      .collect(),
  )
}

macro_rules! check {
  ($s:ident, [$(($token:ident, $str:literal)),*]) => {
    assert!($s.iter().eq(vec![$((Token::$token, $str.to_string())),*].iter()))
  };
}

#[test]
fn token_ident_1() {
  let s = tokens("a").unwrap();
  check!(s, [(Identifier, "a")])
}

#[test]
fn token_ident_2() {
  let s = tokens("abd").unwrap();
  check!(s, [(Identifier, "abd")])
}

#[test]
fn token_ident_3() {
  let s = tokens("abd123").unwrap();
  check!(s, [(Identifier, "abd123")])
}

#[test]
fn token_integer() {
  let s = tokens("123").unwrap();
  check!(s, [(Number, "123")])
}

#[test]
fn token_float() {
  let s = tokens("123.").unwrap();
  check!(s, [(Number, "123.")])
}

#[test]
fn token_float_2() {
  let s = tokens("123.123").unwrap();
  check!(s, [(Number, "123.123")])
}

#[test]
fn token_float_3() {
  let s = tokens("123.0").unwrap();
  check!(s, [(Number, "123.0")])
}

#[test]
fn token_float_6() {
  let s = tokens("123..").unwrap();
  check!(s, [(Number, "123."), (Period, ".")])
}

#[test]
fn token_period_number_period() {
  let s = tokens(".123.456.").unwrap();
  check!(s, [(Period, "."), (Number, "123.456"), (Period, ".")])
}

#[test]
fn token_float_5() {
  let s = tokens("0.123").unwrap();
  check!(s, [(Number, "0.123")])
}

#[test]
fn token_boolean_true() {
  let s = tokens("true").unwrap();
  check!(s, [(Boolean, "true")])
}

#[test]
fn token_boolean_false() {
  let s = tokens("false").unwrap();
  check!(s, [(Boolean, "false")])
}

#[test]
fn token_string() {
  let s = tokens("\"true\"").unwrap();
  check!(s, [(String, "\"true\"")])
}

#[test]
fn token_char() {
  let s = tokens("'t'").unwrap();
  check!(s, [(Char, "'t'")])
}
