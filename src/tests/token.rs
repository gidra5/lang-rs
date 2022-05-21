use super::{Token, Token::*};
use crate::{
  common::{group_map_iterator::GroupMapTrait, Buf},
  parse_tokens,
};

fn tokens(input: &str) -> Result<Vec<Token>, std::string::String> {
  let mut errors_t = vec![];
  Ok(
    input
      .chars()
      .buffered()
      .group_map(|iter| parse_tokens!(errors_t, iter))
      .filter(|token| token != &Token::Skip)
      .collect(),
  )
}

macro_rules! check {
  ($s:ident, [$($token:expr),*]) => {
    assert!($s.iter().eq(vec![$($token),*].iter()))   };
}

#[test]
fn token_ident_1() {
  let s = tokens("a").unwrap();
  check!(s, [Identifier("a".to_string())])
}

#[test]
fn token_ident_2() {
  let s = tokens("abd").unwrap();
  check!(s, [Identifier("abd".to_string())])
}

#[test]
fn token_ident_3() {
  let s = tokens("abd123").unwrap();
  check!(s, [Identifier("abd123".to_string())])
}

#[test]
fn token_integer() {
  let s = tokens("123").unwrap();
  check!(s, [Number(123, 0)])
}

#[test]
fn token_float() {
  let s = tokens("123.").unwrap();
  check!(s, [Number(123, 0)])
}

#[test]
fn token_float_2() {
  let s = tokens("123.123").unwrap();
  check!(s, [Number(123, 123)])
}

#[test]
fn token_float_3() {
  let s = tokens("123.0").unwrap();
  check!(s, [Number(123, 0)])
}

#[test]
fn token_float_6() {
  let s = tokens("123..").unwrap();
  check!(s, [Number(123, 0), Period])
}

#[test]
fn token_period_number_period() {
  let s = tokens(".123.456.").unwrap();
  check!(s, [Period, Number(123, 456), Period])
}

#[test]
fn token_float_5() {
  let s = tokens("0.123").unwrap();
  check!(s, [Number(0, 123)])
}

#[test]
fn token_boolean_true() {
  let s = tokens("true").unwrap();
  check!(s, [Boolean(true)])
}

#[test]
fn token_boolean_false() {
  let s = tokens("false").unwrap();
  check!(s, [Boolean(false)])
}

#[test]
fn token_string() {
  let s = tokens("\"true\"").unwrap();
  check!(s, [String("true".to_string())])
}

#[test]
fn token_char() {
  let s = tokens("'t'").unwrap();
  check!(s, [Char('t')])
}

#[test]
fn token_skip_spaces() {
  let s = tokens("x   y").unwrap();
  check!(s, [
    Identifier("x".to_string()),
    Identifier("y".to_string())
  ])
}

#[test]
fn token_new_line() {
  let s = tokens("x \n y").unwrap();
  check!(s, [
    Identifier("x".to_string()),
    NewLine,
    Identifier("y".to_string())
  ])
}

#[test]
fn token_multiple_newline_counts_as_one() {
  let s = tokens("x\n\n\ny").unwrap();
  check!(s, [
    Identifier("x".to_string()),
    NewLine,
    Identifier("y".to_string())
  ])
}

#[test]
fn token_whitespace_chars_after_new_line() {
  let s = tokens("x\n \n \ny").unwrap();
  check!(s, [
    Identifier("x".to_string()),
    NewLine,
    Identifier("y".to_string())
  ])
}

#[test]
fn token_whitespace_chars_after_new_line_2() {
  let s = tokens("x\n\t\n \ny").unwrap();
  check!(s, [
    Identifier("x".to_string()),
    NewLine,
    Identifier("y".to_string())
  ])
}
