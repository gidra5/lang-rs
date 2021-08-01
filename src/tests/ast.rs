use crate::common::logger::char_stream::{CharStream, TokenStream};

use super::{Expression, Parseable};


fn expr(input: &str) -> Result<Expression, String> {
  let mut stream =
    TokenStream::new(CharStream::from_str(input)).ok_or("Failed to create TokenStream")?;

  Expression::parse(&mut stream)
}

#[test]
fn expr_tests() {
  let s = expr("1").unwrap();
  assert_eq!(s.to_string(), "1");

  let s = expr("1 + 2 * 3").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");

  let s = expr("a + b * c * d + e").unwrap();
  assert_eq!(s.to_string(), "(Add (Add a (Mult (Mult b c) d)) e)");

  let s = expr("f . g . h").unwrap();
  assert_eq!(s.to_string(), "(Period f (Period g h))");

  let s = expr(" 1 + 2 + f . g . h * 3 * 4").unwrap();
  assert_eq!(
    s.to_string(),
    "(Add (Add 1 2) (Mult (Mult (Period f (Period g h)) 3) 4))"
  );

  let s = expr("--1 * 2").unwrap();
  assert_eq!(s.to_string(), "(Mult (Sub (Sub 1)) 2)");
  // assert_eq!(s.to_string(), "(Mult (Dec 1) 2)");

  let s = expr("--f . g").unwrap();
  assert_eq!(s.to_string(), "(Sub (Sub (Period f g)))");

  let s = expr("-9!").unwrap();
  assert_eq!(s.to_string(), "(Sub (Bang 9))");

  let s = expr("f . g !").unwrap();
  assert_eq!(s.to_string(), "(Bang (Period f g))");

  let s = expr("(((0)))").unwrap();
  assert_eq!(s.to_string(), "0");

  let s = expr("(1 + 2) * 3").unwrap();
  assert_eq!(s.to_string(), "(Mult (Add 1 2) 3)");

  let s = expr("1 + (2 * 3)").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");

  let s = expr("(1 + (2 * 3)").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");

  let s = expr("1 + (2 * 3))").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");

  let s = expr("1 + 2 * 3)").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");

  let s = expr("1 + (2 * 3").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");
}
