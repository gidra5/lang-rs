use crate::common::logger::char_stream::{CharStream, TokenStream};

use super::{Expression, Parseable, Statement};

fn stmt(input: &str) -> Result<Statement, String> {
  let mut stream =
    TokenStream::new(CharStream::from_str(input)).ok_or("Failed to create TokenStream")?;

  Statement::parse(&mut stream)
}

fn expr(input: &str) -> Result<Expression, String> {
  let mut stream =
    TokenStream::new(CharStream::from_str(input)).ok_or("Failed to create TokenStream")?;

  Expression::parse(&mut stream)
}

#[test]
fn expr_1_tests() {
  let s = expr("1").unwrap();
  assert_eq!(s.to_string(), "1");
}

#[test]
fn expr_2_tests() {
  let s = expr("--f . g").unwrap();
  assert_eq!(s.to_string(), "(Sub (Sub (Period f g)))");
}

#[test]
fn expr_3_tests() {
  let s = expr("(((0)))").unwrap();
  assert_eq!(s.to_string(), "0");
}

#[test]
fn expr_4_tests() {
  let s = expr("f . g !").unwrap();
  assert_eq!(s.to_string(), "(Bang (Period f g))");
}

#[test]
fn expr_5_tests() {
  let s = expr("-9!").unwrap();
  assert_eq!(s.to_string(), "(Sub (Bang 9))");
}

#[test]
fn expr_6_tests() {
  let s = expr("--1 * 2").unwrap();
  assert_eq!(s.to_string(), "(Mult (Sub (Sub 1)) 2)");
  // assert_eq!(s.to_string(), "(Mult (Dec 1) 2)");
}

#[test]
fn expr_7_tests() {
  let s = expr(" 1 + 2 + f . g . h * 3 * 4").unwrap();
  assert_eq!(
    s.to_string(),
    "(Add (Add 1 2) (Mult (Mult (Period f (Period g h)) 3) 4))"
  );
}

#[test]
fn expr_8_tests() {
  let s = expr("a + b * c * d + e").unwrap();
  assert_eq!(s.to_string(), "(Add (Add a (Mult (Mult b c) d)) e)");
}

#[test]
fn expr_9_tests() {
  let s = expr("1 + 2 * 3").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");
}

#[test]
fn expr_10_tests() {
  let s = expr("f . g . h").unwrap();
  assert_eq!(s.to_string(), "(Period f (Period g h))");
}

#[test]
fn expr_11_tests() {
  let s = expr("1 + (2 * 3").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");
}

#[test]
fn expr_12_tests() {
  let s = expr("1 + 2 * 3)").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");
}

#[test]
fn expr_13_tests() {
  let s = expr("1 + (2 * 3))").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");
}

#[test]
fn expr_14_tests() {
  let s = expr("(1 + (2 * 3)").unwrap_err();
  assert_eq!(s, "Expected closing parenthesis");
}

#[test]
fn expr_15_tests() {
  let s = expr("1 + (2 * 3)").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");
}

#[test]
fn expr_16_tests() {
  let s = expr("(1 + 2) * 3").unwrap();
  assert_eq!(s.to_string(), "(Mult (Add 1 2) 3)");
}

#[test]
fn expr_17_tests() {
  let s = expr("").unwrap_err();
  assert_eq!(s, "No expression");
}


#[test]
fn stmt_1_tests() {
  let s = stmt("2").unwrap_err();
  assert_eq!(s, "Missing semicolon at the end of statement");
}

#[test]
fn stmt_2_tests() {
  let s = stmt("print 2;").unwrap();
  assert_eq!(s, Statement::Print(expr("2").unwrap()));
}

#[test]
fn stmt_3_tests() {
  let s = stmt("print ;").unwrap_err();
  assert_eq!(s, "No expression");
}

#[test]
fn stmt_4_tests() {
  let s = stmt(";").unwrap_err();
  assert_eq!(s, "No expression");
}

#[test]
fn stmt_5_tests() {
  let s = stmt("let").unwrap_err();
  assert_eq!(s, "Expected identifier at let statement.");
}

#[test]
fn stmt_6_tests() {
  let s = stmt("let x").unwrap_err();
  assert_eq!(s, "Missing semicolon at the end of statement");
}

#[test]
fn stmt_7_tests() {
  let s = stmt("let x;").unwrap();
  assert_eq!(s, Statement::Let("x".to_string(), None));
}

#[test]
fn stmt_8_tests() {
  let s = stmt("let x =").unwrap_err();
  assert_eq!(
    s,
    "Error at expression after '=' in let statement: No expression"
  );
}

#[test]
fn stmt_9_tests() {
  let s = stmt("let x = (2").unwrap_err();
  assert_eq!(
    s,
    "Error at expression after '=' in let statement: Expected closing parenthesis"
  );
}

#[test]
fn stmt_10_tests() {
  let s = stmt("let x = 2").unwrap_err();
  assert_eq!(s, "Missing semicolon at the end of statement");
}

#[test]
fn stmt_11_tests() {
  let s = stmt("let x = 2;").unwrap();
  assert_eq!(s, Statement::Let("x".to_string(), Some(expr("2").unwrap())));
}

#[test]
fn stmt_12_tests() {
  let s = stmt("2;").unwrap();
  assert_eq!(s, Statement::Expression(expr("2").unwrap()));
}

#[test]
fn stmt_13_tests() {
  let s = stmt("print 2").unwrap_err();
  assert_eq!(s, "Missing semicolon at the end of statement");
}
