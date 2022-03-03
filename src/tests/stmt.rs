use crate::{
  ast::{ASTNodeExt, Expression, Parseable, Statement},
  common::{
    tests::{expr, stmt},
    CharStream,
    Logger,
  },
  token::TokenStream,
};

#[test]
fn stmt_1() {
  let s = stmt("2").unwrap();
  assert_eq!(s, Statement::Expression(expr("2").unwrap()));
}

#[test]
fn stmt_2() {
  let s = stmt("print 2;").unwrap();
  assert_eq!(s, Statement::Expression(expr("print 2").unwrap()));
}

#[test]
fn stmt_3() {
  let s = stmt("print ;").unwrap();
  assert_eq!(s, Statement::Expression(expr("print").unwrap()));
}

#[test]
fn stmt_4() {
  let s = stmt(";").unwrap();
  assert_eq!(s, Statement::Expression(expr("").unwrap()));
}

#[test]
fn stmt_5() {
  let s = stmt("let").unwrap_err();
  assert_eq!(s, "Expected identifier at let statement.");
}

#[test]
fn stmt_6() {
  let s = stmt("let x").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression::Prefix {
      op:    Box::new(expr("let").unwrap()),
      right: Box::new(expr("x").unwrap()),
    })
  );
}

#[test]
fn stmt_7() {
  let s = stmt("let x;").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression::Prefix {
      op:    Box::new(expr("let").unwrap()),
      right: Box::new(expr("x").unwrap()),
    })
  );
}

#[test]
fn stmt_8() {
  let s = stmt("let x =").unwrap_err();
  assert_eq!(
    s,
    "Error at expression after '=' in let statement: No expression"
  );
}

#[test]
fn stmt_9() {
  let s = stmt("let x = (2").unwrap_err();
  assert_eq!(
    s,
    "Error at expression after '=' in let statement: Unexpected end of input"
  );
}

#[test]
fn stmt_10() {
  let s = stmt("let x = 2").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression::Infix {
      left:  Box::new(expr("let x").unwrap()),
      op:    Box::new(expr("=").unwrap()),
      right: Box::new(expr("2").unwrap()),
    })
  );
}

#[test]
fn stmt_11() {
  let s = stmt("let x = 2;").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression::Infix {
      left:  Box::new(expr("let x").unwrap()),
      op:    Box::new(expr("=").unwrap()),
      right: Box::new(expr("2").unwrap()),
    })
  );
}

#[test]
fn stmt_12() {
  let s = stmt("2;").unwrap();
  assert_eq!(s, Statement::Expression(expr("2").unwrap()));
}

#[test]
fn stmt_13() {
  let s = stmt("print 2").unwrap();
  assert_eq!(s, Statement::Expression(expr("print 2").unwrap()));
}
#[test]
fn stmt_14() {
  let s = stmt("print (x => x + 1) 1").unwrap();
  assert_eq!(
    s,
    Statement::Expression(expr("print (x => x + 1) 1").unwrap())
  );
}

#[test]
fn stmt_let_fn() {
  let s = stmt("let x = () => 1 + 2").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression::Infix {
      left:  Box::new(expr("let x").unwrap()),
      op:    Box::new(expr("=").unwrap()),
      right: Box::new(expr("() => 1 + 2").unwrap()),
    })
  );
}
