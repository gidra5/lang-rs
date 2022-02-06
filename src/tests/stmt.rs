use crate::{
  ast::{ASTNodeExt, Expression, Op, Parseable, Statement},
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
  assert_eq!(s, Statement::Let("x".to_string(), None));
}

#[test]
fn stmt_7() {
  let s = stmt("let x;").unwrap();
  assert_eq!(s, Statement::Let("x".to_string(), None));
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
  assert_eq!(s, Statement::Let("x".to_string(), Some(expr("2").unwrap())));
}

#[test]
fn stmt_11() {
  let s = stmt("let x = 2;").unwrap();
  assert_eq!(s, Statement::Let("x".to_string(), Some(expr("2").unwrap())));
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
fn stmt_for_1() {
  let s = stmt("for x in y: print x").unwrap();
  assert_eq!(
    s,
    Statement::For("x".to_string(), expr("y").unwrap(), vec![
      stmt("print x").unwrap()
    ],)
  );
}

#[test]
fn stmt_for_2() {
  let s = stmt("for x in y: { print x; }").unwrap();
  assert_eq!(
    s,
    Statement::For("x".to_string(), expr("y").unwrap(), vec![
      stmt("print x").unwrap()
    ],)
  );
}

#[test]
fn stmt_for_3() {
  let s = stmt("for x in range(1, 20)\n print x").unwrap();
  assert_eq!(
    s,
    Statement::For("x".to_string(), expr("range(1, 20)").unwrap(), vec![stmt(
      "print x"
    )
    .unwrap()],)
  );
}

#[test]
fn stmt_let_fn() {
  let s = stmt("let x = () => 1 + 2").unwrap();
  assert_eq!(
    s,
    Statement::Let("x".to_string(), Some(expr("() => 1 + 2").unwrap()))
  );
}

#[test]
fn stmt_if_1() {
  let s = stmt("if x + 2: print x; else print \"fuck you\"").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![stmt("print \"fuck you\"").unwrap()]
    )
  );
}

#[test]
fn stmt_if_2() {
  let s = stmt("if x + 2: { print x; print x; } else { print \"fuck you\"; }").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap(), stmt("print x").unwrap()],
      vec![stmt("print \"fuck you\"").unwrap()]
    )
  );
}

#[test]
fn stmt_if_3() {
  let s = stmt("if x + 2: print x; else { print \"fuck you\"; }").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![stmt("print \"fuck you\"").unwrap()]
    )
  );
}

#[test]
fn stmt_if_4() {
  let s = stmt("if x + 2: { print x; } else print \"fuck you\"").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![stmt("print \"fuck you\"").unwrap()]
    )
  );
}

#[test]
fn stmt_if_no_else() {
  let s = stmt("if x + 2: print x").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![]
    )
  );
}

#[test]
fn stmt_if_no_else_newline() {
  let s = stmt("if x + 2\n print x").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![]
    )
  );
}

#[test]
fn stmt_if_no_else_bracketed() {
  let s = stmt("if x + 2: { print x; }").unwrap();
  assert_eq!(
    s,
    Statement::If(
      expr("x + 2").unwrap(),
      vec![stmt("print x").unwrap()],
      vec![]
    )
  );
}

#[test]
fn stmt_if_missing_branch() {
  let s = stmt("if x + 2: else print x").unwrap_err();
  assert_eq!(s, "Empty true branch in if statement");
}

#[test]
fn stmt_if_missing_else_branch() {
  let s = stmt("if x + 2: print x; else ").unwrap_err();
  assert_eq!(s, "Empty false branch in if statement");
}

#[test]
fn stmt_if_missing_colon() {
  let s = stmt("if x + 2 print x").unwrap_err();
  assert_eq!(s, "Missing colon after condition in an if statement");
}

#[test]
fn stmt_if_nesting() {
  let s = stmt("if a\n 1\n else if b\n 2\n else\n 3").unwrap();
  assert_eq!(
    s,
    Statement::If(expr("a").unwrap(), vec![stmt("1").unwrap()], vec![
      Statement::If(expr("b").unwrap(), vec![stmt("2").unwrap()], vec![stmt(
        "3"
      )
      .unwrap()])
    ])
  );
}

#[test]
fn stmt_if_block_nesting() {
  let s = stmt("{ \nif a\n 1\n else if b\n 2\n else\n 3\n }").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression {
      left:  None,
      right: None,
      op:    Op::Block(vec![Statement::If(
        expr("a").unwrap(),
        vec![stmt("1").unwrap()],
        vec![Statement::If(
          expr("b").unwrap(),
          vec![stmt("2").unwrap()],
          vec![stmt("3").unwrap()]
        )]
      )]),
    })
  );
}

#[test]
fn stmt_if_block_nesting_equal() {
  let s = stmt("{ \nif a\n x = 1\n else if b\n x = 2\n else\n x = 3\n }").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression {
      left:  None,
      right: None,
      op:    Op::Block(vec![Statement::If(
        expr("a").unwrap(),
        vec![stmt("x = 1").unwrap()],
        vec![Statement::If(
          expr("b").unwrap(),
          vec![stmt("x = 2").unwrap()],
          vec![stmt("x = 3").unwrap()]
        )]
      )]),
    })
  );
}

#[test]
fn stmt_2_ifs() {
  let s = stmt("{ if x + 2: print x; else print x; if x + 2: print x; else print x; }").unwrap();
  assert_eq!(
    s,
    Statement::Expression(Expression {
      left:  None,
      right: None,
      op:    Op::Block(vec![
        Statement::If(
          expr("x + 2").unwrap(),
          vec![stmt("print x").unwrap()],
          vec![stmt("print x").unwrap()]
        ),
        Statement::If(
          expr("x + 2").unwrap(),
          vec![stmt("print x").unwrap()],
          vec![stmt("print x").unwrap()]
        )
      ]),
    })
  );
}
