use crate::common::logger::char_stream::{value::Value, CharStream, Logger, TokenStream};

use super::{
  expr::Expression,
  stmt::{Block, Statement},
  ASTNodeExt,
  Parseable,
};

fn str_parse<'a, T: Parseable<'a>>(input: &str) -> Result<T, String> {
  let mut logger = Logger { logs: vec![] };
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream")?;

  T::parse(&mut stream)
}

fn stmt(input: &str) -> Result<Statement, String> { str_parse::<Statement>(input) }

fn expr(input: &str) -> Result<Expression, String> { str_parse::<Expression>(input) }

#[test]
fn expr_1() {
  let s = expr("1").unwrap();
  assert_eq!(s.to_string(), "1");
}

#[test]
fn expr_2() {
  let s = expr("--f . g").unwrap();
  assert_eq!(s.to_string(), "(Dec (Period f g))");
}

#[test]
fn expr_3() {
  let s = expr("(((0)))").unwrap();
  assert_eq!(s.to_string(), "0");
}

#[test]
fn expr_4() {
  let s = expr("f . g !").unwrap();
  assert_eq!(s.to_string(), "(Bang (Period f g))");
}

#[test]
fn expr_5() {
  let s = expr("-9!").unwrap();
  assert_eq!(s.to_string(), "(Sub (Bang 9))");
}

#[test]
fn expr_6() {
  let s = expr("--1 * 2").unwrap();
  assert_eq!(s.to_string(), "(Mult (Dec 1) 2)");
}

#[test]
fn expr_7() {
  let s = expr(" 1 + 2 + f . g . h * 3 * 4").unwrap();
  assert_eq!(
    s.to_string(),
    "(Add (Add 1 2) (Mult (Mult (Period f (Period g h)) 3) 4))"
  );
}

#[test]
fn expr_8() {
  let s = expr("a + b * c * d + e").unwrap();
  assert_eq!(s.to_string(), "(Add (Add a (Mult (Mult b c) d)) e)");
}

#[test]
fn expr_9() {
  let s = expr("1 + 2 * 3").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");
}

#[test]
fn expr_10() {
  let s = expr("f . g . h").unwrap();
  assert_eq!(s.to_string(), "(Period f (Period g h))");
}

#[test]
fn expr_11() {
  let s = expr("1 + (2 * 3").unwrap_err();
  assert_eq!(s, "Unexpected end of input");
}

#[test]
fn expr_12() {
  let s = expr("1 + 2 * 3)").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");
}

#[test]
fn expr_13() {
  let s = expr("1 + (2 * 3))").unwrap_err();
  assert_eq!(s, "Unexpected closing parenthesis");
}

#[test]
fn expr_14() {
  let s = expr("(1 + (2 * 3)").unwrap_err();
  assert_eq!(s, "Unexpected end of input");
}

#[test]
fn expr_15() {
  let s = expr("1 + (2 * 3)").unwrap();
  assert_eq!(s.to_string(), "(Add 1 (Mult 2 3))");
}

#[test]
fn expr_16() {
  let s = expr("(1 + 2) * 3").unwrap();
  assert_eq!(s.to_string(), "(Mult (Add 1 2) 3)");
}

#[test]
fn expr_17() {
  let s = expr("").unwrap();
  assert_eq!(s, Expression::default());
}

#[test]
fn expr_18() {
  let s = expr("1 + 2) * 3").unwrap_err();
  assert_eq!(s.to_string(), "Unexpected closing parenthesis");
}

#[test]
fn expr_19_unit() {
  let s = expr("()").unwrap();
  assert_eq!(s.to_string(), "()");
}

#[test]
fn expr_20() {
  let s = expr("(a: 1)").unwrap();
  assert_eq!(s.to_string(), "(a: 1)");
}

#[test]
fn expr_21() {
  let s = expr("(a: 1, b: 2)").unwrap();
  assert_eq!(s.to_string(), "(a: 1, b: 2)");
}

#[test]
fn expr_22() {
  let s = expr("(a: 1, b: 2 + 2)").unwrap();
  assert_eq!(s.to_string(), "(a: 1, b: (Add 2 2))");
}

#[test]
fn expr_23() {
  let s = expr("(a: 1, b: 2 * (2 - 1))").unwrap();
  assert_eq!(s.to_string(), "(a: 1, b: (Mult 2 (Sub 2 1)))");
}

#[test]
fn expr_24() {
  let s = expr("(a: (c: \"d\"), b: 2 * (2 - 1))").unwrap();
  assert_eq!(s.to_string(), "(a: (c: \"d\"), b: (Mult 2 (Sub 2 1)))");
}

#[test]
fn expr_26() {
  let s = expr("(a: (c: \"d\"), b: 2 * (2 - 1\n)\n)").unwrap();
  assert_eq!(s.to_string(), "(a: (c: \"d\"), b: (Mult 2 (Sub 2 1)))");
}

#[test]
fn expr_27() {
  let s = expr("(\n\n\n\na: (c: \"d\"), b: 2 * (2 - 1))").unwrap();
  assert_eq!(s.to_string(), "(a: (c: \"d\"), b: (Mult 2 (Sub 2 1)))");
}

#[test]
fn expr_28() {
  let s = expr("x[1]").unwrap();
  assert_eq!(s.to_string(), "(LBrace x 1)");
}

#[test]
fn expr_29() {
  let s = expr("(1, 2)[3]").unwrap();
  assert_eq!(s.to_string(), "(LBrace (1, 2) 3)");
}

#[test]
fn expr_30() {
  let s = expr("x[1+1]").unwrap();
  assert_eq!(s.to_string(), "(LBrace x (Add 1 1))");
}

#[test]
fn expr_31() {
  let s = expr("(1, 2)[3] - 4").unwrap();
  assert_eq!(s.to_string(), "(Sub (LBrace (1, 2) 3) 4)");
}

#[test]
fn expr_32() {
  let s = expr("x[1+1] - 4").unwrap();
  assert_eq!(s.to_string(), "(Sub (LBrace x (Add 1 1)) 4)");
}

#[test]
fn expr_33() {
  let s = expr("not x[1+1] and false").unwrap();
  assert_eq!(s.to_string(), "(and (not (LBrace x (Add 1 1))) false)");
}

#[test]
fn expr_34() {
  let s = expr("[1+1] + 1").unwrap_err();
  assert_eq!(s.to_string(), "Unexpected indexing position");
}

#[test]
fn expr_35() {
  let s = expr("(a: 1, b: 2).b").unwrap();
  assert_eq!(s.to_string(), "(Period (a: 1, b: 2) b)");
}

#[test]
fn expr_36() {
  let s = expr("([1 + 2]: 1, [\"kek\"]: 2)[3]").unwrap();
  assert_eq!(s.to_string(), "(LBrace ((Add 1 2): 1, \"kek\": 2) 3)");
}

#[test]
fn expr_37() {
  let s = expr("x => x + 1").unwrap();
  assert_eq!(s.to_string(), "(Arrow x (Add x 1))");
}

#[test]
fn expr_38() {
  let s = expr("_ => 1 + 2").unwrap();
  assert_eq!(s.to_string(), "(Arrow _ (Add 1 2))");
}

#[test]
fn expr_39() {
  let s = expr("() => 1 + 2").unwrap();
  assert_eq!(s.to_string(), "(Arrow () (Add 1 2))");
}

#[test]
fn expr_40() {
  let s = expr("(x, y) => x + y").unwrap();
  assert_eq!(s.to_string(), "(Arrow (x, y) (Add x y))");
}

#[test]
fn expr_41() {
  let s = expr("(x => x + 1) 1").unwrap();
  assert_eq!(s.to_string(), "(Apply (Arrow x (Add x 1)) 1)");
}

#[test]
fn expr_42() {
  let s = expr("(x => { print x; if x > 0: self (x - 1); }) 5").unwrap();
  assert_eq!(
    s.to_string(),
    format!(
      "(Apply (Arrow x {}) 5)",
      expr("{ print x; if x > 0: self (x - 1); }")
        .unwrap()
        .to_string()
    )
  );
}

#[test]
fn expr_43() {
  let s = expr("x => { print x; if x > 0: self (x - 1); }").unwrap();
  assert_eq!(
    s.to_string(),
    format!(
      "(Arrow x {})",
      expr("{ print x; if x > 0: self (x - 1); }")
        .unwrap()
        .to_string()
    )
  );
}

#[test]
fn expr_44() {
  let s = expr("(x => { print x; if x > 0: self (x - 1); })").unwrap();
  assert_eq!(
    s.to_string(),
    format!(
      "(Arrow x {})",
      expr("{ print x; if x > 0: self (x - 1); }")
        .unwrap()
        .to_string()
    )
  );
}

#[test]
fn expr_consumes_just_enough() -> Result<(), String> {
  let mut logger = Logger { logs: vec![] };
  let input = "2;";
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream".to_string())?;

  let ASTNodeExt {
    node: expr_res,
    span,
  } = Expression::parse_ext(&mut stream).map_err(|err| format!("{:?}", err.error))?;

  assert_eq!(span.length, 1);

  Ok(())
}

#[test]
fn stmt_1() {
  let s = stmt("2").unwrap();
  assert_eq!(s, Statement::Expression(expr("2").unwrap()));
}

#[test]
fn stmt_2() {
  let s = stmt("print 2;").unwrap();
  assert_eq!(s, Statement::Print(expr("2").unwrap()));
}

#[test]
fn stmt_3() {
  let s = stmt("print ;").unwrap();
  assert_eq!(s, Statement::Print(expr("").unwrap()));
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
  assert_eq!(s, Statement::Print(expr("2").unwrap()));
}
#[test]
fn stmt_14() {
  let s = stmt("print (x => x + 1) 1").unwrap();
  assert_eq!(s, Statement::Print(expr("(x => x + 1) 1").unwrap()));
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
fn stmt_empty_block() {
  let s = stmt("{}").unwrap();
  assert_eq!(s, Statement::Block(Block(vec![])));
}

#[test]
fn stmt_block_no_trailing_semicolon() {
  let s = stmt("{ print 2; }").unwrap();
  assert_eq!(
    s,
    Statement::Block(Block(vec![Statement::Print(expr("2").unwrap())]))
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
fn stmt_if_5() {
  let s = stmt("if x + 2 { print x; print x; } else { print \"fuck you\"; }").unwrap_err();
  assert_eq!(s, "Missing colon after condition in if statement");
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
fn stmt_if_missing_branch_2() {
  let s = stmt("if x + 2 else print x").unwrap_err();
  assert_eq!(s, "Missing colon after condition in if statement");
}

#[test]
fn stmt_if_missing_else_branch() {
  let s = stmt("if x + 2: print x; else ").unwrap_err();
  assert_eq!(s, "Empty false branch in if statement");
}

#[test]
fn stmt_if_missing_colon() {
  let s = stmt("if x + 2 print x").unwrap_err();
  assert_eq!(s, "Missing colon after condition in if statement");
}

#[test]
fn stmt_2_ifs() {
  let s = stmt("{ if x + 2: print x; else print x; if x + 2: print x; else print x; }").unwrap();
  assert_eq!(
    s,
    Statement::Block(Block(vec![
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
    ]))
  );
}
