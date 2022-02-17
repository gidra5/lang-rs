use crate::{
  ast::{ASTNodeExt, Expression, Parseable, ParsingContext, Statement},
  common::{
    tests::{expr, stmt},
    CharStream,
    Logger,
  },
  map,
  token::TokenStream,
};

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
  assert_eq!(s.to_string(), "((Arrow x (Add x 1)) 1)");
}

#[test]
fn expr_42() {
  let s = expr("(x => { print x; if x > 0: self (x - 1); }) 5").unwrap();
  assert_eq!(
    s.to_string(),
    format!(
      "((Arrow x {}) 5)",
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
fn expr_45() {
  let s = expr("x => y => z => x + y + z").unwrap();
  assert_eq!(
    s.to_string(),
    format!("(Arrow x (Arrow y (Arrow z (Add (Add x y) z))))")
  );
}

#[test]
fn expr_46() {
  let s = expr("(x => y => z => x + y + z) 1 2 3").unwrap();
  assert_eq!(
    s.to_string(),
    format!("((((Arrow x (Arrow y (Arrow z (Add (Add x y) z)))) 1) 2) 3)")
  );
}

#[test]
fn expr_47() {
  let s = expr("x 1 2 3").unwrap();
  assert_eq!(s.to_string(), format!("(((x 1) 2) 3)"));
}

#[test]
fn expr_48() {
  let s = expr("(x) (1)").unwrap();
  assert_eq!(s.to_string(), format!("(x 1)"));
}

#[test]
fn expr_49() {
  let s = expr("x (1)").unwrap();
  assert_eq!(s.to_string(), format!("(x 1)"));
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
  } = Expression::parse_ext(&mut stream, &mut ParsingContext {
    declarations: map![],
  })
  .map_err(|err| format!("{}", err.0))?;

  assert_eq!(span.length, 1);

  Ok(())
}

#[test]
fn expr_consumes_just_enough_2() -> Result<(), String> {
  let mut logger = Logger { logs: vec![] };
  let input = "(2);";
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream".to_string())?;

  let ASTNodeExt {
    node: expr_res,
    span,
  } = Expression::parse_ext(&mut stream, &mut ParsingContext {
    declarations: map![],
  })
  .map_err(|err| format!("{}", err.0))?;

  assert_eq!(span.length, 3);

  Ok(())
}

#[test]
fn expr_consumes_just_enough_3() -> Result<(), String> {
  let mut logger = Logger { logs: vec![] };
  let input = "();";
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream".to_string())?;

  let ASTNodeExt {
    node: expr_res,
    span,
  } = Expression::parse_ext(&mut stream, &mut ParsingContext {
    declarations: map![],
  })
  .map_err(|err| format!("{}", err.0))?;

  assert_eq!(span.length, 2);

  Ok(())
}

#[test]
fn expr_consumes_just_enough_4() -> Result<(), String> {
  let mut logger = Logger { logs: vec![] };
  let input = "{};";
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream".to_string())?;

  let ASTNodeExt {
    node: expr_res,
    span,
  } = Expression::parse_ext(&mut stream, &mut ParsingContext {
    declarations: map![],
  })
  .map_err(|err| format!("{}", err.0))?;

  assert_eq!(span.length, 2);

  Ok(())
}

#[test]
fn expr_consumes_just_enough_5() -> Result<(), String> {
  let mut logger = Logger { logs: vec![] };
  let input = "{ 5; };";
  let mut stream = TokenStream::new(CharStream::from_str(input), &mut logger)
    .ok_or("Failed to create TokenStream".to_string())?;

  let ASTNodeExt {
    node: expr_res,
    span,
  } = Expression::parse_ext(&mut stream, &mut ParsingContext {
    declarations: map![],
  })
  .map_err(|err| format!("{}", err.0))?;

  assert_eq!(span.length, 4);

  Ok(())
}

#[test]
fn expr_async() {
  let s = expr("async x").unwrap();
  assert_eq!(s.to_string(), format!("(Async x)"));
}

#[test]
fn expr_await() {
  let s = expr("await x").unwrap();
  assert_eq!(s.to_string(), format!("(Await x)"));
}

#[test]
fn expr_await_async() {
  let s = expr("await async x").unwrap();
  assert_eq!(s.to_string(), format!("(Await (Async x))"));
}

#[test]
fn expr_await_async_fn() {
  let s = expr("await async _ => 1").unwrap();
  assert_eq!(s.to_string(), format!("(Await (Async (Arrow _ 1)))"));
}

#[test]
fn expr_await_apply_async_fn() {
  let s = expr("await async (x => x + 1) 2").unwrap();
  assert_eq!(
    s.to_string(),
    format!("(Await ((Async (Arrow x (Add x 1))) 2))")
  );
}

#[test]
fn expr_apply_inline_fn() {
  let s = expr("inline (x => x + 1) 2").unwrap();
  assert_eq!(s.to_string(), format!("((Inline (Arrow x (Add x 1))) 2)"));
}

#[test]
fn expr_apply_inline_x() {
  let s = expr("inline x 2").unwrap();
  assert_eq!(s.to_string(), format!("((Inline x) 2)"));
}

#[test]
fn expr_for_1() {
  let s = expr("for x in y: print x").unwrap();
  assert_eq!(
    s,
    Expression::For(
      Box::new(expr("x").unwrap()),
      Box::new(expr("y").unwrap()),
      Box::new(expr("print x").unwrap()),
    )
  );
}

#[test]
fn expr_for_2() {
  let s = expr("for x in y: { print x; }").unwrap();
  assert_eq!(
    s,
    Expression::For(
      Box::new(expr("x").unwrap()),
      Box::new(expr("y").unwrap()),
      Box::new(expr("{ print x; }").unwrap()),
    )
  );
}

#[test]
fn expr_for_3() {
  let s = expr("for x in range(1, 20)\n print x").unwrap();
  assert_eq!(
    s,
    Expression::For(
      Box::new(expr("x").unwrap()),
      Box::new(expr("range(1, 20)").unwrap()),
      Box::new(expr("print x").unwrap()),
    )
  );
}

#[test]
fn expr_if_1() {
  let s = expr("if x + 2: print x else print \"fuck you\"").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("print x").unwrap()),
      Some(Box::new(expr("print \"fuck you\"").unwrap())),
    )
  );
}

#[test]
fn expr_if_2() {
  let s = expr("if x + 2: { print x; print x; } else { print \"fuck you\"; }").unwrap();

  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("{ print x; print x; }").unwrap()),
      Some(Box::new(expr("{ print \"fuck you\"; }").unwrap())),
    )
  );
}

#[test]
fn expr_if_3() {
  let s = expr("if x + 2: print x else { print \"fuck you\"; }").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("print x").unwrap()),
      Some(Box::new(expr("{ print \"fuck you\"; }").unwrap())),
    )
  );
}

#[test]
fn expr_if_4() {
  let s = expr("if x + 2: { print x; } else print \"fuck you\"").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("{ print x; }").unwrap()),
      Some(Box::new(expr("print \"fuck you\"").unwrap())),
    )
  );
}

#[test]
fn expr_if_no_else() {
  let s = expr("if x + 2: print x").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("print x").unwrap()),
      None,
    )
  );
}

#[test]
fn expr_if_no_else_newline() {
  let s = expr("if x + 2\n print x").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("print x").unwrap()),
      None,
    )
  );
}

#[test]
fn expr_if_block_no_else() {
  let s = expr("if x + 2: { print x; }").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("x + 2").unwrap()),
      Box::new(expr("{ print x; }").unwrap()),
      None,
    )
  );
}

#[test]
fn expr_if_missing_branch() {
  let s = expr("if x + 2: else print x").unwrap_err();
  assert_eq!(s, "Empty true branch in if expression");
}

#[test]
fn expr_if_missing_else_branch() {
  let s = expr("if x + 2: print x else ").unwrap_err();
  assert_eq!(s, "Empty false branch in if expression");
}

#[test]
fn expr_if_missing_colon() {
  let s = expr("if x + 2 print x").unwrap_err();
  assert_eq!(
    s,
    "Missing colon after condition expression in an if expression"
  );
}

#[test]
fn expr_if_nesting() {
  let s = expr("if a\n 1\n else if b\n 2\n else\n 3").unwrap();
  assert_eq!(
    s,
    Expression::If(
      Box::new(expr("a").unwrap()),
      Box::new(expr("1").unwrap()),
      Some(Box::new(Expression::If(
        Box::new(expr("b").unwrap()),
        Box::new(expr("2").unwrap()),
        Some(Box::new(expr("3").unwrap()))
      )))
    )
  );
}

#[test]
fn expr_if_block_nesting() {
  let s = expr("{ \nif a\n 1\n else if b\n 2\n else\n 3\n }").unwrap();
  assert_eq!(
    s,
    Expression::Block(vec![Statement::Expression(Expression::If(
      Box::new(expr("a").unwrap()),
      Box::new(expr("1").unwrap()),
      Some(Box::new(Expression::If(
        Box::new(expr("b").unwrap()),
        Box::new(expr("2").unwrap()),
        Some(Box::new(expr("3").unwrap()))
      )))
    ))])
  );
}

#[test]
fn expr_if_block_nesting_equal() {
  let s = expr("{ \nif a\n x = 1\n else if b\n x = 2\n else\n x = 3\n }").unwrap();
  assert_eq!(
    s,
    Expression::Block(vec![Statement::Expression(Expression::If(
      Box::new(expr("a").unwrap()),
      Box::new(expr("x = 1").unwrap()),
      Some(Box::new(Expression::If(
        Box::new(expr("b").unwrap()),
        Box::new(expr("x = 2").unwrap()),
        Some(Box::new(expr("x = 3").unwrap()))
      ),))
    ),)]),
  );
}

#[test]
fn expr_2_ifs() {
  let s = expr("{ if x + 2: print x else print x; if x + 2: print x else print x; }").unwrap();
  assert_eq!(
    s,
    Expression::Block(vec![
      Statement::Expression(Expression::If(
        Box::new(expr("x + 2").unwrap()),
        Box::new(expr("print x").unwrap()),
        Some(Box::new(expr("print x").unwrap()))
      ),),
      Statement::Expression(Expression::If(
        Box::new(expr("x + 2").unwrap()),
        Box::new(expr("print x").unwrap()),
        Some(Box::new(expr("print x").unwrap()))
      ),),
    ]),
  );
}
