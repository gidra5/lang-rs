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
    format!("(Apply (Apply (Apply (Arrow x (Arrow y (Arrow z (Add (Add x y) z)))) 1) 2) 3)")
  );
}

#[test]
fn expr_47() {
  let s = expr("x 1 2 3").unwrap();
  assert_eq!(s.to_string(), format!("(Apply (Apply (Apply x 1) 2) 3)"));
}

#[test]
fn expr_48() {
  let s = expr("(x) (1)").unwrap();
  assert_eq!(s.to_string(), format!("(Apply x 1)"));
}

#[test]
fn expr_49() {
  let s = expr("x (1)").unwrap();
  assert_eq!(s.to_string(), format!("(Apply x 1)"));
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
    format!("(Await (Apply (Async (Arrow x (Add x 1))) 2))")
  );
}

#[test]
fn expr_apply_inline_fn() {
  let s = expr("inline (x => x + 1) 2").unwrap();
  assert_eq!(
    s.to_string(),
    format!("(Apply (Inline (Arrow x (Add x 1))) 2)")
  );
}

#[test]
fn expr_apply_inline_x() {
  let s = expr("inline x 2").unwrap();
  assert_eq!(s.to_string(), format!("(Apply (Inline x) 2)"));
}