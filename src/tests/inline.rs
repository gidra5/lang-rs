use crate::{
  common::{tests::expr, Logger},
  inline_expr,
};

#[test]
fn inline_1() {
  let mut logger = Logger { logs: vec![] };
  assert_eq!(
    inline_expr!(&mut logger, "x", x: expr("1").unwrap()),
    expr("1")
  )
}

#[test]
fn inline_2() {
  let mut logger = Logger { logs: vec![] };
  assert_eq!(
    inline_expr!(&mut logger, "y + x", x: expr("1").unwrap()),
    expr("y + 1")
  )
}

#[test]
fn inline_3() {
  let mut logger = Logger { logs: vec![] };
  assert_eq!(
    inline_expr!(&mut logger, "y + x", x: expr("1").unwrap(), y: expr("2").unwrap()),
    expr("2 + 1")
  )
}

#[test]
fn inline_4() {
  let mut logger = Logger { logs: vec![] };
  assert_eq!(
    inline_expr!(&mut logger, "(x => x + 1) x", x: expr("1").unwrap()),
    expr("(x => x + 1) 1")
  )
}

#[test]
fn inline_5() {
  let mut logger = Logger { logs: vec![] };
  assert_eq!(
    inline_expr!(&mut logger, "y x", x: expr("1").unwrap(), y: expr("(x => x + 1)").unwrap()),
    expr("(x => x + 1) 1")
  )
}
