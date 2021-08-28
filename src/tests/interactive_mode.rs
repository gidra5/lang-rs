#![allow(unused)]
use crate::common::logger::char_stream::{value::Value, CharStream};

use super::InteractiveMode;

fn interpret(input: &str) -> InteractiveMode {
  let char_stream = CharStream::from_str(input);
  let mut interpreter = InteractiveMode::new();

  interpreter.exec(char_stream);

  interpreter
}

macro_rules! assert_env {
  ($state:ident, {$($name:ident: $value:expr),*}) => {
    $(
      assert_eq!(
        $state.borrow().get(stringify!($name).to_string()),
        Some($value),
        "variable {} have different value",
        stringify!($name).to_string()
      );
    )*
  };
}

#[test]
fn interactive_scope_mutation() {
  let InteractiveMode { rl: _, env: state } = interpret(
    "
    let x
    {
      x = 2
    }
  ",
  );

  assert_env!(state, {
    x: Value::Number(2.)
  });
}

#[test]
fn interactive_if() {
  let InteractiveMode { rl: _, env: state } = interpret(
    "
    let x = true
    let y = false

    if true: print \"true\"
    if false: print \"false\"
    if x: print \"x\"
    if y: print \"unreachable\"; else print \"not y\"
    if y and x: print \"unreachable\"; else print \"not (y and x)\"
    if y or x: print \"y or x\"; else print \"unreachable\"
  ",
  );

  unsafe {
    assert!(logs.iter().eq(
      vec![
        "\"true\"",
        "\"x\"",
        "\"not y\"",
        "\"not (y and x)\"",
        "\"y or x\"",
      ]
      .iter()
    ))
  }
}

#[test]
fn interactive_2() {
  let InteractiveMode { rl: _, env: state } = interpret(
    "
    let a = \"global a\"
    let b = \"global b\"
    let c = \"global c\"
    {
      let a = \"outer a\"
      let b = \"outer b\"
      {
        let a = \"inner a\"
        print a
        print b
        print c
      }
      print a
      print b
      print c
    }
    print a
    print b
    print c
  ",
  );

  assert_env!(state, {
    a: Value::String("global a".to_string()),
    b: Value::String("global b".to_string()),
    c: Value::String("global c".to_string())
  });

  unsafe {
    println!("{:?}", logs);
    assert!(logs.iter().eq(
      vec![
        "\"inner a\"",
        "\"outer b\"",
        "\"global c\"",
        "\"outer a\"",
        "\"outer b\"",
        "\"global c\"",
        "\"global a\"",
        "\"global b\"",
        "\"global c\""
      ]
      .iter()
    ))
  }
}

#[test]
fn interactive_weird() {
  let InteractiveMode { rl: _, env: state } = interpret(
    "
    let else = (else) => else

    if else: else else; else else else
  ",
  );
}
