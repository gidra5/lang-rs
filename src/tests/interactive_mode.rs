#![allow(unused)]
use std::{cell::RefCell, fmt::Result, rc::Rc};

use crate::{
  common::{
    value::{RecordItem, Value},
    CharStream,
    Logger,
  },
  enviroment::Enviroment,
};

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
        $state.get(&stringify!($name).to_string()),
        Some($value),
        "variable {} have different value than {}",
        stringify!($name).to_string(),
        $value
      );
    )*
  };
}

macro_rules! assert_log {
  ($logs:ident, {$($line:expr),*}) => {
    unsafe { assert!($logs.iter().eq(vec![$($line),*].iter())) }
  };
}

#[test]
fn interactive_scope_mutation() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
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
fn interactive_indexing_tuple() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let x = (1, 2)

    print x[0]
  ",
  );

  assert_env!(state, {
    x: Value::Record(
      vec![
        RecordItem {
          key:   None,
          value: Value::Number(1.),
        },
        RecordItem {
          key:   None,
          value: Value::Number(2.),
        },
      ],
    )
  });

  unsafe { assert!(logs.iter().eq(vec!["1"].iter())) }
}

#[test]
fn interactive_accessing_record_item() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let x = (a: 1, b: 2)

    print x.b
  ",
  );

  assert_env!(state, {
    x: Value::Record(
      vec![
        RecordItem {
          key:   Some(Value::String("a".to_string())),
          value: Value::Number(1.),
        },
        RecordItem {
          key:   Some(Value::String("b".to_string())),
          value: Value::Number(2.),
        },
      ],
    )
  });

  unsafe { assert!(logs.iter().eq(vec!["2"].iter())) }
}

#[test]
fn interactive_for_1() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    for x in _ => 1: print x
  ",
  );

  unsafe { assert!(logs.iter().eq(vec!["1"].iter())) }
}

#[test]
fn interactive_for_2() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    for x in _ => (1, _ => (2, _ => (3, _ => 4))): print x
  ",
  );

  unsafe { assert!(logs.iter().eq(vec!["1", "2", "3", "4"].iter())) }
}

#[test]
fn interactive_for_3() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    for x in x => (1, x => (2, x => (3, x => 4))): print x
  ",
  );

  unsafe { assert!(logs.iter().eq(vec!["1", "2", "3", "4"].iter())) }
}

#[test]
fn interactive_fn_1() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    print (x => x + 1) 1
  ",
  );

  unsafe { assert!(logs.iter().eq(vec!["2"].iter())) }
}

#[test]
fn interactive_fn_2() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    (x => { print x; if x > 0: self (x - 1); }) 5
  ",
  );

  unsafe { assert!(logs.iter().eq(vec!["5", "4", "3", "2", "1", "0"].iter())) }
}

#[test]
fn interactive_fn_3() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("print (x => y => z => x + y + z) 1 2 3");

  unsafe { assert!(logs.iter().eq(vec!["6"].iter())) }
}

#[test]
fn interactive_pattern_matching() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("print () is ()");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_14() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("print x is _");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_3() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = 1; print x is 1");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_4() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = 1; print x is 2");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_5() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (1, 2); print x is (1, 2)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_6() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (1, 2); print x is (_, 2)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_7() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (1, 2); print x is (2, 2)");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_8() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (1, 2); print x is (1, _)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_9() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (1, 2); print x is (1, 3)");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_10() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a: 1, b: 2); print x is (a: 1, b: 2)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_11() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a: 1, b: 2); print x is (a, b: 2)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_12() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a: 1, b: 2); print x is (a: 2, b: 2)");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_13() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = ([1+1]: 1, [2+2]: 2); print x is ([2]: _, [4]: 2)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_15() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("print 1 is 2");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_16() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a: 1, b: 2); print x is (a, b)");

  unsafe { assert!(logs.iter().eq(vec!["true"].iter())) }
}

#[test]
fn interactive_pattern_matching_17() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a: 1, b: 2); print x is (a, c)");

  unsafe { assert!(logs.iter().eq(vec!["false"].iter())) }
}

#[test]
fn interactive_pattern_matching_18() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (a, b) => { print a; print b; }; x (1, 2)");

  unsafe { assert!(logs.iter().eq(vec!["1", "2"].iter())) }
}

#[test]
fn interactive_pattern_matching_19() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret("let x = (x, y) => x + y; print x (1, 2)");

  unsafe { assert!(logs.iter().eq(vec!["3"].iter())) }
}

#[test]
fn interactive_range() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => {
      let res

      if left < right: {
        res = (left, self (left + 1, right))
      } else if left > right: {
        res = (left, self (left - 1, right))
      } else {
        res = right
      }

      _ => res
    }

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_2() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => {
      let res

      if left < right
        res = (left, self (left + 1, right))
      else if left > right
        res = (left, self (left - 1, right))
      else
        res = right

      _ => res
    }

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_3() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => {
      let range = self
      let res = {
        if left < right
          (left, range(left + 1, right))
        else if left > right
          (left, range(left - 1, right))
        else
          right
      }

      _ => res
    }

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_4() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => {
      let range = self

      _ => {
        if left < right
          (left, range(left + 1, right))
        else if left > right
          (left, range(left - 1, right))
        else
          right
      }
    }

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_5() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => {
      let range = self

      _ => {
        if left < right
          (left, range(left + 1, right))
        else if left > right
          (left, range(left - 1, right))
        else
          right
      }
    }

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_6() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = (left, right) => _ =>
      if left < right
        (left, self#1 (left + 1, right))
      else if left > right
        (left, self#1 (left - 1, right))
      else
        right

    for x in range(1,3): print x
  ",
  );

  assert_log!(logs, { "1", "2", "3" });
}

#[test]
fn interactive_range_7() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let range = x => {
      let y = 1
      _ => {
        let z = 2
        _ => (x, y, z)
      }
    }

    print range(3)()() == range(3)()()
  ",
  );

  assert_log!(logs, { "true" });
}

#[test]
fn interactive_if() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let x = true
    let y = false

    if true: print \"true\"
    if false: print \"false\"
    if x: print \"x\"
    if y: print \"unreachable\" else print \"not y\"
    if y and x: print \"unreachable\" else print \"not (y and x)\"
    if y or x: print \"y or x\" else print \"unreachable\"
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
fn interactive_block_if_expr() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let x = true
    let y = false
    let z = { if x: 1 else 2; }
    let w = { if y: 3 else 4; }

    print z
    print w
  ",
  );

  assert_log!(logs, { "1", "4" })
}

#[test]
fn interactive_if_equal() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let x = true
    let y = false
    let z

    if x: z = 1
  ",
  );

  assert_env!(state, { z: Value::Number(1.) });
}

#[test]
fn interactive_block_if_2() -> std::result::Result<(), String> {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let left = 1
    let right = 2
    let range = {
      let res

      if left < right
        res = (left, (left + 1, right))
      else if left > right
        res = (left, (left - 1, right))
      else
        res = right

      _ => res
    }
  ",
  );

  Ok(())
}

#[test]
fn interactive_2() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
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
fn interactive_3() {
  let InteractiveMode {
    rl: _,
    env: state,
    context,
    logger: Logger { logs },
  } = interpret(
    "
    let a = \"global a\"
    let b = \"global b\"
    let c = \"global c\"
    {
      let a = \"outer a\"
      let b = \"outer b\"
      {
        let a = \"inner a\"
        print a#1
        print b
        print c
      }
      print a#1
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
    assert!(logs.iter().eq(
      vec![
        "\"outer a\"",
        "\"outer b\"",
        "\"global c\"",
        "\"global a\"",
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

// #[test]
// fn interactive_weird() {
//   let InteractiveMode {
//     rl: _,
//     env: state,
//     context,
//     logger: Logger { logs },
//   } = interpret(
//     "
//     let else = else => else

//     if else: else else; else else else
//   ",
//   );
// }
