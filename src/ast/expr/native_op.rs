use std::{collections::HashMap, fmt::Display};

use itertools::Itertools;

use crate::{
  ast::{Evaluatable, RuntimeError},
  common::value::Value,
  enviroment::Enviroment,
  map,
  runtime_error,
};

fn fact(n: f64) -> f64 {
  if n <= 1. {
    1.
  } else {
    n * fact(n - 1.)
  }
}


fn exec_native(name: &str, env: &mut Enviroment, args: &[Value]) -> Result<Value, RuntimeError> {
  Ok(match (name, args) {
    ("!", [Value::Number(num)]) => Value::Number(fact(*num)),
    ("!", [_]) => return Err(runtime_error!("Operator '{name}' expects a number")),
    ("!", _) => {
      return Err(runtime_error!(
        "Operator '{name}' expects exactly 1 argument"
      ))
    },

    (name, args) => {
      return Err(runtime_error!(
        "No such native function as '{name}' expecting [{}]",
        args.into_iter().join(", ")
      ))
    },
  })
}
