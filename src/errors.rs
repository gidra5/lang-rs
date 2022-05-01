use std::fmt::{Display, Formatter};

use itertools::Itertools;

/// matches error productions
#[derive(Debug)]
pub enum ParsingError {
  Generic(String),

  Aggregate(Vec<ParsingError>),
}

#[macro_export]
macro_rules! parse_error {
  ($($rest: expr),+) => {
    crate::errors::ParsingError::Generic(
      format!($($rest),+)
    )
  };
}

impl Display for ParsingError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Generic(msg) => write!(f, "{}", msg),
      Self::Aggregate(errs) => write!(f, "{}", errs.into_iter().join("\n")),
    }
  }
}

pub enum RuntimeError {
  Generic(String),
}

#[macro_export]
macro_rules! runtime_error {
  ($($rest: expr),+) => {
    crate::errors::RuntimeError::Generic(
      format!($($rest),+)
    )
  };
}


impl Display for RuntimeError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Generic(msg) => write!(f, "{}", msg),
    }
  }
}
