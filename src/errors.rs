use derive_more::Display;

/// matches error productions
#[derive(Debug, Clone, Display, PartialEq)]
pub enum ParsingError {
  Generic(String),
  NoTokens,
  NoOperands,
  EndOfExpression,

  UnexpectedParens,
  UnexpectedBrace,
  UnexpectedBracket,
  MissingParens,
  MissingBrace,
  MissingBracket,
  UnexpectedIndexing,
}

#[macro_export]
macro_rules! parse_error {
  ($($rest: expr),+) => {
    crate::errors::ParsingError::Generic(format!($($rest),+))
  };
}

#[derive(Debug, Clone, Display)]
pub struct RuntimeError {
  pub msg: String,
}

#[macro_export]
macro_rules! runtime_error {
  ($($rest: tt),*) => {
    crate::errors::RuntimeError {
      msg: format!($($rest),*)
    }
  };
}
