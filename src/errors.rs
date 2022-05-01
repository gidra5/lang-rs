use derive_more::Display;

/// matches error productions
#[derive(Debug, Clone, Display)]
pub struct ParsingError {
  pub msg: String,
}

#[macro_export]
macro_rules! parse_error {
  ($($rest: expr),+) => {
    crate::errors::ParsingError {
      msg: format!($($rest),+)
    }
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
