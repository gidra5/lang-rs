
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
