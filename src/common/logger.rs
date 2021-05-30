pub use crate::{common::*, ast::*, token::*};

pub struct Logger;
impl Logger {
  pub fn log(msg: &str) {
    println!("Log: {}", msg);
  }

  pub fn warning(msg: &str) {
    println!("Warning: {}", msg);
  }

  pub fn error(msg: &str) {
    println!("Error: {}", msg);
  }

  pub fn error_token(err: TokenizationError<'_>) {
    println!("Error: {} at\n{}", err.msg, err.span);
  }

  pub fn error_parse(err: ParsingError<'_>) {
    println!("Error: {}\n{}", err.span, err.msg);
  }
}


#[derive(Clone, Debug)]
pub struct Span<T: ReversableIterator> {
  /// Stream snapshot where occured error
  pub stream: T,

  /// Length of span in symbols
  pub length: usize,
}

impl<T: ReversableIterator> Span<T> {
  pub fn pos(&self) -> usize { self.stream.pos() }
}

impl<'a> std::fmt::Display for Span<TokenStream<'a>> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "todo")
  }
}
