use itertools::Itertools;

use crate::{ast::*, common::*, token::*};

pub trait LoggerTrait {
  fn write(&mut self, msg: String) {
    println!("{}", msg);
  }

  fn log(&mut self, msg: &str) { Self::write(self, format!("Log: {}", msg)); }

  fn warning(&mut self, msg: &str) { Self::write(self, format!("Warning: {}", msg)); }

  fn error(&mut self, msg: &str) { Self::write(self, format!("Error: {}", msg)); }

  fn error_token(&mut self, TokenizationError { msg, span }: TokenizationError) {
    Self::error(self, format!("{} at\n{}", msg, span).as_str());
  }
}

pub struct Logger {
  pub logs: Vec<String>,
}

impl LoggerTrait for Logger {
  fn write(&mut self, msg: String) {
    self.logs.push(msg.clone());

    println!("{}", msg);
  }
}
