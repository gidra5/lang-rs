pub use crate::{ast::*, common::*, token::*};

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
    // let stream = &self.stream;
    // let src = self.stream.to_string();

    // let line = self.stream.line();
    // let end_line = self.stream.line_from_pos(self.stream.pos() + self.length);

    // let column = self.stream.column();
    // let end_column = self.stream.column_from_pos(self.stream.pos() +
    // self.length);

    // let lines_str = src
    //   .lines()
    //   .enumerate()
    //   .skip(line)
    //   .take(end_line - line + 1)
    //   .map(|(i, line_src)| {
    //     let length = if i == line {
    //       line_src.len() - column
    //     } else if i == end_line {
    //       end_column
    //     } else {
    //       line_src.len()
    //     };
    //     let column = if i == line { column } else { 0 };

    //     let mut underscore = format!("|\u{001b}[{}C~", column);
    //     for i in
    //       (0..(i as f64).log10().ceil() as usize + if self.stream.file == "." { 1
    // } else { 0 })     {
    //       underscore = " ".to_string() + &underscore;
    //     }
    //     for i in (1..length) {
    //       underscore += "\u{001b}[0.5C~";
    //     }

    //     format!(" {}| {}\n {}", i + 1, line_src, underscore)
    //   })
    //   .collect::<Vec<String>>()
    //   .join("\n");

    // match self.stream.file {
    //   "." => {
    //     write!(
    //       f,
    //       "line {}, column {}: \n{}",
    //       line + 1,
    //       column + 1,
    //       lines_str,
    //     )
    //   },
    //   file => {
    //     write!(
    //       f,
    //       "line {}, column {} in file {}: \n{}",
    //       line + 1,
    //       column + 1,
    //       file,
    //       lines_str
    //     )
    //   },
    // }
    write!(f, "todo")
  }
}
