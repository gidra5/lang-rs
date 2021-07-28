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
    let err_token = self.stream.peek();
    let err_end_token = self.stream.peek_ext(self.length).pop().flatten();

    if let None = err_token {
      let prev_token = self.stream.prev();
      if let None = prev_token {
        return write!(f, "Empty stream???");
      }
      let TokenExt { span, .. } = prev_token.unwrap();

      return write!(f, "Unexpected end of stream at\n{}", span);
    }

    let err_token = err_token.unwrap();
    let err_end_token = err_end_token.unwrap_or(
      self
        .stream
        .peek_ext(self.length - 1)
        .pop()
        .flatten()
        .unwrap(),
    );

    let char_stream = err_token.span.stream;
    let src = char_stream.to_string();
    let char_length = err_end_token.span.pos() + err_end_token.span.length - char_stream.pos();

    let line = char_stream.line();
    let end_line = char_stream.line_from_pos(char_stream.pos() + char_length);

    let column = char_stream.column();
    let end_column = char_stream.column_from_pos(char_stream.pos() + char_length);

    let lines_str = src
      .lines()
      .enumerate()
      .skip(line)
      .take(end_line - line + 1)
      .map(|(i, line_src)| {
        let length = if i == line {
          line_src.len() - column
        } else if i == end_line {
          end_column
        } else {
          line_src.len()
        };
        let column = if i == line { column } else { 0 };
        let offset =
          (i as f64).log10().ceil() as usize + if char_stream.file == "." { 1 } else { 0 };
        let spaces = std::iter::repeat(" ").take(offset).collect::<String>();
        let underscore = format!(
          "{}|\u{001b}[{}C{}",
          spaces,
          column,
          std::iter::repeat("~").take(length).collect::<String>()
        );

        format!(" {}| {}\n {}", i + 1, line_src, underscore)
      })
      .collect::<Vec<String>>()
      .join("\n");

    match char_stream.file {
      "." => {
        write!(
          f,
          "line {}, column {}: \n{}",
          line + 1,
          column + 1,
          lines_str,
        )
      },
      file => {
        write!(
          f,
          "line {}, column {} in file {}: \n{}",
          line + 1,
          column + 1,
          file,
          lines_str
        )
      },
    }
    // write!(f, "todo")
  }
}
