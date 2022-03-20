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

  fn error_parse(&mut self, (error, span): (ParsingError, Span<TokenStream>)) {
    match error {
      ParsingError::Generic(msg) => Self::error(self, format!("{}\n\n{}", span, msg).as_str()),
      ParsingError::Aggregate(errs) => errs
        .into_iter()
        .for_each(|x| self.error_parse((x, span.clone()))),
    }
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

#[derive(Clone, Debug, Default)]
pub struct Span<T: ReversableIterator> {
  /// Stream snapshot where occured error
  pub stream: T,

  /// Length of span in symbols
  pub length: usize,
}

impl<T: ReversableIterator> Span<T> {
  pub fn pos(&self) -> usize { self.stream.pos() }
  pub fn src(&self) -> Vec<Option<<T>::Item>> { self.stream.peek_ext(self.length) }
}

impl Span<CharStream> {
  pub fn string_src(&self) -> String {
    self
      .src()
      .iter()
      .map(|x| match x {
        Some(y) => y.to_string(),
        None => "".to_string(),
      })
      .collect()
  }
}

impl std::fmt::Display for Span<TokenStream> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let err_token = self.stream.peek();
    let err_end_token = self.stream.peek_ext(self.length).pop().flatten();

    if err_token.is_none() {
      let prev_token = self.stream.prev();
      if prev_token.is_none() {
        return write!(f, "Empty stream???");
      }
      let TokenExt { span, .. } = prev_token.unwrap();

      return write!(f, "Unexpected end of stream at\n{}", span);
    }

    let err_token = err_token.unwrap();
    let err_end_token = err_end_token.unwrap_or_else(|| {
      self
        .stream
        .peek_ext(self.length - 1)
        .pop()
        .flatten()
        .unwrap()
    });

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

    match char_stream.file.as_str() {
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
