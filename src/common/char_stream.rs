pub use crate::common::*;

impl From<&str> for ReversableStream<char> {
  fn from(s: &str) -> Self { Self::new(s.chars().collect::<Vec<char>>()) }
}

impl From<String> for ReversableStream<char> {
  fn from(s: String) -> Self { Self::from(s.as_str()) }
}

#[derive(Debug, Clone)]
pub struct CharStream<'a> {
  stream:   ReversableStream<char>,
  pub file: &'a str,
}

impl CharStream<'_> {
  pub fn new(filename: &str) -> Result<CharStream, &str> {
    std::fs::read(filename)
      .map(|file| {
        CharStream {
          stream: ReversableStream::<char>::from(
            String::from_utf8(file).expect("Failed to parse as utf8 text"),
          ),
          file:   filename,
        }
      })
      .map_err(|err| {
        use std::io::ErrorKind::*;
        match err.kind() {
          NotFound => "No such file",
          PermissionDenied => {
            "Permission denied, maybe try running as administrator/sudo or add 'executable' flag"
          },
          _ => "Unexpected IO error",
        }
      })
  }

  pub fn from_string<'a>(s: String) -> CharStream<'a> {
    CharStream {
      stream: ReversableStream::<char>::from(s),
      file:   ".",
    }
  }

  pub fn from_str<'a>(s: &str) -> CharStream<'a> {
    CharStream {
      stream: ReversableStream::<char>::from(s),
      file:   ".",
    }
  }

  pub fn stream(&self) -> &ReversableStream<char> { &self.stream }

  pub fn line(&self) -> usize { self.line_from_pos(self.pos()) }

  pub fn line_from_pos(&self, pos: usize) -> usize {
    self
      .stream
      .data()
      .iter()
      .take(pos)
      .filter(|&&c| c == '\n')
      .count()
  }

  pub fn column(&self) -> usize { self.column_from_pos(self.pos()) }

  pub fn column_from_pos(&self, pos: usize) -> usize {
    let mut col = 0;
    self
      .stream
      .data()
      .iter()
      .take(pos)
      .for_each(|&c| if c == '\n' { col = 0 } else { col += 1 });
    col
  }

  pub fn substring(&self, left: usize, right: usize) -> String {
    self
      .stream
      .data()
      .iter()
      .skip(left)
      .take(right - left)
      .collect::<std::string::String>()
  }

  pub fn check<F: FnOnce(char) -> bool>(&self, f: F) -> bool {
    self.stream.peek().map(f) == Some(true)
  }
  pub fn check_next<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
    if self.check(f) {
      self.next()
    } else {
      None
    }
  }
}

impl std::fmt::Display for CharStream<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.stream.data().iter().collect::<std::string::String>()
    )
  }
}

impl ReversableIterator for CharStream<'_> {
  type Item = char;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> { self.stream.next_ext(size) }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.peek_ext(size) }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.prev_ext(size) }

  fn pos(&self) -> usize { self.stream.pos() }
}


impl<'a> std::fmt::Display for Span<CharStream<'a>> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let stream = &self.stream;
    let src = self.stream.to_string();

    let line = self.stream.line();
    let end_line = self.stream.line_from_pos(self.stream.pos() + self.length);

    let column = self.stream.column();
    let end_column = self.stream.column_from_pos(self.stream.pos() + self.length);

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

        let mut underscore = format!("|\u{001b}[{}C~", column);
        for i in
          (0..(i as f64).log10().ceil() as usize + if self.stream.file == "." { 1 } else { 0 })
        {
          underscore = " ".to_string() + &underscore;
        }
        for i in (1..length) {
          underscore += "\u{001b}[0.5C~";
        }

        format!(" {}| {}\n {}", i + 1, line_src, underscore)
      })
      .collect::<Vec<String>>()
      .join("\n");

    match self.stream.file {
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
  }
}
