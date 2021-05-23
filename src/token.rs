#![allow(unused)]
use crate::common::*;
use std::hash::{Hash, Hasher};

#[path = "tests/token.rs"]
mod tests;

#[derive(Clone, Debug)]
pub struct Span<'a> {
  /// File which is spanned
  file: &'a str,

  /// Pos in file at which span begins
  pos: usize,

  /// Length of span in symbols
  length: usize,
}

impl<'a> std::fmt::Display for Span<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match std::fs::read(self.file) {
      Ok(file) => {
        let mut src = String::from_utf8(file).expect("Failed to parse as utf8 text");
        let mut stream = CharStream::from_string(src);
        stream.set_pos(self.pos);
        let line = stream.line();
        let column = stream.column();
        let line_src = match src.lines().skip(line).next() {
          Some(src) => src,
          None => {
            Logger::error("Failed to read line");
            return Err(std::fmt::Error);
          }
        };
        let mut underscore = format!("\u{001b}[{}C~", column as f64 - 0.5);
        for i in (1..self.length) {
          underscore += "\u{001b}[0.5C~";
        }

        write!(
          f,
          "line {}, column {} in file {}: \n{}| {}\n{}",
          line, column, self.file, line, line_src, underscore
        )
      }
      Err(_) => Err(std::fmt::Error),
    }
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Identifier(String),
  String(String),
  Number(f64),
  Boolean(bool),
  Char(char),
  None,
}

#[derive(Clone, Debug)]
pub struct TokenExt<'a> {
  pub token: Token,
  pub src: String,
  pub span: Span<'a>,
}

impl PartialEq for TokenExt<'_> {
  fn eq(&self, other: &Self) -> bool {
    self.token == other.token && self.value() == other.value()
  }
}

impl TokenExt<'_> {
  pub fn value(&self) -> Value {
    Value::None
  }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
  // Keywords
  Let,
  Return,
  Entry,
  Placeholder,

  // Punct
  LAngleBracket,
  LParenthesis,
  LBracket,
  LBrace,
  RAngleBracket,
  RParenthesis,
  RBracket,
  RBrace,
  Semicolon,
  Period,
  Colon,
  Comma,

  // Operators
  Pipe,
  Appersand,
  QuestionMark,
  Bang,
  Hash,
  Dollar,
  At,
  Add,
  Sub,
  Dec,
  Inc,
  Mult,
  Div,
  Pow,
  Mod,
  Equal,
  EqualEqual,
  LessEqual,
  GreaterEqual,

  // Literals
  Number,
  Boolean,
  Char,
  String,

  // Identifier
  Identifier,

  Skip,
}

impl<'a> Tokenizable<'a> for Token {
  fn tokenize(stream: &mut CharStream<'a>) -> Option<TokenExt<'a>> {
    use Token::*;
    let mut span = Span::<'a> {
      file: stream.file,
      pos: stream.pos(),
      length: 1,
    };

    Some(TokenExt {
      token: match stream.next()? {
        ' ' | '\t' | '\r' | '\n' => Skip,
        '<' => {
          if stream.is_next('=') {
            LessEqual
          } else {
            LAngleBracket
          }
        }
        '>' => {
          if stream.is_next('=') {
            GreaterEqual
          } else {
            RAngleBracket
          }
        }
        '(' => LParenthesis,
        ')' => RParenthesis,
        '{' => LBracket,
        '}' => RBracket,
        '[' => LBrace,
        ']' => RBrace,
        ';' => Semicolon,
        ':' => Colon,
        ',' => Comma,
        '|' => Pipe,
        '&' => Appersand,
        '?' => QuestionMark,
        '!' => Bang,
        '#' => Hash,
        '$' => Dollar,
        '@' => At,
        '+' => {
          if stream.is_next('=') {
            Inc
          } else {
            Add
          }
        }
        '-' => {
          if stream.is_next('=') {
            Dec
          } else {
            Sub
          }
        }
        '*' => Mult,
        '/' => {
          if stream.is_next('/') {
            while stream.is_not_next('\n') {}
            Skip
          } else if stream.is_next('*') {
            while stream.next() != Some('*') || stream.next() != Some('/') {
              println!("{:?}", stream.peek())
            }
            Skip
          } else {
            Div
          }
        }
        '^' => Pow,
        '%' => Mod,
        '=' => {
          if stream.is_next('=') {
            EqualEqual
          } else {
            Equal
          }
        }
        '\'' => {
          if stream.next() != None && stream.is_next('\'') {
            Char
          } else {
            Logger::error("Unexpected end of char literal");
            return None;
          }
        }
        '"' => {
          while stream.peek() != Some('"') {
            if stream.next() == None {
              Logger::error("Unexpected end of file");
              break;
            }
          }
          String
        }
        '.' => Period,

        c => {
          if c.is_ascii_digit() {
            while stream.check_next(|c| c.is_ascii_digit()) != None {}

            if stream.is_next('.') {
              while stream.check_next(|c| c.is_ascii_digit()) != None {}
            }

            Number
          } else if c.is_ascii_alphabetic() || c == '_' {
            if stream.check(|c| c.is_ascii_alphanumeric()) {
              while stream.check_next(|c| c.is_ascii_alphanumeric()) != None {}

              match stream.substring(span.pos, stream.pos()).as_str() {
                "entry" => Entry,
                "let" => Let,
                "return" => Return,
                "true" | "false" => Boolean,
                _ => Identifier,
              }
            } else {
              Placeholder
            }
          } else {
            Logger::error("Unexpected character");
            return None;
          }
        }
      },
      src: stream.substring(span.pos, {
        span.length = stream.pos() - span.pos;
        stream.pos()
      }),
      span,
    })
  }
}

pub trait Tokenizable<'a>
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut CharStream<'a>) -> Option<TokenExt<'a>>;
}

pub struct TokenStream<'a> {
  pub stream: ReversableStream<TokenExt<'a>>,
}

impl<'a> TokenStream<'a> {
  pub fn new(mut char_stream: CharStream<'a>) -> Option<TokenStream> {
    let mut tokens = vec![];
    let mut had_error = false;

    while char_stream.peek() != None {
      match Token::tokenize(&mut char_stream) {
        Some(TokenExt {
          token: Token::Skip,
          src: _,
          span: _,
        }) => continue,
        Some(token) => tokens.push(token),
        None => had_error = true,
      }
    }

    if had_error {
      None
    } else {
      Some(Self {
        stream: ReversableStream::<TokenExt>::new(tokens),
      })
    }
  }
}
