#![allow(unused)]
use crate::common::*;
use std::hash::{Hash, Hasher};

#[path = "tests/token.rs"]
mod tests;

#[derive(Clone, Debug)]
pub struct Span<'a> {
  /// Stream snapshot where occured error
  stream: CharStream<'a>,

  /// Length of span in symbols
  length: usize,
}

impl Span<'_> {
  pub fn pos(&self) -> usize {
    self.stream.pos()
  }
}

impl<'a> std::fmt::Display for Span<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let stream = &self.stream;
    let src = stream.to_string();
    let line = stream.line();
    let column = stream.column();
    let line_src = match src.lines().nth(line) {
      Some(src) => src,
      None => {
        Logger::error("Failed to read line");
        return Err(std::fmt::Error);
      }
    };
    let mut underscore = format!(
      "\u{001b}[{}C~",
      column + 4 + (line as f64).log10().ceil() as usize
    );
    for i in (1..self.length) {
      underscore += "\u{001b}[0.5C~";
    }

    match self.stream.file {
      "." => write!(
        f,
        "line {}, column {}: \n {}| {}\n{}",
        line + 1,
        column,
        line + 1,
        line_src,
        underscore
      ),
      file => write!(
        f,
        "line {}, column {} in file {}: \n {}| {}\n{}",
        line + 1,
        column + 1,
        file,
        line + 1,
        line_src,
        underscore
      ),
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
  NewLine,
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
  fn tokenize(stream: &mut CharStream<'a>) -> Result<TokenExt<'a>, TokenizationError<'a>> {
    use Token::*;
    let mut span = Span {
      stream: stream.clone(),
      length: 1,
    };
    let mut msg = "";
    let token = (|| {
      Some(match stream.next()? {
        ' ' | '\t' | '\r' | '\n' => Skip,
        // ' ' | '\t' | '\r' => Skip,
        // '\n' => NewLine,
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
          if stream.is_next('+') {
            Inc
          } else {
            Add
          }
        }
        '-' => {
          if stream.is_next('-') {
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
            msg = "Unexpected end of char literal";
            return None;
          }
        }
        '"' => {
          while stream.peek() != Some('"') {
            if stream.next() == None {
              msg = "Unexpected end of string literal";
              return None;
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

              match stream.substring(span.pos(), stream.pos()).as_str() {
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
            msg = "Unexpected character";
            return None;
          }
        }
      })
    })();
    span.length = stream.pos() - span.pos();

    let token = match token {
      Some(t) => t,
      None => {
        return Err(TokenizationError {
          span,
          msg: msg.to_string(),
        })
      }
    };

    Ok(TokenExt {
      token,
      src: stream.substring(span.pos(), stream.pos()),
      span,
    })
  }
}

pub struct TokenizationError<'a> {
  pub span: Span<'a>,
  pub msg: String,
}
pub trait Tokenizable<'a>
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut CharStream<'a>) -> Result<TokenExt<'a>, TokenizationError<'a>>;
}

pub struct TokenStream<'a> {
  pub stream: ReversableStream<TokenExt<'a>>,
}

impl<'a> TokenStream<'a> {
  pub fn new(mut char_stream: CharStream<'a>) -> Option<TokenStream> {
    let mut tokens = vec![];
    let mut had_err = false;
    let mut err_info: Option<TokenizationError> = None;

    while char_stream.peek() != None {
      match Token::tokenize(&mut char_stream) {
        Ok(TokenExt {
          token: Token::Skip,
          src: _,
          span: _,
        }) => {}
        Ok(token) => {
          tokens.push(token);

          if let Some(e) = err_info {
            Logger::error_token(e);
            err_info = None;
          }
        }
        Err(e) => {
          had_err = true;
          if let Some(mut e2) = err_info {
            if e2.msg == e.msg {
              e2.span.length = 1 + e.span.stream.pos() - e2.span.stream.pos();
              err_info = Some(e2);
            } else {
              Logger::error_token(e2);
              err_info = Some(e);
            }
          } else {
            err_info = Some(e);
          }
        }
      }
    }

    if had_err {
      if let Some(e) = err_info {
        Logger::error_token(e);
        err_info = None;
      }
      None
    } else {
      Some(Self {
        stream: ReversableStream::<TokenExt>::new(tokens),
      })
    }
  }
}
