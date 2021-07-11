#![allow(unused)]
use crate::common::*;
use std::{
  fmt::Debug,
  hash::{Hash, Hasher},
};

#[path = "tests/token.rs"]
mod tests;

#[derive(Clone)]
pub struct TokenExt<'a> {
  pub token: Token,
  pub src:   String,
  pub span:  Span<CharStream<'a>>,
}


impl Debug for TokenExt<'_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("TokenExt")
      .field("token", &self.token)
      .field("src", &self.src)
      .finish()
  }
}


impl PartialEq for TokenExt<'_> {
  fn eq(&self, other: &Self) -> bool { self.token == other.token && self.value() == other.value() }
}
impl Eq for TokenExt<'_> {}

impl TokenExt<'_> {
  pub fn value(&self) -> Value {
    match self.token {
      Token::Number => Value::Number(self.src.parse::<f64>().unwrap()),
      Token::Boolean => {
        match self.src.as_str() {
          "true" => Value::Boolean(true),
          "false" => Value::Boolean(false),
          _ => unreachable!(),
        }
      },
      Token::String => Value::String(self.src[1..self.span.length - 1].to_string()),
      Token::Char => Value::Char(self.src.chars().nth(1).unwrap()),
      Token::Identifier => Value::Identifier(self.src.clone()),
      token
      @
      (Token::Pipe
      | Token::Appersand
      | Token::QuestionMark
      | Token::Bang
      | Token::Hash
      | Token::Dollar
      | Token::At
      | Token::Add
      | Token::Sub
      | Token::Dec
      | Token::Inc
      | Token::Mult
      | Token::Div
      | Token::Pow
      | Token::Mod
      | Token::Equal
      | Token::EqualEqual
      | Token::LessEqual
      | Token::GreaterEqual) => Value::Operator(token),
      _ => Value::None,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
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
  // NewLine,
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
        '<' => LAngleBracket,
        '>' => RAngleBracket,
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
        '+' => Add,
        '-' => Sub,
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
        },
        '^' => Pow,
        '%' => Mod,
        '=' => Equal,
        '\'' => {
          if stream.next() != None && stream.is_next('\'') {
            Char
          } else {
            while stream.peek() != Some('\'') {
              if stream.peek() == None {
                break;
              }
              stream.next();
            }
            msg = "Unexpected end of char literal";
            return None;
          }
        },
        '"' => {
          while stream.peek() != Some('"') {
            if stream.peek() == None {
              msg = "Unexpected end of string";
              return None;
            } else if stream.peek() == Some('\\') {
              stream.next();
            }
            stream.next();
          }
          stream.next();
          String
        },
        '.' => Period,

        c => {
          if c.is_ascii_digit() {
            while stream.check_next(|c| c.is_ascii_digit()) != None {}

            if stream.is_next('.') {
              while stream.check_next(|c| c.is_ascii_digit()) != None {}
            }

            Number
          } else if c.is_ascii_alphabetic() || c == '_' {
            if stream.check(|c| c.is_ascii_alphanumeric() || c == '_') {
              while stream.check_next(|c| c.is_ascii_alphanumeric() || c == '_') != None {}
            }

            match stream.substring(span.pos(), stream.pos()).as_str() {
              "entry" => Entry,
              "let" => Let,
              "return" => Return,
              "true" | "false" => Boolean,
              "_" => Placeholder,
              _ => Identifier,
            }
          } else {
            msg = "Unexpected character";
            return None;
          }
        },
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
      },
    };

    Ok(TokenExt {
      token,
      src: stream.substring(span.pos(), stream.pos()),
      span,
    })
  }
}

pub struct TokenizationError<'a> {
  pub span: Span<CharStream<'a>>,
  pub msg:  String,
}

pub trait Tokenizable<'a>
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut CharStream<'a>) -> Result<TokenExt<'a>, TokenizationError<'a>>;
}

#[derive(Clone, Debug)]
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
        }) => {},
        Ok(token) => {
          tokens.push(token);

          if let Some(e) = err_info {
            Logger::error_token(e);
            err_info = None;
          }
        },
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
        },
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

impl<'a> ReversableIterator for TokenStream<'a> {
  type Item = TokenExt<'a>;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> { self.stream.next_ext(size) }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.peek_ext(size) }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.prev_ext(size) }

  fn pos(&self) -> usize { self.stream.pos() }
}
