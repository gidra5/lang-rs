#![allow(unused)]
use crate::common::*;
use std::{
  default,
  fmt::{Debug, Display, Formatter},
  hash::{Hash, Hasher},
};

#[path = "tests/token.rs"]
mod tests;

#[macro_export]
macro_rules! token_pat {
  ($($token_ident:ident $(: $(@$id:ident)? $($token_pat:ident)|+)? $(, $src_ident:ident$(: $($src_pat:pat)|+)? )?)?) => {
    crate::token::TokenExt {
      $($token_ident $(: $($id @ )? ($(crate::token::Token::$token_pat)|+))?,
      $($src_ident $(: $($src_pat)|+)?, )?)?
      ..
    }
  };
}

#[macro_export]
macro_rules! match_token {
  ($({ $src:ident }, )? $(@$token_ident:ident)? $($pattern:ident)|+) => {
    Some(crate::token_pat!(token: $(@$token_ident)? $($pattern)|+ $(, $src)?))
  };
}

#[macro_export]
macro_rules! check_token {
  ($token:expr $(, { $src:ident })?, $($pattern:ident)|+ $(if $cond:expr)?) => {
    matches!($token, Some(crate::token::TokenExt {
      token: $(crate::token::Token::$pattern)|+,
      $(src: $src,)?
      ..
    }) $(if $cond)?)
  };
}

#[macro_export]
macro_rules! skip {
  ($token_stream:ident $(, { $src:ident })?, $($pattern:ident)|+ $(if $cond:expr)?) => {
    if crate::check_token!($token_stream.peek() $(, { $src })?, $($pattern)|+ $(if $cond)?) {
      $token_stream.next();
      true
    } else { false }
  };
}

#[macro_export]
macro_rules! check_token_end {
  ($stream:ident) => {
    matches!($stream.peek(), None)
  };
}
#[macro_export]
macro_rules! punct_or_newline {
  ($token:expr, $($punct:ident)|+) => {
    matches!($token, Some(crate::token::TokenExt { token: crate::token::Token::NewLine | $(crate::token::Token::$punct)|+, ..}) | None)
  };
}

#[derive(Clone, Default)]
pub struct TokenExt {
  pub token: Token,
  pub src:   String,
  pub span:  Span<CharStream>,
}

impl Debug for TokenExt {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("TokenExt")
      .field("token", &self.token)
      .field("src", &self.src)
      .finish()
  }
}


impl PartialEq for TokenExt {
  fn eq(&self, other: &Self) -> bool { self.token == other.token && self.value() == other.value() }
}
impl Eq for TokenExt {}

impl TokenExt {
  pub fn value(&self) -> Value {
    match self.token {
      Token::Number => Value::Number(self.src.parse::<f64>().unwrap()),
      Token::Boolean => match self.src.as_str() {
        "true" => Value::Boolean(true),
        "false" => Value::Boolean(false),
        _ => unreachable!(),
      },
      Token::String => Value::String(self.src[1..self.span.length - 1].to_string()),
      Token::Char => Value::Char(self.src.chars().nth(1).unwrap()),
      // Token::Placeholder => Value::Placeholder,
      _ => Value::None,
    }
  }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub enum Token {
  // Keywords
  Return,
  Entry,
  Import,
  Module,
  External,
  Public,
  Block,
  Infer,
  Infix,
  Postfix,
  Prefix,
  Placeholder,
  For,
  If,
  Else,
  Match,

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
  Arrow,
  Spread,
  Enum,

  Is,
  In,
  As,
  Async,
  Await,
  Inline,
  Type,

  // Literals
  Number,
  Boolean,
  Char,
  String,

  // Identifier
  Identifier,

  // Artificial
  #[default]
  Skip,
  Apply,
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Token::Placeholder => write!(f, "_"),
      token => write!(f, "{:?}", token),
    }
  }
}

impl Display for TokenExt {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      token_pat!(token: Identifier | String | Number | Boolean | Char, src) => write!(f, "{}", src),
      token_pat!(token) => write!(f, "{}", token),
    }
  }
}

impl Tokenizable for Token {
  fn tokenize(stream: &mut CharStream) -> Result<TokenExt, TokenizationError> {
    use Token::*;
    let stream_snapshot = stream.clone();

    let token = (|| {
      Ok(match stream.next().ok_or("Unexpected end of stream")? {
        ' ' | '\t' | '\r' => Skip,
        '\n' => {
          while let Some(' ' | '\t' | '\r' | '\n') = stream.peek() {
            stream.next();
          }
          NewLine
        },
        '<' if stream.is_next('=') => LessEqual,
        '<' => LAngleBracket,
        '>' if stream.is_next('=') => GreaterEqual,
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
        '+' if stream.is_next('+') => Inc,
        '+' => Add,
        '-' if stream.is_next('-') => Dec,
        '-' => Sub,
        '*' => Mult,
        '/' if stream.is_next('/') => {
          while stream.is_not_next('\n') {}
          Skip
        },
        '/' if stream.is_next('*') => {
          while stream.next() != Some('*') || stream.next() != Some('/') {}
          Skip
        },
        '/' => Div,
        '^' => Pow,
        '%' => Mod,
        '=' if stream.is_next('>') => Arrow,
        '=' if stream.is_next('=') => EqualEqual,
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
            return Err("Unexpected end of char literal");
          }
        },
        '"' => {
          while stream.peek() != Some('"') {
            if stream.peek() == None {
              return Err("Unexpected end of string");
            } else if stream.peek() == Some('\\') {
              stream.next();
            }
            stream.next();
          }
          stream.next();
          String
        },
        '.' if stream.is_next('.') && stream.is_next('.') => Spread,
        '.' => Period,
        c if c.is_ascii_digit() => {
          while stream.check_next(|c| c.is_ascii_digit()) != None {}

          if stream.is_next('.') {
            while stream.check_next(|c| c.is_ascii_digit()) != None {}
          }

          Number
        },
        c if c.is_ascii_alphabetic() || c == '_' => {
          if stream.check(|c| c.is_ascii_alphanumeric() || c == '_') {
            while stream.check_next(|c| c.is_ascii_alphanumeric() || c == '_') != None {}
          }

          match stream
            .substring(stream_snapshot.pos(), stream.pos())
            .as_str()
          {
            "entry" => Entry,
            "return" => Return,
            "use" => Import,
            "mod" => Module,
            "ext" => External,
            "pub" => Public,
            "infer" => Infer,
            "block" => Block,
            "match" => Match,
            "enum" => Enum,
            "is" => Is,
            "as" => As,
            "in" => In,
            "true" | "false" => Boolean,
            "infix" => Infix,
            "postfix" => Postfix,
            "prefix" => Prefix,
            "_" => Placeholder,
            "for" => For,
            "if" => If,
            "else" => Else,
            "async" => Async,
            "await" => Await,
            "inline" => Inline,
            _ => Identifier,
          }
        },
        _ => {
          return Err("Unexpected character");
        },
      })
    })();
    let span = Span {
      length: stream.pos() - stream_snapshot.pos(),
      stream: stream_snapshot,
    };

    match token {
      Ok(token) => Ok(TokenExt {
        token,
        src: span.string_src(),
        span,
      }),
      Err(msg) => Err(TokenizationError {
        span,
        msg: msg.to_string(),
      }),
    }
  }
}

pub struct TokenizationError {
  pub span: Span<CharStream>,
  pub msg:  String,
}

pub trait Tokenizable
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut CharStream) -> Result<TokenExt, TokenizationError>;
}

#[derive(Clone, Debug)]
pub struct TokenStream {
  pub stream: ReversableStream<TokenExt>,
}

impl TokenStream {
  pub fn new<L: LoggerTrait>(mut char_stream: CharStream, logger: &mut L) -> Option<TokenStream> {
    let mut tokens = vec![];
    let mut had_err = false;
    let mut err_info: Option<TokenizationError> = None;

    while char_stream.peek() != None {
      match Token::tokenize(&mut char_stream) {
        Ok(token_pat!(token: Skip)) => {},
        Ok(token) => {
          tokens.push(token);

          if let Some(e) = err_info {
            logger.error_token(e);
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
              logger.error_token(e2);
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
        logger.error_token(e);
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

impl ReversableIterator for TokenStream {
  type Item = TokenExt;

  fn next_ext(&mut self, size: usize) -> Vec<Option<Self::Item>> { self.stream.next_ext(size) }

  fn peek_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.peek_ext(size) }

  fn prev_ext(&self, size: usize) -> Vec<Option<Self::Item>> { self.stream.prev_ext(size) }

  fn pos(&self) -> usize { self.stream.pos() }
  fn backtrack(&mut self, size: usize) { self.stream.backtrack(size) }
}
