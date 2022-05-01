use crate::{
  common::reversable_iterator::Buffered,
  enviroment::Enviroment,
  errors::RuntimeError,
  is_next,
  parseable::Parseable,
  runtime_error,
  value::{Evaluatable, Value},
};
use std::{
  fmt::{Debug, Display, Formatter},
  hash::Hash,
};

#[path = "tests/token.rs"]
mod tests;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Default)]
pub enum Token {
  // Keywords
  Return,
  Entry,
  Import,
  Module,
  External,
  Public,
  With,
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
  Number(i64),
  Boolean(bool),
  Char(char),
  String(String),

  // Identifier
  Identifier(String),

  // Artificial
  #[default]
  Skip,
  Apply,
}

impl Token {
  pub fn value(&self) -> Result<Value, RuntimeError> { self.evaluate(None).0 }
}

impl Display for Token {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Token::Placeholder => write!(f, "_"),
      token => write!(f, "{:?}", token),
    }
  }
}

impl Evaluatable for Token {
  fn evaluate(&self, env: Option<Enviroment>) -> (Result<Value, Self::E>, Option<Enviroment>) {
    (
      match self {
        // TODO: interpolation
        Token::String(val) => Ok(Value::String(val.clone())),
        _ => Err(runtime_error!("Not a value")),
      },
      env,
    )
  }
}

impl<T: Iterator<Item = char>> Parseable<Buffered<T>> for Token {
  fn parse(mut stream: Self::I) -> (Self::I, Option<Self::O>) {
    use Token::*;

    let character = stream.next();

    if let None = character {
      return (stream, None);
    }
    let res = match character.unwrap() {
      ' ' | '\t' | '\r' => Skip,
      '\n' => {
        while is_next!([skip] stream, (' ' | '\t' | '\r' | '\n')) {}
        NewLine
      },
      '<' if is_next!([skip] stream, '=') => LessEqual,
      '<' => LAngleBracket,
      '>' if is_next!([skip] stream, '=') => GreaterEqual,
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
      '+' if is_next!([skip] stream, '+') => Inc,
      '+' => Add,
      '-' if is_next!([skip] stream, '-') => Dec,
      '-' => Sub,
      '*' => Mult,
      '/' if is_next!([skip] stream, '/') => {
        while stream.peek().is_some() && is_next!([skip not] stream, '\n') {}
        Skip
      },
      '/' if is_next!([skip] stream, '*') => {
        while is_next!([skip not] stream[2], '*', '/') {}
        Skip
      },
      '/' => Div,
      '^' => Pow,
      '%' => Mod,
      '=' if is_next!([skip] stream, '>') => Arrow,
      '=' if is_next!([skip] stream, '=') => EqualEqual,
      '=' => Equal,
      '\'' => loop {
        if let [Some(c), Some('\'')] = stream.next_ext(2)[..] {
          break Char(c);
        }
        return (stream, None);
      },
      '"' => {
        let mut src = "".to_string();
        while !is_next!(stream, '"') {
          if let Some(c) = stream.next() {
            src += &c.to_string()
          } else {
            return (stream, None);
          }
        }
        String(src)
      },
      '.' if is_next!([skip] stream[2], '.', '.') => Spread,
      '.' => Period,
      c if c.is_ascii_digit() => {
        let mut num = c.to_string().parse::<i64>().unwrap();
        while let Some(c) = stream.peek() {
          if let Ok(d) = c.to_string().parse::<i64>() {
            num = 10 * num + d;
            stream.next();
          } else {
            break;
          }
        }

        Number(num)
      },
      c if c.is_ascii_alphabetic() || c == '_' => {
        let mut src = c.to_string();
        if let Some(c) = stream.peek() {
          if (c.is_ascii_alphanumeric() || c == '_') {
            while let Some(c) = stream.peek() {
              if !(c.is_ascii_alphanumeric() || c == '_') {
                break;
              }
              src += &c.to_string();
              stream.next();
            }
          }
        }

        match src.as_str() {
          "entry" => Entry,
          "return" => Return,
          "use" => Import,
          "with" => With,
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
          // "true" | "false" => Boolean,
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
          _ => Identifier(src),
        }
      },
      _ => {
        return (stream, None);
      },
    };

    (stream, Some(res))
  }
}
