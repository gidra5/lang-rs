use crate::{
  common::buffered_iterator::Buffered,
  enviroment::Enviroment,
  errors::{ParsingError, RuntimeError},
  is_next,
  parse_error,
  parseable::Parseable,
  runtime_error,
  value::{Evaluatable, Value},
};
use std::{
  fmt::{Debug, Display, Formatter},
  hash::Hash,
};

// #[path = "tests/token.rs"]
// mod tests;

#[derive(Clone)]
pub struct TokenizationInput<T>
where
  T: Iterator + Clone,
  T::Item: Clone,
{
  pub iter:   Buffered<T>,
  pub errors: Vec<ParsingError>,
}

impl<T> Iterator for TokenizationInput<T>
where
  T: Iterator + Clone,
  T::Item: Clone,
{
  type Item = T::Item;
  fn next(&mut self) -> Option<Self::Item> { self.iter.next() }
}

impl<T> TokenizationInput<T>
where
  T: Iterator + Clone,
  T::Item: Clone,
{
  pub fn new(iter: Buffered<T>) -> Self {
    Self {
      iter,
      errors: vec![],
    }
  }
}

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
  Number(i64, u64),
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

impl<T: Iterator<Item = char> + Clone> Parseable<TokenizationInput<T>> for Token {
  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    let TokenizationInput {
      mut iter,
      mut errors,
    } = input;
    use Token::*;

    let character = iter.next();

    if let None = character {
      return (TokenizationInput { iter, errors }, None);
    }
    let res = match character.unwrap() {
      ' ' | '\t' | '\r' => Skip,
      '\n' => {
        while is_next!([skip] iter, (' ' | '\t' | '\r' | '\n')) {}
        NewLine
      },
      '<' if is_next!([skip] iter, '=') => LessEqual,
      '<' => LAngleBracket,
      '>' if is_next!([skip] iter, '=') => GreaterEqual,
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
      '+' if is_next!([skip] iter, '+') => Inc,
      '+' => Add,
      '-' if is_next!([skip] iter, '-') => Dec,
      '-' => Sub,
      '*' => Mult,
      '/' if is_next!([skip] iter, '/') => {
        while iter.peek().is_some() && is_next!([skip not] iter, '\n') {}
        Skip
      },
      '/' if is_next!([skip] iter, '*') => {
        while is_next!([skip not] iter[2], '*', '/') {}
        Skip
      },
      '/' => Div,
      '^' => Pow,
      '%' => Mod,
      '=' if is_next!([skip] iter, '>') => Arrow,
      '=' if is_next!([skip] iter, '=') => EqualEqual,
      '=' => Equal,
      '\'' => loop {
        if let [Some(c), Some('\'')] = iter.next_ext(2)[..] {
          break Char(c);
        }
        errors.push(parse_error!("Unterminated char literal"));
        return (TokenizationInput { iter, errors }, None);
      },
      '"' => {
        let mut src = "".to_string();
        while is_next!([not skip] iter, '"') {
          if let Some(c) = iter.next() {
            src += &c.to_string()
          } else {
            errors.push(parse_error!("Unterminated string literal"));
            return (TokenizationInput { iter, errors }, None);
          }
        }
        String(src)
      },
      '.' if is_next!([skip] iter[2], '.', '.') => Spread,
      '.' => Period,
      c if c.is_ascii_digit() => {
        let mut num = c.to_string().parse::<i64>().unwrap();
        let mut fract = 0_u64;
        while let Some(c) = iter.peek() {
          if let Ok(d) = c.to_string().parse::<i64>() {
            num = 10 * num + d;
            iter.next();
          } else {
            break;
          }
        }

        if is_next!([skip] iter, '.') {
          while let Some(c) = iter.peek() {
            if let Ok(d) = c.to_string().parse::<u64>() {
              fract = 10 * fract + d;
              iter.next();
            } else {
              break;
            }
          }
        }

        Number(num, fract)
      },
      c if c.is_ascii_alphabetic() || c == '_' => {
        let mut src = c.to_string();
        if let Some(c) = iter.peek() {
          if (c.is_ascii_alphanumeric() || c == '_') {
            while let Some(c) = iter.peek() {
              if !(c.is_ascii_alphanumeric() || c == '_') {
                break;
              }
              src += &c.to_string();
              iter.next();
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
          "true" => Boolean(true),
          "false" => Boolean(false),
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
        errors.push(parse_error!("Unrecognized input"));
        return (TokenizationInput { iter, errors }, None);
      },
    };

    (TokenizationInput { iter, errors }, Some(res))
  }
}
