#![allow(unused)]
use crate::common::*;

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
        let mut stream = CharStream::from_string(String::from_utf8(file).expect("Failed to parse as utf8 text"));
        let mut line = 0;
        let mut column = 0;
    
        while let Some(c) = stream.next() {
          match c {
            '\n' => line += 1,
            _ => column += 1,
          }
        }
    
        write!(f, "line {}, column {}, len {} in file {}", line, column, self.length, self.file)
      },
      Err(_) => Err(std::fmt::Error)
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
  None
}

#[derive(Clone, Debug)]
pub struct TokenExt<'a, T: PartialEq>(pub T, pub String, pub Span<'a>);

impl<T: PartialEq> PartialEq for TokenExt<'_, T> {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

impl TokenExt<'_, Token> {
  fn value(&self) -> Value {
    Value::None
  }
}

#[derive(Clone, PartialEq, Debug)]
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
  fn tokenize(stream: &mut CharStream<'a>) -> Option<TokenExt<'a, Self>> {
    use Token::*;
    let mut span = Span::<'a> { file: stream.file, pos: stream.pos(), length: 1 };

    Some(TokenExt(match stream.next()? {
        ' ' | '\t' | '\r' | '\n' => Skip,
        '<' => if stream.is_next('=') { LessEqual } else { LAngleBracket },
        '>' => if stream.is_next('=') { GreaterEqual } else { RAngleBracket },
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
        '+' => if stream.is_next('=') { Inc } else { Add },
        '-' => if stream.is_next('=') { Dec } else { Sub },
        '*' => Mult,
        '/' => if stream.is_next('/') { while stream.is_not_next('\n') { } Skip } 
          else if stream.is_next('*') { while stream.next() != Some('*') || stream.next() != Some('/') { println!("{:?}", stream.peek()) } Skip } else { Div },
        '^' => Pow,
        '%' => Mod,
        '=' => if stream.is_next('=') { EqualEqual } else { Equal },
        '\'' => if stream.check_next(|c| c.is_ascii_alphabetic()) != None 
                && stream.is_next('\'') { Char } else { Logger::error(); return None },
        '"' => { while stream.next() != Some('"') { } String },
        '.' => if stream.check_next(|c| c.is_ascii_digit()) != None {
          while stream.check_next(|c| c.is_ascii_digit()) != None {}
          Number
        } else { Period },
        
        c => if c.is_ascii_digit() { 
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
              _ => Identifier
            }
          } else { Placeholder }
        } else { Logger::error(); return None },
      }, 
      stream.substring(span.pos, { span.length = stream.pos() - span.pos; stream.pos() }),
      span
    ))
  }
}

pub trait Tokenizable<'a>
where
  Self: Sized + Clone + PartialEq,
{
  fn tokenize(stream: &mut CharStream<'a>) -> Option<TokenExt<'a, Self>>;
}

pub struct TokenStream<'a> {
  pub stream: ReversableStream<TokenExt<'a, Token>>,
}

impl<'a> TokenStream<'a> {
  pub fn new(mut char_stream: CharStream<'a>) -> Option<TokenStream> {
    let mut tokens = vec![];
    let mut had_error = false;

    while char_stream.peek() != None {
      match Token::tokenize(&mut char_stream) {
        Some(TokenExt(Token::Skip, _, _)) => continue,
        Some(token) => tokens.push(token),
        None => had_error = true
      }
    } 

    if had_error { None } else {
      Some(Self { stream: ReversableStream::<TokenExt<Token>>::new(tokens) })
    }
  }
}