use crate::{ast::ParsingInput, parse_error, parseable::Parseable, token::Token};

use super::Operator;

#[macro_export]
macro_rules! get_number {
  ($token_stream: ident, $def:literal) => {
    $token_stream
      .next()
      .and_then(|x| {
        if let token_pat!(token: Number, src) = x {
          Some(src.parse::<u8>().unwrap())
        } else {
          None
        }
      })
      .unwrap_or($def)
  };
}

impl<T: Iterator<Item = Token>> Parseable<ParsingInput<T>> for Operator {
  fn parse(mut input: ParsingInput<T>) -> (ParsingInput<T>, Option<Self::O>) {
    match input.tokens.peek() {
      Some(Token::For) => Operator::parse(vec![Token::For, Token::In, Token::Colon], input),
      Some(Token::If) => Operator::parse(vec![Token::If, Token::Colon], input),
      Some(Token::LParenthesis) => {
        Operator::parse(vec![Token::LParenthesis, Token::RParenthesis], input)
      },
      Some(Token::LBrace) => Operator::parse(vec![Token::LBrace, Token::RBrace], input),
      Some(Token::LBracket) => Operator::parse(vec![Token::LBracket, Token::RBracket], input),
      Some(token) => {
        input.tokens.next();
        (input, Some(Operator::Token(token)))
      },
      None => {
        input
          .errors
          .push(parse_error!("Unexpected end of expression"));
        return (input, None);
      },
    }
  }
}


impl Operator {
  pub fn parse<T: Iterator<Item = Token>>(
    op: Vec<Token>,
    mut input: ParsingInput<T>,
  ) -> (ParsingInput<T>, Option<Operator>) {
    if let Some(Token::NewLine) = input.tokens.peek() {
      input.tokens.next();
    }
    let mut operands = vec![];
    let mut iter = op.iter();

    if let Some(token) = iter.next() {
      match input.tokens.next() {
        Some(ref t) if t != token => {
          input
            .errors
            .push(parse_error!("Unexpected end of expression"));
          return (input, None);
        },
        _ => (),
      }
    }

    for token in iter {
      let mut operand = vec![];
      while input.tokens.peek().is_some_with(|x| x != token) {
        if let Some(Token::NewLine) = input.tokens.peek() {
          input.tokens.next();
        }
        let (mut i, res) = <Operator as Parseable<_>>::parse(input);
        if let Some(t) = res {
          operand.push(t);
          input = i;
        } else {
          i.errors.push(parse_error!("Unexpected end of expression"));
          return (i, None);
        }
      }
      if let None = input.tokens.next() {
        input
          .errors
          .push(parse_error!("Unexpected end of expression"));
        return (input, None);
      }
      operands.push(operand);
    }

    (input, Some(Operator::Operand { operands, op }))
  }
}
