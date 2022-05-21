use crate::{
  common::Buffered,
  errors::ParsingError,
  is_next,
  parseable::ParsingContext,
  token::Token,
};

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

#[macro_export]
macro_rules! parse_operand {
  ($errors: expr, $iter: expr, $ctx: expr) => {{
    use crate::{ast::Operator, errors::ParsingError, token::Token};
    Operator::parse(
      match $iter.peek() {
        Some(t @ Token::RParenthesis) => {
          $errors.push(ParsingError::UnexpectedParens);
          vec![t]
        },
        Some(t @ Token::RBrace) => {
          $errors.push(ParsingError::UnexpectedBrace);
          vec![t]
        },
        Some(t @ Token::RBracket) => {
          $errors.push(ParsingError::UnexpectedBracket);
          vec![t]
        },
        None => {
          return None;
        },
        Some(Token::For) => vec![Token::For, Token::In, Token::Colon],
        Some(Token::If) => vec![Token::If, Token::Colon],
        Some(Token::LParenthesis) => vec![Token::LParenthesis, Token::RParenthesis],
        Some(Token::LBrace) => vec![Token::LBrace, Token::RBrace],
        Some(Token::LBracket) => vec![Token::LBracket, Token::RBracket],
        Some(token) => vec![token],
      },
      $iter,
      $errors,
      $ctx,
    )
  }};
}

impl Operator {
  pub fn parse<T: Iterator<Item = Token>>(
    op: Vec<Token>,
    tokens: &mut Buffered<T>,
    errors: &mut Vec<ParsingError>,
    context: &mut ParsingContext,
  ) -> Option<Operator> {
    is_next!([skip] tokens, Token::NewLine);
    let mut operands = vec![];
    let mut iter = op.iter().peekable();

    if let Some(token) = iter.next() {
      match tokens.next() {
        Some(ref t) if t != token => {
          errors.push(ParsingError::EndOfExpression);
          return None;
        },
        Some(_) if iter.peek().is_none() => {
          return Some(Operator::Token(token.clone()));
        },
        _ => (),
      }
    }

    for token in iter {
      let mut operand = vec![];
      while tokens.peek().is_some_with(|x| x != token) {
        is_next!([skip] tokens, Token::NewLine);

        let res = parse_operand!(errors, tokens, context);
        if let Some(t) = res {
          operand.push(t);
        } else {
          errors.push(ParsingError::EndOfExpression);
          return None;
        }
        is_next!([skip] tokens, Token::NewLine);
      }
      if tokens.next().is_none() {
        errors.push(ParsingError::EndOfExpression);
        return None;
      }
      operands.push(operand);
    }

    Some(Operator::Operand { operands, op })
  }
}
