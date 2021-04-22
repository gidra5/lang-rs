#[cfg(test)]
use super::*;

#[test]
fn not_tokenizes_empty_string() {
  let mut stream = ReversableStream::<char>::from("");
  assert_eq!(Token::tokenize(&mut stream), None);
}

#[test]
fn tokenizes() {
  let mut stream = ReversableStream::<char>::from("let x = -2 +7.; \"ok?\"");
  let mut token_stream = ReversableStream::<Token>::from(stream);
  assert_eq!(token_stream.next(), Some(Token::Keyword(Keyword::Let)));
  assert_eq!(
    token_stream.next(),
    Some(Token::Identifier("x".to_string()))
  );
  assert_eq!(token_stream.next(), Some(Token::Operator(Operator::Equal)));
  assert_eq!(token_stream.next(), Some(Token::Operator(Operator::Sub)));
  assert_eq!(
    token_stream.next(),
    Some(Token::Literal(Literal::Number("2".to_string())))
  );
  assert_eq!(token_stream.next(), Some(Token::Operator(Operator::Add)));
  assert_eq!(
    token_stream.next(),
    Some(Token::Literal(Literal::Number("7.".to_string())))
  );
  assert_eq!(token_stream.next(), Some(Token::Punct(Punct::Semicolon)));
  assert_eq!(
    token_stream.next(),
    Some(Token::Literal(Literal::String("ok?".to_string())))
  );
}

#[test]
fn tokenizes_keyword() {
  let mut stream = ReversableStream::<char>::from("let");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Keyword(Keyword::Let))
  );
  let mut stream = ReversableStream::<char>::from("return");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Keyword(Keyword::Return))
  );
  let mut stream = ReversableStream::<char>::from("entry");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Keyword(Keyword::Entry))
  );
  let mut stream = ReversableStream::<char>::from("_");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Keyword(Keyword::Placeholder))
  );
}

#[test]
fn tokenizes_literal() {
  let mut stream = ReversableStream::<char>::from("\"ok\"");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::String("ok".to_string())))
  );
  let mut stream = ReversableStream::<char>::from("'b'");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Char('b')))
  );
  let mut stream = ReversableStream::<char>::from("123");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Number("123".to_string())))
  );
  let mut stream = ReversableStream::<char>::from("123.");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Number("123.".to_string())))
  );
  let mut stream = ReversableStream::<char>::from("123.56");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Number("123.56".to_string())))
  );
  let mut stream = ReversableStream::<char>::from("true");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Boolean(true)))
  );
  let mut stream = ReversableStream::<char>::from("false");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Literal(Literal::Boolean(false)))
  );
}

#[test]
fn tokenizes_identifier() {
  let mut stream = ReversableStream::<char>::from("ok");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Identifier("ok".to_string()))
  );
  let mut stream = ReversableStream::<char>::from("a123_");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Identifier("a123_".to_string()))
  );
  let mut stream = ReversableStream::<char>::from("_a");
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Identifier("_a".to_string()))
  );
}

#[test]
fn tokenizes_punct() {
  let mut stream = ReversableStream::<char>::from("/**/<>(){}[]\n;//.:,");

  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::MultilineComment(BracketSide::Left)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::MultilineComment(BracketSide::Right)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::AngleBracket(BracketSide::Left)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::AngleBracket(BracketSide::Right)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Parenthesis(BracketSide::Left)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Parenthesis(BracketSide::Right)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Bracket(BracketSide::Left)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Bracket(BracketSide::Right)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Brace(BracketSide::Left)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Brace(BracketSide::Right)))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::EndOfLine))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Semicolon))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Comment))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Period))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Colon))
  );
  assert_eq!(
    Token::tokenize(&mut stream),
    Some(Token::Punct(Punct::Comma))
  );
}
