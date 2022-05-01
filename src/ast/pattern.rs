use crate::{
  enviroment::Enviroment,
  errors::ParsingError,
  parse_error,
  parseable::{Parseable, ParsingContext},
  token::Token,
  value::Value,
};

use super::{Expression, ParsingInput};

#[derive(Clone, PartialEq, Debug)]
pub enum RecordPatternKey {
  None,
  Rest,
  Identifier(String),
  Value(PatternBinder),
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordPatternItem {
  pub key:   RecordPatternKey,
  pub value: PatternBinder,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Pattern {
  Bind { name: String },
  Value(Value),
  Record(Vec<RecordPatternItem>),
}

impl<T: Iterator<Item = Token> + Clone> Parseable<ParsingInput<T>> for Pattern {
  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    let mut tokens = input.tokens;
    let mut errors = input.errors;
    let context = input.context;
    match tokens.peek() {
      Some(Token::Identifier(_)) => {
        let name = if let Some(Token::Identifier(src)) = tokens.peek() {
          tokens.next();
          src
        } else {
          errors.push(parse_error!("Expected name in pattern"));
          return (
            ParsingInput::<T> {
              tokens,
              errors,
              context,
            },
            None,
          );
        };

        (
          ParsingInput::<T> {
            tokens,
            errors,
            context,
          },
          Some(Pattern::Bind { name }),
        )
      },
      // _ => match Expression::parse(ParsingInput::<T> {
      //   tokens,
      //   errors,
      //   context,
      // }) {
      // (mut i, Some(expr)) => match Self::from_expr(&expr) {
      //   Ok(pat) => (i, Some(pat)),
      //   Err(e) => {
      //     i.errors.push(e);
      //     (i, None)
      //   },
      // },
      _ => Pattern::parse(ParsingInput::<T> {
        tokens,
        errors,
        context,
      }),
    }
  }
}

impl Pattern {
  pub fn from_expr(_expr: &Expression) -> Result<Self, ParsingError> { todo!() }
  pub fn _match(&self, _val: Value, _env: &mut Enviroment) -> bool { todo!() }
  pub fn bind(&self, _val: Value, _env: &mut Enviroment) { todo!() }
  pub fn context_bind(&self, _context: &mut ParsingContext, _bound_expr: &Expression) { todo!() }
}

#[derive(Clone, PartialEq, Debug)]
pub struct PatternBinder {
  pub pattern: Pattern,
  pub default: Option<Expression>,
  pub alias:   Option<String>,
}

impl<T: Iterator<Item = Token> + Clone> Parseable<ParsingInput<T>> for PatternBinder {
  fn parse(input: Self::I) -> (Self::I, Option<Self::O>) {
    (input, None)
    // let (input, pattern) = Pattern::parse(input);
    // let mut tokens = input.tokens;
    // let mut errors = input.errors;
    // let mut context = input.context;

    // let alias = if let [Some(Token::At), Some(Token::Identifier(src))] =
    // tokens.peek_ext(2)[..] {   tokens.next_ext(2);
    //   Some(src)
    // } else {
    //   None
    // };
    // let default = if let Some(Equal) = tokens.peek() {
    //   tokens.next();
    //   // Some(Expression::parse(ParsingInput::<T> {
    //   //   tokens,
    //   //   errors,
    //   //   context,
    //   // }))
    // } else {
    //   None
    // };

    // PatternBinder {
    //   pattern,
    //   default,
    //   alias,
    // }
  }
  // fn parse(stream: &mut TokenStream, context: &mut ParsingContext) ->
  // Result<Self, ParsingError> {   let pattern = Pattern::parse(stream,
  // context)?;

  //   let alias = if let [Some(Token::At), Some(Token::Identifier(src))] =
  // stream.peek_ext(2)[..] {     stream.next_ext(2);
  //     Some(src)
  //   } else {
  //     None
  //   };
  //   let default = if let Some(Equal) = stream.peek() {
  //     stream.next();
  //     Some(Expression::parse(stream, context)?)
  //   } else {
  //     None
  //   };

  //   Ok(PatternBinder {
  //     pattern,
  //     default,
  //     alias,
  //   })
  // }
}
