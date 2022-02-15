use std::collections::HashMap;
use crate::token_pat;

#[path = "../tests/inline.rs"]
mod tests;

use super::expr::{Expression, Op};

pub fn inline(pat: Expression, args: HashMap<String, Expression>) -> Expression {
  match pat {
    Expression {
      left: None,
      op: Op::Value(token_pat!(token: Identifier, src: id)),
      right: None,
    } if args.contains_key(&id) => args[&id].clone(),
    Expression { left, op, right } => 
      Expression {
        left: left.map(|x| Box::new(inline(*x, args.clone()))),
        op,
        right: right.map(|x| Box::new(inline(*x, args.clone()))),
      },
  }
}


#[macro_export]
macro_rules! inline_expr {
  ($logger:expr, $context:expr, $pat:literal, $([$name:expr]: $values:expr),*) => {{
    use crate::map;
    use crate::{token::TokenStream, common::CharStream, ast::{Expression, inline::inline, Parseable}};
    let mut stream = TokenStream::new(CharStream::from_str($pat), $logger)
      .ok_or("Failed to create TokenStream".to_string());
    let res = stream.map(|mut stream| 
      Expression::parse(&mut stream, $context).map(|expr| 
        inline(expr, map![$($name => $values),*])
      )
    );
    
    match res {
      Ok(x) => x,
      Err(x) => Err(crate::ast::ParsingError::Generic(x)), 
    }
  }};
  ($logger:expr, $context:expr, $pat:literal, $($name:ident: $values:expr),*) => {{
    use crate::map;
    use crate::{token::TokenStream, common::CharStream, ast::{Expression, inline::inline, Parseable}};
    let mut stream = TokenStream::new(CharStream::from_str($pat), $logger)
      .ok_or("Failed to create TokenStream".to_string());
    let res = stream.map(|mut stream| 
      Expression::parse(&mut stream, $context).map(|expr| 
        inline(expr, map![$(stringify!($name).to_string() => $values),*])
      )
    );

    match res {
      Ok(x) => x,
      Err(x) => Err(crate::ast::ParsingError::Generic(x)), 
    }
  }};
}
