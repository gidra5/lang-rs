use crate::token_pat;
use std::collections::HashMap;

#[path = "../tests/inline.rs"]
mod tests;

use super::{expr::Expression, expr_struct, RecordKey};

pub fn inline(pat: Expression, args: HashMap<String, Expression>) -> Expression {
  match pat {
    Expression::Value(token_pat!(token: Identifier, src: id)) if args.contains_key(&id) => {
      args[&id].clone()
    },
    Expression::Record(items) => {
      Expression::Record(
        items
          .into_iter()
          .map(|expr_struct::RecordItem { key, value }| {
            expr_struct::RecordItem {
              key:   match key {
                RecordKey::Value(value) => RecordKey::Value(inline(value, args.clone())),
                x => x,
              },
              value: inline(value, args.clone()),
            }
          })
          .collect(),
      )
    },
    Expression::Block(exprs) => {
      Expression::Block(
        exprs
          .into_iter()
          .map(|expr| inline(expr, args.clone()))
          .collect(),
      )
    },
    Expression::If(cond, t_b, f_b) => {
      Expression::If(
        Box::new(inline(*cond, args.clone())),
        Box::new(inline(*t_b, args.clone())),
        f_b.map(|f_b| Box::new(inline(*f_b, args))),
      )
    },
    Expression::For(pat, iter, body) => {
      Expression::For(
        pat,
        Box::new(inline(*iter, args.clone())),
        Box::new(inline(*body, args)),
      )
    },
    Expression::Prefix { op, right } => {
      Expression::Prefix {
        op:    Box::new(inline(*op, args.clone())),
        right: Box::new(inline(*right, args)),
      }
    },
    Expression::Postfix { left, op } => {
      Expression::Postfix {
        left: Box::new(inline(*left, args.clone())),
        op:   Box::new(inline(*op, args)),
      }
    },
    Expression::Infix {
      left,
      op: op @ box Expression::Value(token_pat!(token: Arrow)),
      right,
    } => Expression::Infix { left, op, right },
    Expression::Infix { left, op, right } => {
      Expression::Infix {
        left:  Box::new(inline(*left, args.clone())),
        op:    Box::new(inline(*op, args.clone())),
        right: Box::new(inline(*right, args)),
      }
    },
    x => x,
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
