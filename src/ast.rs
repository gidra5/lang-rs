#![allow(unused)]
use crate::common::*;
use crate::token::*;
use futures::future::BoxFuture;
use futures::FutureExt;
use futures::executor::block_on;
use std::hash::{ Hash, Hasher };
use std::collections::HashMap;

#[path = "tests/ast.rs"]
mod tests;

/*
  Syntax definition:
  decl := let ident, [ident], "=", expr
  derivative_expr := "d[", ident, "]", expr
  fn_expr := ident, "(", expr, ")"
  expr := derivative_expr | summand, ["-"|"+", expr] | fn_expr
  summand := multiplier, ["*"|"/"|"^"|"%", summand]
  multiplier := number | ident | "(", expr, ")"

  enter expression to evaluate it or i to enter interactive mode
*/

/// Program := (Declaration | Expression)*
#[derive(Debug)]
pub struct Program(pub Vec<Either<Declaration, Expression>>);
pub struct ProgramError(Option<DeclarationError>);
impl std::fmt::Display for ProgramError {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    match &self.0 {
      Some(err) => write!(fmt, "{}", err),
      None => write!(fmt, "Expected one of: let, statement: \n\t{}", ExpressionError)
    }
  }
}

/// Declaration := let IdentifierDeclaration = Expression
#[derive(Debug)]
pub struct Declaration(pub IdentifierDeclaration, pub Expression);
impl Hash for Declaration {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.0.hash(state);
  }
}
impl PartialEq for Declaration {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}
impl Eq for Declaration {}
pub enum DeclarationError {
  // NoLet,
  NoIdentifier,
  IncorrectStatement
}
impl std::fmt::Display for DeclarationError {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    match self {
      NoIdentifier => write!(fmt, "Expected identifier"),
      IncorrectStatement => write!(fmt, "Expected expression: \n\t{}", ExpressionError)
    }
  }
}

/// IdentifierDeclaration := ident (: ident)?
#[derive(Debug)]
pub struct IdentifierDeclaration(pub Identifier, pub Option<Identifier>);
impl Hash for IdentifierDeclaration {
  fn hash<H: Hasher>(&self, state: &mut H) {
    self.0.hash(state);
    self.1.hash(state);
  }
}
impl PartialEq for IdentifierDeclaration {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

/// Expresson := Product ((+ | -) Expresson)?
#[derive(Debug)]
pub struct Expression(Product, Option<(Operator, Box<Expression>)>);

impl<'a> Expression {
  pub fn evaluate(&'a self, decls: &'a HashMap<Identifier, Expression>) -> BoxFuture<'a, f64> {
    async move {
      let left = self.0.evaluate(decls).await;

      if let Some((op, expr)) = &self.1 {
        let right = expr.evaluate(decls).await;
        match op {
          Operator::Add => left + right,
          Operator::Sub => left - right,
          _ => unreachable!()
        }
      } else { left }
    }.boxed()
  }
}

/// Product := Factor ((* | / | ^ | %) Factor)?
#[derive(Debug)]
pub struct Product(Factor, Option<(Operator, Box<Product>)>);

impl<'a> Product {
  pub fn evaluate(&'a self, decls: &'a HashMap<Identifier, Expression>) -> BoxFuture<'a, f64> {
    async move {
      let left = self.0.evaluate(decls).await;
  
      if let Some((op, expr)) = &self.1 {
        let right = expr.evaluate(decls).await;
        match op {
          Operator::Mult => left * right,
          Operator::Div => left / right,
          Operator::Pow => left.powf(right),
          Operator::Mod => left % right,
          _ => unreachable!()
        }
      } else { left }
    }.boxed()
  }
}

/// Factor := literal | ident | (Expression)
#[derive(Debug)]
pub enum Factor {
  Literal(Literal),
  // FunctionExpression(Identifier, Vec<Expression>),
  Identifier(Identifier),
  WrappedExpression(Box<Expression>)
}

impl<'a> Factor {
  pub fn evaluate(&'a self, decls: &'a HashMap<Identifier, Expression>) -> BoxFuture<'a, f64> {
    async move {
      match self {
        Factor::Literal(Literal::Number(num)) => num.parse().unwrap(), 
        Factor::WrappedExpression(expr) => expr.evaluate(decls).await,
        Factor::Identifier(ident) => {
          decls.get(ident).map(|x| block_on(x.evaluate(decls))).unwrap_or_else(|| { println!("No declared identifier {}", ident); 0. })
        },
        _ => todo!(), 
      }
    }.boxed()
  }
}

pub struct ExpressionError;
impl std::fmt::Display for ExpressionError {
  fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(fmt, "Expected one of: literal, identifier, '('")
  }
}

impl Parseable<Token> for Program {
  type ParsingError = ProgramError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let mut vec = vec![];

    while token_stream.peek() != None {
      let decl = token_stream.check(|stream| match stream.next() {
        Some(Token::Keyword(Keyword::Let)) => Ok(Declaration::parse(stream)),
        _ => Err(ProgramError(None))
      });

      match decl {
        Ok(Ok(decl)) => { vec.push(Either::Left(decl)); continue; },
        Ok(Err(err)) => { return Err(ProgramError(Some(err))) },
        Err(_) => (),
      }

      vec.push(Either::Right(match Expression::parse(token_stream) {
        Ok(parsed) => parsed,
        Err(err) => return Err(ProgramError(None)),
      }))
    }

    Ok(Self(vec))
  }
}

impl Parseable<Token> for Declaration {
  type ParsingError = DeclarationError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let ident = match token_stream.next() {
      Some(Token::Identifier(id)) => Ok(id),
      _ => Err(DeclarationError::NoIdentifier)
    }?;

    let constraint = token_stream.check(|stream| {
      match stream.next() {
        Some(Token::Punct(Punct::Colon)) => Ok(()),
        _ => Err(())
      }?;

      match stream.next() {
        Some(Token::Identifier(id)) => Ok(id),
        _ => Err(())
    }}).ok();
    let ident_decl = IdentifierDeclaration::parse(token_stream)?;

    let expression = match Expression::parse(token_stream) {
      Ok(parsed) => parsed,
      Err(_) => return Err(DeclarationError::IncorrectStatement),
    };

    Ok(Declaration(ident_decl, expression))
  }
}

impl Parseable<Token> for IdentifierDeclaration {
  type ParsingError = DeclarationError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let ident = match token_stream.next() {
      Some(Token::Identifier(id)) => Ok(id),
      _ => Err(DeclarationError::NoIdentifier)
    }?;

    let constraint = token_stream.check(|stream| {
      match stream.next() {
        Some(Token::Punct(Punct::Colon)) => Ok(()),
        _ => Err(())
      }?;

      match stream.next() {
        Some(Token::Identifier(id)) => Ok(id),
        _ => Err(())
    }}).ok();

    Ok(IdentifierDeclaration(ident, constraint))
  }
}

impl Parseable<Token> for Expression {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let product = Product::parse(token_stream)?;
    // println!("{:?}", token_stream.peek());
    // let rest = token_stream.check(|stream| match stream.next() {
    //     Some(Token::Operator(op @ Operator::Add)) 
    //   | Some(Token::Operator(op @ Operator::Sub)) => Ok((op, Expression::parse(stream))),
    //   _ => Err(ExpressionError::NoError)
    // }).ok().map(|(op, result)| match result {
    //   Ok(result) => Ok((op, Box::new(result))),
    //   Err(e) => Err(e),
    // }).transpose()?;

    let mut rest = None;
    if let Some(Token::Operator(op @ Operator::Add)) | Some(Token::Operator(op @ Operator::Sub)) = token_stream.peek() {
      token_stream.next();
      rest = Some((op, Box::new(Expression::parse(token_stream)?)))
    }

    Ok(Self(product, rest))
  }
}

impl Parseable<Token> for Product {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let factor = Factor::parse(token_stream)?;
    // let rest = token_stream.check(|stream| match stream.next() {
    //     Some(Token::Operator(op @ Operator::Add)) 
    //   | Some(Token::Operator(op @ Operator::Sub)) => Ok((op, Expression::parse(stream))),
    //   _ => Err(ExpressionError::NoError)
    // }).ok().map(|(op, result)| match result {
    //   Ok(result) => Ok((op, Box::new(result))),
    //   Err(e) => Err(e),
    // }).transpose()?;

    let mut rest = None;
    if let Some(Token::Operator(op @ Operator::Mult))
      | Some(Token::Operator(op @ Operator::Div))
      | Some(Token::Operator(op @ Operator::Pow))
      | Some(Token::Operator(op @ Operator::Mod)) = token_stream.peek() {
      token_stream.next();
      rest = Some((op, Box::new(Product::parse(token_stream)?)));
    }

    Ok(Self(factor, rest))
  }
}

impl Parseable<Token> for Factor {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    {
      let literal = token_stream.check(|stream| match stream.next() {
        Some(Token::Literal(literal)) => Ok(literal),
        _ => Err(ExpressionError)
      });
  
      match literal {
        Ok(literal) => return Ok(Factor::Literal(literal)),
        Err(_) => (),
      }
    }
    
    {
      let ident = match token_stream.next() {
        Some(Token::Identifier(id)) => Ok(id),
        _ => Err(ExpressionError)
      };
  
      match ident {
        Ok(ident) => return Ok(Factor::Identifier(ident)),
        Err(_) => (),
      }
    }

    token_stream.check(|stream| {
      match stream.next() {
        Some(Token::Punct(Punct::Parenthesis(BracketSide::Left))) => Ok(()),
        _ => Err(ExpressionError)
      }?;

      let expr = Expression::parse(stream);

      match stream.next() {
        Some(Token::Punct(Punct::Parenthesis(BracketSide::Right))) => Ok(()),
        _ => Err(ExpressionError)
      }?;

      expr
    }).map(|expr| Factor::WrappedExpression(Box::new(expr)))
  }
}