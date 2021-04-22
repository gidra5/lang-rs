#![allow(unused)]
use crate::common::*;
use crate::token::*;

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

/// Program := (Declaration | Statement)*
pub type Program = Vec<Either<Declaration, Statement>>;

// Declaration := let DeclarationParameters (: DeclarationConstraint)? = Statement
/// Declaration := let ident (: ident)? = Statement
#[derive(Debug)]
pub struct Declaration {
  // parameters: DeclarationParameters
  ident: Identifier,
  // constraint: Option<DeclarationConstraint>
  constraint: Option<Identifier>,
  statement: Statement,
}

pub enum DeclarationError {
  NoLet,
  NoIdentifier,
  IncorrectStatement(StatementError)
}

impl From<StatementError> for DeclarationError {
  fn from(err: StatementError) -> DeclarationError { 
    DeclarationError::IncorrectStatement(err)
  }
}

// DeclarationConstraint := Expression
// struct DeclarationConstraint {
// }

// DeclarationParameters := Argument? ident Argument?
// struct DeclarationParameters {
//   ident: Token::Identifier
//   arguments: (Option<Argument>, Option<Argument>)
// }

// Argument := ident (: DeclarationConstraint)? | Pattern
// struct Argument(Either<(Token::Identifier, Option<DeclarationConstraint>), Pattern>)

// Statement := Expression | { Program Statement }
/// Statement := Expression 
#[derive(Debug)]
pub enum Statement {
  // BlockStatement(Program, Box<Statement>),
  Expression(Expression),
}

pub enum StatementError {
  NoOpeningBracket,
  NoClosingBracket,
  IncorrectExpression(ExpressionError)
}

impl From<ExpressionError> for StatementError {
  fn from(err: ExpressionError) -> StatementError { 
    StatementError::IncorrectExpression(err)
  }
}

/// Expresson := Product ((+ | -) Product)*
#[derive(Debug)]
pub struct Expression(Product, Vec<(Operator, Box<Expression>)>);

/// Product := Factor ((* | / | ^ | %) Factor)?
#[derive(Debug)]
pub struct Product(Factor, Option<(Operator, Box<Product>)>);

/// Factor := literal | ident Expression* | (Expression)
#[derive(Debug)]
pub enum Factor {
  Literal(Literal),
  FunctionExpression(Identifier, Vec<Expression>),
  WrappedExpression(Box<Expression>)
}

pub enum ExpressionError {
  NoOpeningParenthesis,
  NoClosingParenthesis,
  IncorrectFunctionExpression,
  NoSuchLiteral
}

impl Parseable<Token> for Declaration {
  type ParsingError = DeclarationError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    token_stream.check(|stream| {
      match stream.next() {
        Some(Token::Keyword(Keyword::Let)) => Ok(()),
        _ => Err(DeclarationError::NoLet)
      }?;

      let ident = match stream.next() {
        Some(Token::Identifier(id)) => Ok(id),
        _ => Err(DeclarationError::NoIdentifier)
      }?;

      let constraint = stream.check(|stream| {
        match stream.next() {
          Some(Token::Punct(Punct::Colon)) => Ok(()),
          _ => Err(())
        }?;

        match stream.next() {
          Some(Token::Identifier(id)) => Ok(id),
          _ => Err(())
      }}).ok();

      let statement = stream.check(Statement::parse)?;

      Ok(Declaration {
        ident,
        constraint,
        statement
      })
    })
  }
}

impl Parseable<Token> for Statement {
  type ParsingError = StatementError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    // match Expression::parse(token_stream) {
    //   Ok(parsed) => Ok(Self::Expression(parsed)),
    //   Err(err) => {
    //     match stream.next() {
    //       Some(Token::Punct(Punct::Bracket(BracketSide::Left))) => Ok(()),
    //       _ => Err(StatementError::NoLet)
    //     }?;
    // 
    //     Err(StatementError::IncorrectExpression(err))
    //   }
    // }
    Ok(Self::Expression(Expression::parse(token_stream)?))
  }
}

impl Parseable<Token> for Expression {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let product = Product::parse(token_stream)?;
    let mut rest = vec![];

    loop {
      let token = token_stream.check(|stream| match stream.next() {
        Some(Token::Operator(op @ Operator::Add)) | Some(Token::Operator(op @ Operator::Sub)) => Ok(op),
        _ => Err(())
      }).ok();

      if let Some(op) = token {
        rest.push((op, Box::new(Expression::parse(token_stream)?)))
      } else { break; }
    }

    Ok(Self(product, rest))
  }
}


impl Parseable<Token> for Product {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
    let product = Factor::parse(token_stream)?;
    let rest = token_stream.check(|stream| match stream.next() {
        Some(Token::Operator(op @ Operator::Mult))
      | Some(Token::Operator(op @ Operator::Div))
      | Some(Token::Operator(op @ Operator::Pow))
      | Some(Token::Operator(op @ Operator::Mod)) => Ok(op),
      _ => Err(())
    }).ok();

    if let Some(op) = rest {
      Ok(Self(product, Some((op, Box::new(Product::parse(token_stream)?)))))
    } else { Ok(Self(product, None)) }
  }
}

impl Parseable<Token> for Factor {
  type ParsingError = ExpressionError;

  fn parse(token_stream: &mut TokenStream) -> Result<Self, Self::ParsingError> {
      let literal = token_stream.check(|stream| match stream.next() {
        Some(Token::Literal(literal)) => Ok(literal),
        _ => Err(ExpressionError::NoSuchLiteral)
      });
  
      match literal {
        Ok(literal) => return Ok(Factor::Literal(literal)),
        Err(_) => (),
      }
      
      let inner_expr = token_stream.check(|stream| {
        match stream.next() {
          Some(Token::Punct(Punct::Parenthesis(BracketSide::Left))) => Ok(()),
          _ => Err(ExpressionError::NoOpeningParenthesis)
        }?;
  
        let expr = Expression::parse(stream);
  
        match stream.next() {
          Some(Token::Punct(Punct::Parenthesis(BracketSide::Right))) => Ok(()),
          _ => Err(ExpressionError::NoClosingParenthesis)
        }?;
  
        expr
      });
  
      match inner_expr {
        Ok(expr) => return Ok(Factor::WrappedExpression(Box::new(expr))),
        Err(_) => (),
      };
  
      let ident = match token_stream.next() {
        Some(Token::Identifier(id)) => Ok(id),
        _ => Err(ExpressionError::IncorrectFunctionExpression)
      }?;
  
      Ok(Factor::FunctionExpression(ident, Vec::<Expression>::parse(token_stream).unwrap()))
  }
}