use crate::token::*;
use crate::common::*;


/*
  Syntax definition:
  decl := ident, ["(", ident, ")"], "=", expr
  derivative_expr := "d[", ident, "]", expr
  fn_expr := ident, "(", expr, ")"
  expr := derivative_expr | summand, ["-"|"+", expr] | fn_expr
  summand := multiplier, ["*"|"/"|"^"|"%", summand]
  multiplier := number | ident | "(", expr, ")"

  enter expression to evaluate it or i to enter interactive mode
*/

/// Program := (Declaration | Statement)*
pub struct Program(Vec<Either<Declaration, Statement>>);

// Declaration := let DeclarationParameters (: DeclarationConstraint)? = Statement
/// Declaration := let ident (: ident)? = Statement
struct Declaration {
  // parameters: DeclarationParameters
  ident: Identifier,
  // constraint: Option<DeclarationConstraint>
  constraint: Option<Identifier>,
  statement: Statement
}

/// DeclarationConstraint := Expression
// struct DeclarationConstraint {
// }

/// DeclarationParameters := Argument? ident Argument?
// struct DeclarationParameters {
//   ident: Token::Identifier
//   arguments: (Option<Argument>, Option<Argument>)
// }

/// Argument := ident (: DeclarationConstraint)? | Pattern
// struct Argument(Either<(Token::Identifier, Option<DeclarationConstraint>), Pattern>)

/// Statement := { Statement+ } | Expression
enum Statement {
  BlockStatement(Vec<Statement>), 
  Expression(Expression)
}

/// Either function call, literal value or wrapped expression
/// 
/// Expresson := ident Expression* | literal | (Expression)
struct Expression {

}

impl Parseable<Token> for Program {
  type ParsingError = String;
  
  fn parse(
    token_stream: &mut TokenStream<Token>,
  ) -> Result<Self, Self::ParsingError> {
    todo!();
  }
}