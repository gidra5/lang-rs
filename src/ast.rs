#![allow(unused)]
use crate::{common::*, enviroment::Enviroment, *};
use std::{
  cell::RefCell,
  cmp::Ordering,
  fmt::{Display, Formatter},
  hash::{Hash, Hasher},
  rc::Rc,
};

#[path = "tests/ast.rs"]
mod tests;

/// matches error productions
// pub enum ErrorType {
//   Generic(String)
// }

#[derive(Debug)]
pub struct ParsingError<'a> {
  // pub error_type: ErrorType,
  pub span: Span<TokenStream<'a>>,
  pub msg:  String,
}

pub trait Parseable<'a>
where
  Self: Sized,
{
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String>;
  fn parse_ext(stream: &mut TokenStream<'a>) -> Result<ASTNodeExt<'a, Self>, ParsingError<'a>> {
    let mut span = Span {
      stream: stream.clone(),
      length: 1,
    };
    let res = Self::parse(stream);

    span.length = stream.pos() - span.pos();

    match res {
      Ok(node) => Ok(ASTNodeExt { node, span }),
      Err(msg) => Err(ParsingError { msg, span }),
    }
  }
}

pub trait Synchronizable<'a> {
  fn synchronize(stream: &mut TokenStream<'a>) {
    stream.next();
    while !Self::sync_point(stream) && stream.peek().is_some() {
      stream.next();
    }
  }

  fn sync_point(stream: &mut TokenStream<'a>) -> bool { false }
}

pub trait Evaluatable {
  fn evaluate(self, env: &mut Rc<RefCell<Enviroment>>) -> Value;
}
/*
  Syntax definition:

  enter expression to evaluate it or i to enter interactive mode
*/


#[derive(Clone, Debug)]
pub struct ASTNodeExt<'a, T> {
  pub node: T,
  pub span: Span<TokenStream<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
  left:  Option<Box<Expression>>,
  op:    Value,
  right: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Fixity {
  Prefix,
  Infix,
  Postfix,
  None,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Operator {
  fixity: Fixity,
  value:  Value,
}

impl Default for Operator {
  fn default() -> Operator {
    Operator {
      value:  Value::None,
      fixity: Fixity::None,
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let Self { left, op, right } = self;
    match (left, right) {
      (None, None) => write!(f, "{}", op),
      (Some(left), None) => write!(f, "({} {})", op, left),
      (None, Some(right)) => write!(f, "({} {})", op, right),
      (Some(left), Some(right)) => write!(f, "({} {} {})", op, left, right),
    }
  }
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

impl Evaluatable for Expression {
  fn evaluate(self, env: &mut Rc<RefCell<Enviroment>>) -> Value {
    match self {
      Expression {
        left: None,
        op: Value::Identifier(id),
        right: None,
      } => env.borrow().get(id).unwrap_or(Value::None),
      Expression {
        left: None,
        op,
        right: None,
      } => op,
      Expression {
        left: Some(left),
        op,
        right: None,
      } => op.postfix((*left).evaluate(env)),
      Expression {
        left: None,
        op,
        right: Some(right),
      } => op.prefix((*right).evaluate(env)),
      Expression {
        left: Some(left),
        op: op @ Value::Operator(Token::Equal),
        right: Some(right),
      } => {
        let left = *left;
        let right = (*right).evaluate(env);
        if let Expression {
          left: None,
          op: Value::Identifier(left),
          right: None,
        } = left
        {
          env.borrow_mut().set(left, right.clone());
          right
        } else {
          op.infix(left.evaluate(env), right)
        }
      },
      Expression {
        left: Some(left),
        op,
        right: Some(right),
      } => op.infix((*left).evaluate(env), (*right).evaluate(env)),
    }
  }
}

impl<'a> Parseable<'a> for Expression {
  fn parse(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
    let mut top = Frame {
      lhs:      None,
      operator: None,
    };
    let mut stack = Vec::new();
    loop {
      let token = token_stream.peek();
      let operator = loop {
        let operator = token
          .clone()
          .map(|token| Operator::new(token, top.lhs.is_none()))
          .flatten();
        match operator {
          // Some(None) => return Err("No such operator"),
          // Some(t @ Some(op)) if top.operator <= t => break op,
          Some(op) if top.operator <= Some(op.clone()) => break op,
          _ => {
            let res = top;
            if let Some(Expression {
              op: Value::Operator(Token::LParenthesis),
              left: None,
              right: Some(_),
            }) = res.lhs
            {
              return Err("Expected closing parenthesis".to_string());
            }

            top = match stack.pop() {
              Some(it) => it,
              None => return res.lhs.ok_or_else(|| "No expression".to_string()),
            };

            top.lhs = Some(Expression {
              op:    res.operator.unwrap().value,
              left:  top.lhs.map(Box::new),
              right: res.lhs.map(Box::new),
            });
          },
        };
      };
      token_stream.next();

      if let Operator {
        value: Value::Operator(Token::RParenthesis),
        fixity: Fixity::Postfix,
      } = operator
      {
        if let Some(Operator {
          value: Value::Operator(Token::LParenthesis),
          fixity: Fixity::Prefix,
        }) = top.operator
        {
          let res = top;
          top = stack.pop().unwrap();
          top.lhs = res.lhs;
          continue;
        } else {
          return Err("Unexpected closing parenthesis".to_string());
        }
      }

      stack.push(top);
      top = Frame {
        lhs:      None,
        operator: Some(operator),
      };
    }
  }
}

impl Operator {
  pub fn new(token: TokenExt, prefix: bool) -> Option<Self> {
    let op = Operator {
      value:  token.value(),
      fixity: if prefix {
        Fixity::Prefix
      } else {
        Fixity::Infix
      },
    };

    if op.exist() {
      return Some(op);
    }

    let op = Operator {
      fixity: if prefix {
        Fixity::None
      } else {
        Fixity::Postfix
      },
      ..op
    };

    if op.exist() {
      return Some(op);
    }

    None
  }

  fn exist(&self) -> bool {
    match self {
      Operator {
        value:
          Value::Identifier(_)
          | Value::String(_)
          | Value::Char(_)
          | Value::Number(_)
          | Value::Boolean(_),
        fixity: Fixity::None,
      } => true,
      Operator {
        value:
          Value::Operator(Token::LParenthesis)
          | Value::Operator(Token::Add)
          | Value::Operator(Token::Inc)
          | Value::Operator(Token::Dec)
          | Value::Operator(Token::Mult)
          | Value::Operator(Token::Sub),
        fixity: Fixity::Prefix,
      } => true,
      Operator {
        value: Value::Operator(Token::RParenthesis) | Value::Operator(Token::Bang),
        fixity: Fixity::Postfix,
      } => true,
      Operator {
        value:
          Value::Operator(Token::Period)
          | Value::Operator(Token::Equal)
          | Value::Operator(Token::EqualEqual)
          | Value::Operator(Token::Add)
          | Value::Operator(Token::Sub)
          | Value::Operator(Token::Mod)
          | Value::Operator(Token::Mult)
          | Value::Operator(Token::Div),
        fixity: Fixity::Infix,
      } => true,
      Operator {
        value: Value::Identifier(id),
        fixity: Fixity::Infix,
      } if id == "mod" => true,
      _ => false,
    }
  }

  fn bp(&self) -> Option<(u8, u8)> {
    Some(match self {
      Operator {
        value:
          Value::Identifier(_)
          | Value::String(_)
          | Value::Char(_)
          | Value::Number(_)
          | Value::Boolean(_),
        fixity: Fixity::None,
      } => (99, 100),
      Operator {
        value,
        fixity: Fixity::Prefix,
      } => {
        (99, match value {
          Value::Operator(token) => {
            match token {
              Token::LParenthesis => 0,
              Token::Add | Token::Sub => 9,
              Token::Inc | Token::Dec => 11,
              Token::Mult => 13,
              _ => return None,
            }
          },
          _ => return None,
        })
      },
      Operator {
        value,
        fixity: Fixity::Postfix,
      } => {
        (
          match value {
            Value::Operator(token) => {
              match token {
                Token::RParenthesis => 0,
                Token::Bang => 15,
                _ => return None,
              }
            },
            _ => return None,
          },
          100,
        )
      },
      Operator {
        value,
        fixity: Fixity::Infix,
      } => {
        match value {
          Value::Operator(token) => {
            match token {
              Token::Period => (18, 17),
              Token::Equal => (2, 1),
              Token::Add | Token::Sub => (5, 6),
              Token::Mult | Token::Div => (7, 8),
              Token::EqualEqual => (20, 19),
              _ => return None,
            }
          },
          Value::Identifier(id) if id == "mod" => (22, 21),
          _ => return None,
        }
      },
      _ => return None,
    })
  }
}

impl PartialOrd for Operator {
  fn partial_cmp(&self, other: &Operator) -> Option<Ordering> {
    let (_, r_bp1) = self.bp()?;
    let (l_bp2, _) = other.bp()?;

    Some(match (r_bp1 < l_bp2, r_bp1 > l_bp2) {
      (false, false) => Ordering::Equal,
      (true, false) => Ordering::Less,
      (false, true) => Ordering::Greater,
      _ => return None,
    })
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
  Print(Expression),
  Expression(Expression),
  Let(String, Option<Expression>),
  Block(Block),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(Vec<Statement>);

impl<'a> Parseable<'a> for Block {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let mut res = vec![];

    while !check_token!(stream, Token::RBracket) && !check_token_end!(stream) {
      res.push(Statement::parse(stream)?);
    }

    if let TokenExt {
      token: Token::RBracket,
      ..
    } = stream
      .next()
      .ok_or_else(|| "Missing closing bracket".to_string())?
    {
      Ok(Self(res))
    } else {
      Err("Missing closing bracket".to_string())
    }
  }
}

impl<'a> Parseable<'a> for Statement {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> {
    let TokenExt { token, src, span } = stream
      .peek()
      .ok_or_else(|| "Unexpected end of statement.".to_string())?;

    let res =
      match token {
        Token::Identifier if src == "print" => {
          stream.next();
          Self::Print(Expression::parse(stream)?)
        },
        Token::LBracket => {
          stream.next();

          Self::Block(Block::parse(stream)?)
        },
        Token::Let => {
          stream.next();

          let id = stream
            .next()
            .map(|token| {
              match token.token {
                Token::Identifier => Some(token.src),
                _ => None,
              }
            })
            .flatten()
            .ok_or_else(|| "Expected identifier at let statement.".to_string())?;

          if let TokenExt {
            token: Token::Equal,
            ..
          } = stream
            .peek()
            .ok_or_else(|| "Missing semicolon at the end of let statement".to_string())?
          {
            stream.next();
            Self::Let(
              id,
              Some(Expression::parse(stream).map_err(|err| {
                format!("Error at expression after '=' in let statement: {}", err)
              })?),
            )
          } else {
            Self::Let(id, None)
          }
        },
        _ => Self::Expression(Expression::parse(stream)?),
      };

    if let TokenExt {
      token: Token::Semicolon,
      ..
    } = stream
      .next()
      .ok_or_else(|| "Missing semicolon at the end of statement".to_string())?
    {
      Ok(res)
    } else {
      Err("Missing semicolon at the end of statement".to_string())
    }
  }
}

impl Evaluatable for Statement {
  fn evaluate(self, env: &mut Rc<RefCell<Enviroment>>) -> Value {
    match self {
      Self::Expression(expr) => {
        println!("{} = {}", expr.clone(), expr.evaluate(env))
      },
      Self::Print(expr) => println!("{}", expr.evaluate(env)),
      Self::Let(id, expr) => {
        let val = expr.map_or(Value::None, |expr| expr.evaluate(env));
        env.borrow_mut().set(id, val)
      },
      Self::Block(Block(statements)) => {
        let mut new_env = Enviroment::new();
        new_env.set_enclosing(env.clone());
        let mut new_env = Rc::new(RefCell::new(new_env));

        for stmt in statements {
          stmt.evaluate(&mut new_env);
        }
      },
    };

    Value::None
  }
}
