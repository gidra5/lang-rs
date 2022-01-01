use std::{
  cell::RefCell,
  cmp::Ordering,
  fmt::{Display, Formatter},
  rc::Rc,
};

use itertools::Itertools;

use crate::{
  check_token,
  check_token_end,
  common::{
    char_stream::{value::Value, Token, TokenExt, TokenStream},
    logger::char_stream::LoggerTrait,
    reversable_iterator::ReversableIterator,
  },
  enviroment::Enviroment,
  punct_or_newline,
  token::{self, char_stream::value},
};

use super::{Evaluatable, Parseable};

#[derive(Clone, PartialEq, Debug)]
pub enum Op {
  Value(Value),
  Record(Vec<RecordItem>),
  // Block(Vec<RecordItem>),
}
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
  left:  Option<Box<Expression>>,
  op:    Op,
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

impl Default for Expression {
  fn default() -> Expression {
    Expression {
      left:  None,
      op:    Op::Value(Value::None),
      right: None,
    }
  }
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
    match op {
      Op::Value(op) => {
        match (left, right) {
          (None, None) => write!(f, "{}", op),
          (Some(left), None) => write!(f, "({} {})", op, left),
          (None, Some(right)) => write!(f, "({} {})", op, right),
          (Some(left), Some(right)) => write!(f, "({} {} {})", op, left, right),
        }
      },
      Op::Record(record_items) => {
        let mut x = record_items
          .iter()
          .map(|RecordItem { name, expr }| (name, format!("{}", expr)))
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].0 != "0") {
          write!(
            f,
            "({})",
            x.iter()
              .map(|(name, expr)| format!("{}: {}", name, expr))
              .join(", ")
          )
        } else if x.len() == 1 {
          write!(f, "{}", x.pop().unwrap().1)
        } else {
          write!(f, "()")
        }
      },
    }
  }
}

#[derive(Clone, PartialEq, Debug)]
pub struct RecordItem {
  name: String,
  expr: Expression,
}

fn parse_braces(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), Token::LBrace) {
    Err("Expected opening brace".to_string())
  } else {
    token_stream.next();

    let expr = Expression::parse(token_stream)?;

    if !check_token!(token_stream.peek(), Token::RBrace) {
      Err("Expected closing brace".to_string())
    } else {
      Ok(expr)
    }
  }
}

fn parse_parens(token_stream: &mut TokenStream<'_>) -> Result<Expression, String> {
  if !check_token!(token_stream.peek(), Token::LParenthesis) {
    Err("Expected opening parenthesis".to_string())
  } else {
    token_stream.next();
    let mut x = vec![];
    if check_token!(token_stream.peek(), Token::RParenthesis) {
      token_stream.next();

      return Ok(Expression {
        left:  None,
        right: None,
        op:    Op::Record(x),
      });
    }

    {
      let y = token_stream.peek_ext(2);
      let mut name = None;

      if check_token!(y[0], Token::Identifier) && check_token!(y[1], Token::Colon) {
        name = Some(token_stream.next().unwrap().src);
        token_stream.next();
      }

      let expr = parse_expr(token_stream, true)?;

      x.push(RecordItem {
        name: name.unwrap_or(x.len().to_string()),
        expr,
      });
    }

    while !check_token!(token_stream.peek(), Token::RParenthesis) {
      if !punct_or_newline!(token_stream.next(), Comma) {
        return Err(
          "Unexpected token, should be either comma, newline or closing parenthesis".to_string(),
        );
      }
      if check_token_end!(token_stream) {
        return Err("Unexpected end of input".to_string());
      }

      let y = token_stream.peek_ext(2);
      let mut name = None;

      if check_token!(y[0], Token::Identifier) && check_token!(y[1], Token::Colon) {
        name = Some(token_stream.next().unwrap().src);
        token_stream.next();
      }

      let expr = parse_expr(token_stream, true)?;

      x.push(RecordItem {
        name: name.unwrap_or(x.len().to_string()),
        expr,
      })
    }
    token_stream.next();

    Ok(Expression {
      left:  None,
      right: None,
      op:    Op::Record(x),
    })
  }
}

#[derive(Debug, Clone)]
struct Frame {
  operator: Option<Operator>,
  lhs:      Option<Expression>,
}

impl Evaluatable for Expression {
  fn evaluate<L: LoggerTrait>(self, env: &mut Rc<RefCell<Enviroment>>, logger: &mut L) -> Value {
    match self {
      Expression {
        left: None,
        op: Op::Value(Value::Identifier(id)),
        right: None,
      } => env.borrow().get(id).unwrap_or_default(),
      Expression {
        left: None,
        op: Op::Value(op),
        right: None,
      } => op,
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: None,
      } => op.postfix((*left).evaluate(env, logger)),
      Expression {
        left: None,
        op: Op::Value(op),
        right: Some(right),
      } => op.prefix((*right).evaluate(env, logger)),
      Expression {
        left: Some(left),
        op: Op::Value(op @ Value::Operator(Token::Equal)),
        right: Some(right),
      } => {
        let left = *left;
        let right = (*right).evaluate(env, logger);
        if let Expression {
          left: None,
          op: Op::Value(Value::Identifier(left)),
          right: None,
        } = left
        {
          env.borrow_mut().set(left, right.clone());
          println!("{:?}", env);
          right
        } else {
          op.infix(left.evaluate(env, logger), right)
        }
      },
      Expression {
        left: Some(left),
        op: Op::Value(op),
        right: Some(right),
      } => {
        op.infix(
          (*left).evaluate(env, logger),
          (*right).evaluate(env, logger),
        )
      },
      Expression {
        left: _,
        op: Op::Record(op),
        right: _,
      } => {
        let mut x = op
          .into_iter()
          .map(|RecordItem { name, expr }| {
            value::RecordItem {
              name,
              value: expr.evaluate(env, logger),
            }
          })
          .collect::<Vec<_>>();
        if x.len() > 1 || (x.len() == 1 && x[0].name != "0") {
          Value::Record(x)
        } else if x.len() == 1 {
          x.pop().unwrap().value
        } else {
          Value::Unit
        }
      },
    }
  }
}
fn parse_expr(token_stream: &mut TokenStream<'_>, in_parens: bool) -> Result<Expression, String> {
  let mut top = Frame {
    lhs:      None,
    operator: None,
  };
  let mut stack = Vec::new();

  loop {
    let token = token_stream.peek();
    if check_token!(token, Token::LParenthesis) {
      top.lhs = Some(parse_parens(token_stream)?);
      continue;
    } else if check_token!(token, Token::RParenthesis) && !in_parens {
      return Err("Unexpected closing parenthesis".to_string());
    }
    let operator = loop {
      let operator = token
        .clone()
        .map(|token| Operator::new(token, top.lhs.is_none()))
        .flatten();
      match operator {
        // Some(None) => return Err("No such operator"),
        // Some(t @ Some(op)) if top.operator <= t => break op,
        Some(op)
          if top.operator <= Some(op.clone())
            // && !matches!(op, Operator {
            //   value:  Value::Operator(Token::RParenthesis),
            //   fixity: Fixity::Postfix,
            // }) 
            =>
        {
          break op
        },
        _ => {
          let res = top;
          // if let Some(Expression {
          //   op: Value::Operator(Token::LParenthesis),
          //   left: None,
          //   right: Some(_),
          // }) = res.lhs
          // {
          //   return Err("Expected closing parenthesis".to_string());
          // }

          top = match stack.pop() {
            Some(it) => it,
            None => return Ok(res.lhs.unwrap_or_default()),
          };

          top.lhs = Some(Expression {
            op:    Op::Value(res.operator.unwrap().value),
            left:  top.lhs.map(Box::new),
            right: res.lhs.map(Box::new),
          });
        },
      };
    };
    token_stream.next();

    // if let Operator {
    //   value: Value::Operator(Token::RParenthesis),
    //   fixity: Fixity::Postfix,
    // } = operator
    // {
    //   if !in_parens {
    //     return Err("Unexpected closing parenthesis".to_string());
    //   }
    // }

    stack.push(top);
    top = Frame {
      lhs:      None,
      operator: Some(operator),
    };
  }
}

impl<'a> Parseable<'a> for Expression {
  fn parse(stream: &mut TokenStream<'a>) -> Result<Self, String> { parse_expr(stream, false) }
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
          Value::Operator(
            Token::LParenthesis | Token::Add | Token::Inc | Token::Dec | Token::Mult | Token::Sub,
          ),
        fixity: Fixity::Prefix,
      } => true,
      Operator {
        value: Value::Operator(Token::RParenthesis | Token::Bang),
        fixity: Fixity::Postfix,
      } => true,
      Operator {
        value:
          Value::Operator(
            Token::Comma
            | Token::Period
            | Token::LAngleBracket
            | Token::RAngleBracket
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::Equal
            | Token::EqualEqual
            | Token::Add
            | Token::Sub
            | Token::Mod
            | Token::Mult
            | Token::Div,
          ),
        fixity: Fixity::Infix,
      } => true,
      Operator {
        value: Value::Identifier(id),
        fixity: Fixity::Infix,
      } => matches!(id.as_str(), "mod" | "and" | "or"),
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
              Token::Comma => (22, 21),
              Token::Colon => (18, 17),
              Token::Period => (24, 23),
              Token::Equal => (2, 1),
              Token::Add | Token::Sub => (5, 6),
              Token::Mult | Token::Div => (7, 8),
              Token::EqualEqual => (20, 19),
              Token::LAngleBracket => (20, 19),
              Token::RAngleBracket => (20, 19),
              Token::LessEqual => (20, 19),
              Token::GreaterEqual => (20, 19),
              _ => return None,
            }
          },
          Value::Identifier(id) if id == "mod" => (22, 21),
          Value::Identifier(id) if id == "and" => (24, 23),
          Value::Identifier(id) if id == "or" => (25, 26),
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
