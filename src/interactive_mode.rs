use crate::{common::*, enviroment::*};
use rustyline::{error::*, *};
use rustyline_derive::*;


#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl: Editor<InteractiveModeHelper>,
  env: Enviroment
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut rl = Editor::new();

    rl.set_helper(Some(InteractiveModeHelper {}));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    Self { rl, env: Enviroment::new() }
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: CharStream) {
    match TokenStream::new(code) {
      Some(mut tokens) => {
        match Expression::parse(&mut tokens, 0) {
          Ok(tree) => println!("{:?}", Self::evaluate(&mut self.env, tree)),
          Err(msg) => println!("{}", msg),
        };
      },
      None => println!("Tokenization failed"),
    };
  }

  /// runs interpreter in interactive mode
  pub fn run(&mut self) {
    loop {
      let readline = self.rl.readline(">> ");

      match readline {
        Ok(line) => {
          if line == "quit" {
            break;
          }

          self.rl.add_history_entry(line.as_str());

          match TokenStream::new(CharStream::from_string(line)) {
            Some(mut tokens) => {
              match Expression::parse(&mut tokens, 0) {
                Ok(tree) => println!("{:?}", Self::evaluate(&mut self.env, tree)),
                Err(msg) => println!("{}", msg),
              };
            },
            None => println!("Tokenization failed"),
          };
        },
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }

  fn evaluate(env: &mut Enviroment, expr: Expression) -> Value {
    use Token::*;
    use Expression::*;

    match expr {
      BinaryExpression(left, op, right) => {
        let right = Self::evaluate(env, *right);
        let left = if op != Equal { Self::evaluate(env, *left) } else { 
          return match *left {
            Literal(Value::Identifier(ident)) => { env.set(ident, right.clone()); right },
            _ => Value::None,
          }
        };
        

        match op {
          Add => left.add(right),
          Sub => left.sub(right),
          Mult => left.mult(right),
          Div => left.div(right),
          Pow => left.pow(right),
          Mod => left.rem(right),
          EqualEqual => left.e(right), 
          LessEqual => left.le(right), 
          GreaterEqual => left.ge(right),
          _ => unreachable!()
        }
      },
      UnaryPrefixExpression(op, expr) => {
        let expr = Self::evaluate(env, *expr);

        match op {
          Sub => expr.neg(),
          Bang => expr.inv(),
          Dec => expr.dec(),
          Inc => expr.inc(),
          _ => unreachable!()
        }
      },
      UnaryPostfixExpression(op, expr) => {
        let _expr = Self::evaluate(env, *expr);

        match op {
          _ => unreachable!()
        }
      },
      FunctionCallExpression(_func, _arg) => {
        Value::None
        // let func = Self::evaluate(env, *func);

        // func.evaluate(env, arg)
      },
      Literal(val) => match val {
        Value::Identifier(id) => match env.get(id) {
          Some(val) => val,
          None => Value::None
        },
        val => val
      },
    }
  }
}
