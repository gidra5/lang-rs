use crate::token::Token;
use crate::common::*;
use crate::ast::Program;
use crate::evaluator::Evaluator;
use rustyline::{error::*, *};
use rustyline_derive::*;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl: Editor<InteractiveModeHelper>,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut inst = Self { rl: Editor::new() };
    
    inst.rl.set_helper(Some(InteractiveModeHelper {}));
    inst.rl.bind_sequence(
        KeyEvent(KeyCode::Enter, Modifiers::ALT),
        Cmd::Newline
      );

    inst
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: String) {
    let _lines = code.lines();

    todo!();
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

          let mut tokens = ReversableStream::<Token>::from(line.clone());
          let parsed = Program::parse(&mut tokens);
          let result = Evaluator::evaluate(parsed.ok().unwrap());

          println!("{}", result);

          self.rl.add_history_entry(line.as_str());
        },
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
