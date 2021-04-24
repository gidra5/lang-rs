use crate::ast::Program;
use crate::common::*;
use crate::evaluator::Evaluator;
use crate::token::Token;
use rustyline::{error::*, *};
use rustyline_derive::*;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl: Editor<InteractiveModeHelper>,
  evaluator: Evaluator,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut inst = Self { rl: Editor::new(), evaluator: Evaluator::new() };

    inst.rl.set_helper(Some(InteractiveModeHelper {}));
    inst
      .rl
      .bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    inst
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: String) {
    // let _lines = code.lines();

    let mut tokens = ReversableStream::<Token>::from(code);
    let parsed = match Program::parse(&mut tokens) {
      Ok(parsed) => parsed,
      Err(e) => { println!("{}", e); return; },
    }; 
    self.evaluator.evaluate(parsed);
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

          let mut tokens = ReversableStream::<Token>::from(line);
          let parsed = match Program::parse(&mut tokens) {
            Ok(parsed) => parsed,
            Err(e) => { println!("{}", e); continue; },
          }; 
          self.evaluator.evaluate(parsed);
        }
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
