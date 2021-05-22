use crate::ast::Program;
use crate::common::*;
use rustyline::{error::*, *};
use rustyline_derive::*;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl: Editor<InteractiveModeHelper>,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut inst = Self {
      rl: Editor::new(),
    };

    inst.rl.set_helper(Some(InteractiveModeHelper {}));
    inst
      .rl
      .bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    inst
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: CharStream) {
    // let _lines = code.lines();

    let mut tokens = TokenStream::new(code).unwrap();
    while let Some(token) = tokens.stream.next() {
      println!("{:?}", token);
    }
    // let parsed = match Program::parse(&mut tokens) {
    //   Ok(parsed) => parsed,
    //   Err(e) => {
    //     println!("{}", e);
    //     return;
    //   }
    // };
    // self.evaluator.evaluate(parsed);
    todo!()
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
              while let Some(token) = tokens.stream.next() {
                println!("{:?}", token);
              }
            },
            None => println!("An error occured during tokenization"),
          };
        }
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
