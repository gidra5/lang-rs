use std::{cell::RefCell, rc::Rc};

use crate::{common::*, enviroment::*};
use rustyline::{error::*, *};
use rustyline_derive::*;


#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl:  Editor<InteractiveModeHelper>,
  env: Rc<RefCell<Enviroment>>,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut rl = Editor::new();

    rl.set_helper(Some(InteractiveModeHelper {}));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    Self {
      rl,
      env: Rc::new(RefCell::new(Enviroment::new())),
    }
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: CharStream) {
    match TokenStream::new(code) {
      Some(mut tokens) => {
        match Statement::parse_ext(&mut tokens) {
          Ok(tree) => {
            tree.node.evaluate(&mut self.env);
          },
          Err(msg) => Logger::error_parse(msg),
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
              match Statement::parse_ext(&mut tokens) {
                Ok(tree) => {
                  tree.node.evaluate(&mut self.env);
                },
                Err(msg) => Logger::error_parse(msg),
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
}
