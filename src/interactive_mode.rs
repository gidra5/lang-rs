use crate::ast::*;
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
    let mut rl = Editor::new();

    rl.set_helper(Some(InteractiveModeHelper {}));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    Self { rl }
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: CharStream) {
    // let _lines = code.lines();

    match TokenStream::new(code) {
      Some(mut tokens) => {
        println!("{:?}", tokens.stream.data());
        match Expression::parse(&mut tokens, 0) {
          Ok(tree) => println!("tree {:?}", tree),
          Err(_) => println!("fuck"),
        };
      }
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
              println!("{:?}", tokens.stream.data());
              match Expression::parse(&mut tokens, 0) {
                Ok(tree) => println!("tree {:?}", tree),
                Err(_) => println!("fuck"),
              };
            }
            None => println!("Tokenization failed"),
          };
        }
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
