use crate::{ast::*, common::*, enviroment::*, token::*};
use rustyline::{error::*, *};
use rustyline_derive::*;

#[path = "tests/interactive_mode.rs"]
mod tests;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl:         Editor<InteractiveModeHelper>,
  env:        Enviroment,
  context:    ParsingContext,
  pub logger: Logger,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut rl = Editor::new();

    rl.set_helper(Some(InteractiveModeHelper {}));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    Self {
      rl,
      context: ParsingContext::new(),
      env: Enviroment::new(),
      logger: Logger { logs: vec![] },
    }
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: CharStream) {
    match TokenStream::new(code, &mut self.logger) {
      Some(mut tokens) => {
        match Script::parse_ext(&mut tokens, &mut self.context) {
          Ok(tree) => {
            tree.node.evaluate(&mut self.env, &mut self.logger);
          },
          Err(msg) => self.logger.error_parse(msg),
        };
      },
      None => self.logger.error("Tokenization failed"),
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

          match TokenStream::new(CharStream::from_string(line), &mut self.logger) {
            Some(mut tokens) => {
              match Script::parse_ext(&mut tokens, &mut self.context) {
                Ok(tree) => {
                  tree.node.evaluate(&mut self.env, &mut self.logger);
                },
                Err(msg) => self.logger.error_parse(msg),
              };
            },
            None => self.logger.error("Tokenization failed"),
          };
        },
        Err(ReadlineError::Interrupted) => break,
        Err(_) => self.logger.log("No input"),
      }
    }
  }
}
