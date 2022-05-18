use crate::{
  common::{expr, group_map_iterator::GroupMapTrait, Buf},
  enviroment::Enviroment,
  parse_tokens,
  parseable::ParsingContext,
  token::Token,
};
use rustyline::{error::*, *};
use rustyline_derive::*;

#[path = "tests/interactive_mode.rs"]
mod tests;

#[derive(Completer, Helper, Highlighter, Hinter, Validator)]
pub struct InteractiveModeHelper {}

pub struct InteractiveMode {
  rl:      Editor<InteractiveModeHelper>,
  env:     Option<Enviroment>,
  context: ParsingContext,
}

impl InteractiveMode {
  pub fn new() -> Self {
    let mut rl = Editor::new();

    rl.set_helper(Some(InteractiveModeHelper {}));
    rl.bind_sequence(KeyEvent(KeyCode::Enter, Modifiers::ALT), Cmd::Newline);

    Self {
      rl,
      context: ParsingContext::new(),
      env: None,
    }
  }

  /// executes given code in interpreter
  pub fn exec(&mut self, code: String) {
    let mut errors = vec![];
    let mut tokens = code
      .chars()
      .buffered()
      .group_map(|iter| parse_tokens!(errors, iter))
      .filter(|token: &Token| token.clone() != Token::Skip)
      .buffered();
    (&mut tokens).for_each(|token| println!("{token}"));
    errors.iter().for_each(|err| println!("{err}"))
    // match Script::parse(tokens) {
    //   Ok(tree) => match tree.node.evaluate(&mut self.env) {
    //     Ok(_) => (),
    //     Err(RuntimeError::Generic(msg)) => println!("Runtime error: {msg}"),
    //   },
    //   Err(msg) => println!("Parsing error: {msg}"),
    // };
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
          let res = expr(&line);
          match res {
            Ok(o) => println!("res: {:?}", o),
            Err(e) => println!("err: {:?}", e),
          };
        },
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
