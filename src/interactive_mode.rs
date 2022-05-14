use std::iter::{Filter, Inspect};

use crate::{
  ast::{Expression, ParsingInput},
  common::{Buf, Buffered},
  enviroment::*,
  parseable::{Parseable, ParseableIterator, Parsed, ParsingContext},
  token::{Token, TokenizationInput},
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
    let mut tokens: Parsed<_, Token> = TokenizationInput::new(code.chars().buffered()).parsed();
    (&mut tokens).for_each(|token| println!("{token}"));
    let errors = tokens.source.errors;
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
          println!("{line}");
          let tokens: Buffered<Inspect<Filter<Parsed<_, Token>, _>, _>> =
            TokenizationInput::new(line.chars().inspect(|x| println!("\"{x}\"")).buffered())
              .parsed()
              .filter(|token: &Token| token.clone() != Token::Skip)
              .inspect(|x| println!("f: {:?}", x))
              .buffered();
          let res = <Expression as Parseable<_>>::parse(ParsingInput {
            tokens,
            context: self.context.clone(),
            errors: vec![],
          });
          match res {
            (_, Some(o)) => println!("res: {:?}", o),
            (i, None) => println!("err: {:?}", i.errors),
          };
        },
        Err(ReadlineError::Interrupted) => break,
        Err(_) => println!("No input"),
      }
    }
  }
}
