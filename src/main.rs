#![allow(clippy::inconsistent_struct_constructor, dead_code)] //TODO: should remove dead_code when ready
#![feature(bindings_after_at)]
extern crate automata;
extern crate either;
extern crate fancy_regex;
extern crate itertools;
extern crate rustyline;

#[macro_use]
extern crate clap;

mod ast;
mod common;
mod enviroment;
mod interactive_mode;
mod token;
mod vm;

use crate::common::*;

fn main() {
  let yaml = load_yaml!("cli.yml");
  let matches = clap::App::from_yaml(yaml).get_matches();
  let code = {
    if let Some(filename) = matches.value_of("SRC_FILE") {
      match CharStream::new(filename) {
        Ok(s) => Some(s),
        Err(e) => {
          Logger::error(e);
          None
        },
      }
    } else {
      None
    }
  };

  if matches.is_present("interactive") {
    use interactive_mode::*;

    let mut im = InteractiveMode::new();

    if let Some(code) = code {
      im.exec(code);
    }

    im.run();
  } else {
    todo!();
  }
}
