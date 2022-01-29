#![allow(clippy::inconsistent_struct_constructor, dead_code)] //TODO: should remove dead_code when ready
#![feature(box_patterns)]
#![feature(derive_default_enum)]
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
use interactive_mode::*;

fn main() {
  let yaml = load_yaml!("cli.yml");
  let matches = clap::App::from_yaml(yaml).get_matches();

  let mut im = InteractiveMode::new();

  let code = {
    if let Some(filename) = matches.value_of("SRC_FILE") {
      match CharStream::new(filename) {
        Ok(s) => Some(s),
        Err(e) => {
          im.logger.error(e);
          None
        },
      }
    } else {
      None
    }
  };

  if let Some(code) = code {
    im.exec(code);
  }

  if matches.is_present("interactive") {
    im.run();
  }
}
