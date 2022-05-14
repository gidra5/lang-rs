#![allow(
  clippy::inconsistent_struct_constructor,
  dead_code,
  unused_macros,
  unused_parens
)] //TODO: should remove dead_code when ready
#![feature(box_patterns)]
#![feature(is_some_with)]
#![feature(derive_default_enum)]
#![feature(associated_type_defaults)]
#![feature(type_alias_impl_trait)]
extern crate derivative;
extern crate either;
extern crate fancy_regex;
extern crate itertools;
extern crate rustyline;

#[macro_use]
extern crate clap;

mod ast;
mod common;
mod enviroment;
mod errors;
mod interactive_mode;
mod namespace;
mod parseable;
mod token;
mod types;
mod value;
mod vm;

use interactive_mode::*;

fn main() {
  let yaml = load_yaml!("cli.yml");
  let matches = clap::App::from_yaml(yaml).get_matches();

  let mut im = InteractiveMode::new();

  let code = {
    if let Some(filename) = matches.value_of("SRC_FILE") {
      let file = std::fs::read(filename)
        .map(|file| String::from_utf8(file).expect("Failed to parse as utf8 text"))
        .map_err(|err| {
          use std::io::ErrorKind::*;
          match err.kind() {
            NotFound => "No such file",
            PermissionDenied => {
              "Permission denied, maybe try running as administrator/sudo or add 'executable' flag"
            },
            _ => "Unexpected IO error",
          }
        });
      match file {
        Ok(src) => Some(src),
        Err(err) => {
          println!("{err}");
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
