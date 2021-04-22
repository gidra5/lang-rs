extern crate automata;
extern crate either;
extern crate itertools;
extern crate regex;
extern crate rustyline;

#[macro_use]
extern crate clap;

mod ast;
mod common;
mod evaluator;
mod interactive_mode;
mod token;

fn main() {
  let yaml = load_yaml!("cli.yml");
  let matches = clap::App::from_yaml(yaml).get_matches();
  let code = {
    if let Some(filename) = matches.value_of("SRC_FILE") {
      // extract file content
      let file = match std::fs::read(filename) {
        Ok(file) => file,
        Err(err) => {
          use std::io::ErrorKind::*;
          let msg = match err.kind() {
            NotFound => "No such file",
            PermissionDenied => "Permission denied, maybe try running as administrator/sudo or add 'executable' flag",
            _ => "Unexpected IO error",
          };

          println!("{}", msg);

          return;
        }
      };

      Some(String::from_utf8(file).expect("Failed to parse as utf8 text"))
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
