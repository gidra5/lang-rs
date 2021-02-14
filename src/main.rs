extern crate automata;
extern crate itertools;

mod args;
mod token;

fn main() {
    // extract file content
    let file = {
        let mut args = std::env::args().skip(1);

        match args.next() {
            Some(filename) => match std::fs::read(filename) {
                Ok(file) => file,
                Err(err) => {
                    use std::io::ErrorKind::*;
                    println!("{}", match err.kind() {
                        NotFound => "Invalid file",
                        PermissionDenied => "Permission denied",
                        _ => "Unexpected IO error",
                    });

                    return;
                }
            },
            None => { println!("No arguments given, please specify input file"); return; }
        }
    };

    let code = String::from_utf8(file).expect("failed to parse as utf8 text");

    todo!();
}
