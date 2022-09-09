use std::io::{BufRead, Write};

use crate::lexer::Lexer;
use crate::parser::parse;

pub fn start() {
    println!("Welcome to the Diamond Lang REPL!");
    print!(">> ");
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin().lock();
    for line in stdin.lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(line.as_bytes(), line.len());
        match parse(lexer) {
            Ok(statements) => {
                for statement in statements {
                    println!("{statement:?}");
                }
            }
            Err(errs) => println!("{errs}"),
        }
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }
}
