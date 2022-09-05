use std::io::{BufRead, Write};

use crate::lexer::Lexer;

pub fn start() {
    println!("Welcome to the Monkey Lang REPL!");
    print!(">> ");
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin().lock();
    for line in stdin.lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(line.as_bytes(), line.len());
        lexer.into_iter().for_each(|token| {
            println!("{token:?}");
        });
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }
}
