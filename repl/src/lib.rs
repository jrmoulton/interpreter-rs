use std::{
    io::{BufRead, Write},
    sync::Arc,
};

use evaluator::{eval, structs::Environment};
use lexer::{Lexer, PeekLex};
use owo_colors::OwoColorize;
use parser::parse;

pub fn start(env: Option<(Arc<Environment>, PeekLex)>) -> ! {
    println!("Welcome to Awesome Name Lang REPL!");
    let mut line_num = 1;
    let prompt = |num| format!("{num}: >> ").red().to_string();
    print!("{}", prompt(line_num));
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin().lock();
    let (env, mut peek_lex) = match env {
        Some(inner) => inner,
        None => (
            Arc::new(Environment::default()),
            PeekLex::new(Lexer::default()),
        ),
    };
    for line in stdin.lines() {
        line_num += 1;
        let line = line.unwrap();
        peek_lex.update(line);
        let ast = match parse(&mut peek_lex) {
            Ok(ast) => Some(ast),
            Err(errs) => {
                println!("{errs:?}\n");
                None
            },
        };
        if let Some(ast) = ast {
            match eval(ast, env.clone()) {
                Ok(obj) => {
                    // if let Object::Empty(_) = obj {
                    //     // Don't print anything
                    // } else {
                    println!("{}", obj);
                    // }
                },
                Err(errs) => println!("{errs:?}\n"),
            };
        }
        print!("{}", prompt(line_num));
        std::io::stdout().flush().unwrap();
    }
    unreachable!()
}
