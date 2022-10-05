use std::io::{BufRead, Write};
use std::sync::Arc;

use evaluator::eval;
use evaluator::{object::Object, structs::Environment};
use lexer::Lexer;
use parser::parse;

pub fn start(env: Option<Arc<Environment>>) -> ! {
    println!("Welcome to Awesome Name Lang REPL!");
    print!(">> ");
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin().lock();
    let env = match env {
        Some(env) => env,
        None => Arc::new(Environment::default()),
    };
    for line in stdin.lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        let ast = match parse(lexer) {
            Ok(ast) => Some(ast),
            Err(errs) => {
                println!("{errs:?}\n");
                None
            }
        };
        if let Some(ast) = ast {
            match eval(ast, env.clone()) {
                Ok(obj) => {
                    if let Object::Empty(_) = obj {
                        // Don't print anything
                    } else {
                        println!("{}", obj);
                    }
                }
                Err(errs) => println!("{errs:?}\n"),
            };
        }
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }
    unreachable!()
}
