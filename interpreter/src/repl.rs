use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::rc::Rc;

use crate::evaluator::eval;
use crate::evaluator::Environment;
use crate::lexer::Lexer;
use crate::parser::parse;

pub fn start() {
    println!("Welcome to the Diamond Lang REPL!");
    print!(">> ");
    std::io::stdout().flush().unwrap();
    let stdin = std::io::stdin().lock();
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    for line in stdin.lines() {
        let line = line.unwrap();
        let lexer = Lexer::new(&line);
        let ast = match parse(lexer) {
            Ok(ast) => Some(ast),
            Err(errs) => {
                println!("{errs:?}");
                None
            }
        };
        if let Some(ast) = ast {
            let res = match eval(ast, env.clone()) {
                Ok(obj) => obj.to_string(),
                Err(errs) => format!("{:?}", errs),
            };
            println!("{res}");
        }
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }
}
