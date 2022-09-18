use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::rc::Rc;

use crate::evaluator::eval;
use crate::evaluator::Environment;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::parse;

pub fn start() {
    println!("Welcome to Awesome Name Lang REPL!");
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
            match eval(ast, env.clone()) {
                Ok(obj) => {
                    if let Object::Empty(_) = obj {
                        // Don't print anything
                    } else {
                        println!("{}", obj);
                    }
                }
                Err(errs) => println!("{:?}", errs),
            };
        }
        print!(">> ");
        std::io::stdout().flush().unwrap();
    }
}
