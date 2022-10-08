use std::{error::Error, fs, sync::Arc};

use clap::Parser;
use evaluator::{eval, structs::Environment};
use lexer::Lexer;
use parser::parse;
use repl::start;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(value_parser)]
    file: Option<String>,
    #[clap(short, long)]
    interactive: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    if let Some(file_path) = args.file {
        let file = fs::read_to_string(&file_path)?;
        let env = Arc::new(Environment::default());
        let lexer = Lexer::new(&file);
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
                    if !args.interactive {
                        println!("{}", obj);
                    }
                }
                Err(errs) => println!("{}", errs),
            };
        }
        if args.interactive {
            start(Some(env));
        }
    } else {
        start(None)
    }
    Ok(())
}
