use std::{error::Error, fmt::Display, fs};

// #[cfg(not(target_env = "msvc"))]
// use tikv_jemallocator::Jemalloc;

// #[cfg(not(target_env = "msvc"))]
// #[global_allocator]
// static GLOBAL: Jemalloc = Jemalloc;
use clap::Parser;
use compiler::Compiler;
use error_stack::{IntoReport, Result, ResultExt};
use lexer::{Lexer, PeekLex};
use parser::parse;
use vm::VM;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(value_parser)]
    file: String,
}

#[derive(Debug)]
enum Errors {
    IO,
    Parser,
    VirtualMachine,
}
impl Display for Errors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Error for Errors {}

fn main() -> Result<(), Errors> {
    let args = Args::parse();

    let file = fs::read_to_string(&args.file)
        .into_report()
        .change_context(Errors::IO)?;
    let lexer = Lexer::new(file);
    let mut peek_lex = PeekLex::new(lexer);
    let mut ast = parse(&mut peek_lex)
        .change_context(Errors::Parser)?
        .into_iter();
    let mut compiler = Compiler::new();
    compiler.compile(&mut ast);
    let mut vm = VM::new(compiler);
    vm.run().change_context(Errors::VirtualMachine)?;
    Ok(())
}
