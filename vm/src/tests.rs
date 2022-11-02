use compiler::compiler::Compiler;

use super::*;
use expect_test::expect_file;
use lexer::{Lexer, PeekLex};
use parser::parse;

fn new_vm(code: &'static str) -> VM {
    let lexer = Lexer::new(code.into());
    let mut peek_lex = PeekLex::new(lexer);
    let mut ast = parse(&mut peek_lex).unwrap().into_iter();
    let mut compiler = Compiler::new();
    compiler.compile(&mut ast);
    let bytecode = compiler.bytecode;
    // bytecode.push(crate::bytecode::OpCode::Print);
    VM::new(compiler.constants, bytecode)
}

#[test]
fn single_int() {
    let code: &'static str = r#"57"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, 57.into());
            let expected = expect_file!["../tests/expect_test_results/single_int.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn prefix_expr() {
    let code: &'static str = r#"-57"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, (-57).into());
            let expected = expect_file!["../tests/expect_test_results/prefix_expr.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn binary_divide() {
    let code: &'static str = r#"10 / 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, (2).into());
            let expected = expect_file!["../tests/expect_test_results/binary_divide.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn binary_multiply() {
    let code: &'static str = r#"10 * 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, (50).into());
            let expected = expect_file!["../tests/expect_test_results/binary_multiply.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}
