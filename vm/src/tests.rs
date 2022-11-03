use compiler::Compiler;

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
fn single_bool() {
    let code: &'static str = r#"false"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, false.into());
            let expected = expect_file!["../tests/expect_test_results/single_bool.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn bang_false() {
    let code: &'static str = r#"!false"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, true.into());
            let expected = expect_file!["../tests/expect_test_results/bang_false.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn greater_than() {
    let code: &'static str = r#"4 > 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, false.into());
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn less_than() {
    let code: &'static str = r#"4 < 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, true.into());
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn equal() {
    let code: &'static str = r#"5 == 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, true.into());
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn not_equal() {
    let code: &'static str = r#"3 == 5"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, false.into());
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
fn string_add() {
    let code: &'static str = r#" "5" + "5" "#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, "55".to_string().into());
            let expected = expect_file!["../tests/expect_test_results/string_add.txt"];
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

#[test]
fn if_true_five() {
    let code: &'static str = r#"if true {5}; 404"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, 404.into());
            let expected = expect_file!["../tests/expect_test_results/if_true_five.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_false_5_else_3_final() {
    let code: &'static str = r#"if false {5} else {3}; 404"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, 404.into());
            let expected = expect_file!["../tests/expect_test_results/if_false_5_else_3_final.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_false_5_else_3() {
    let code: &'static str = r#"if false {5} else {3}"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, 3.into());
            let expected = expect_file!["../tests/expect_test_results/if_false_5_else_3.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_true_5_else_3() {
    let code: &'static str = r#"if true {5} else {3}"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, 5.into());
            let expected = expect_file!["../tests/expect_test_results/if_true_5_else_3.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_arue_5_else_3_semi() {
    let code: &'static str = r#"if true {5} else {3};"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, evaluator::object::EmptyWrapper.into());
            let expected = expect_file!["../tests/expect_test_results/if_true_5_else_3_semi.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_false_no_alternative_semi() {
    let code: &'static str = r#"if false {3};"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, evaluator::object::EmptyWrapper.into());
            let expected =
                expect_file!["../tests/expect_test_results/if_false_no_alternative_semi.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_false_3() {
    let code: &'static str = r#"if false {3}"#;
    let mut vm = new_vm(code);
    match vm.run() {
        Ok(object) => {
            assert_eq!(object, evaluator::object::EmptyWrapper.into());
            let expected = expect_file!["../tests/expect_test_results/if_false_3.txt"];
            expected.assert_eq(&format!("{vm:#?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}
