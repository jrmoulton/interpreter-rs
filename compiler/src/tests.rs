use super::*;
use expect_test::expect_file;
use lexer::{Lexer, PeekLex};
use parser::parse;

fn new_compiler(code: &'static str) -> Compiler {
    let lexer = Lexer::new(code.into());
    let mut peek_lex = PeekLex::new(lexer);
    let mut ast = parse(&mut peek_lex).unwrap().into_iter();
    let mut compiler = Compiler::new();
    compiler.compile(&mut ast);
    compiler
}

#[test]
fn single_int() {
    let code: &'static str = r#"57"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/single_int.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn int_add() {
    let code: &'static str = r#"11 + 6"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/int_add.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn int_sub() {
    let code: &'static str = r#"11 - 6"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/int_sub.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn int_mul() {
    let code: &'static str = r#"11 * 3"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/int_mul.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn pre_neg() {
    let code: &'static str = r#"-4"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/pre_neg.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn pre_neg_sub() {
    let code: &'static str = r#"5 - -4"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/pre_neg_sub.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn less_than() {
    let code: &'static str = r#"5 < 4"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/less_than.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn greater_than() {
    let code: &'static str = r#"5 > 4"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/greater_than.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn equal_to() {
    let code: &'static str = r#"5 == 5"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/equal_to.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn bool_true() {
    let code: &'static str = r#"true"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/bool_true.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn bool_false() {
    let code: &'static str = r#"false"#;
    let compiler = new_compiler(code);
    let expected = expect_file!["../tests/expect_test_results/bool_false.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn bang_bool_false() {
    let code: &'static str = r#"!false"#;
    let compiler = new_compiler(code);
    let exp_bytecode = vec![OpCode::Const(1), OpCode::Bang];
    assert_eq!(exp_bytecode, compiler.bytecode);
    let expected = expect_file!["../tests/expect_test_results/bang_bool_false.txt"];
    expected.assert_eq(&format!("{compiler:#?}"));
}

#[test]
fn if_true_five() {
    let code: &'static str = r#"if true {5}; 404"#;
    let compiler = new_compiler(code);
    let exp_bytecode = vec![
        /*0*/ OpCode::Const(1),
        /*1*/ OpCode::JumpNotTruthy(4),
        /*2*/ OpCode::Const(2),
        /*3*/ OpCode::Jump(5),
        /*4*/ OpCode::Const(0),
        /*5*/ OpCode::Pop,
        /*6*/ OpCode::Const(3),
    ];
    assert_eq!(exp_bytecode, compiler.bytecode);
}

#[test]
fn if_false_no_alt_semi() {
    let code: &'static str = r#"if false {5};"#;
    let compiler = new_compiler(code);
    let exp_bytecode = vec![
        /*0*/ OpCode::Const(1),
        /*1*/ OpCode::JumpNotTruthy(4),
        /*2*/ OpCode::Const(2),
        /*3*/ OpCode::Jump(5),
        /*4*/ OpCode::Const(0),
        /*5*/ OpCode::Pop,
    ];
    assert_eq!(exp_bytecode, compiler.bytecode);
}

#[test]
fn if_false_5_else_3() {
    let code: &'static str = r#"if false {5} else {3}; 404"#;
    let compiler = new_compiler(code);
    let exp_bytecode = vec![
        /*0*/ OpCode::Const(1), // false
        /*1*/ OpCode::JumpNotTruthy(4), // if false go to 4
        /*2*/ OpCode::Const(2), // true: 5
        /*3*/ OpCode::Jump(5), // after const(5) jump to 5
        /*4*/ OpCode::Const(3), // false: 3
        /*5*/ OpCode::Pop, // semicolon
        /*6*/ OpCode::Const(4), // 404
    ];
    assert_eq!(exp_bytecode, compiler.bytecode);
    // let expected = expect_file!["../tests/expect_test_results/if_true_five.txt"];
    // expected.assert_eq(&format!("{compiler:#?}"));
}
#[test]
fn if_true_5_else_3() {
    let code: &'static str = r#"if true {5} else {3}"#;
    let compiler = new_compiler(code);
    let exp_bytecode = vec![
        /*0*/ OpCode::Const(1),
        /*1*/ OpCode::JumpNotTruthy(4),
        /*2*/ OpCode::Const(2),
        /*3*/ OpCode::Jump(5),
        /*4*/ OpCode::Const(3),
    ];
    assert_eq!(exp_bytecode, compiler.bytecode);
    // let expected = expect_file!["../tests/expect_test_results/if_true_five.txt"];
    // expected.assert_eq(&format!("{compiler:#?}"));
}
