#![cfg(test)]
use crate::lexer::Lexer;

use super::*;
use expect_test::expect_file;

#[test]
fn simple_prefix_op_expression_statement() {
    let code: &'static str = r#"-5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file![
                "./../../tests/expect_test_results/parser/simple_prefix_op_expression_statement.txt"
            ];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn single_let() {
    let code: &'static str = r#"let x = 5;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file!["./../../tests/expect_test_results/parser/single_let.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn single_let_with_bool() {
    let code: &'static str = r#"let x = true;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/single_let_with_bool.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn single_let_with_add() {
    let code: &'static str = r#"let x = 10 + 3;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/single_let_with_add.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn single_operator_precedence_expression_statement() {
    let code: &'static str = r#"5 + 5 * 5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file![
                "./../../tests/expect_test_results/parser/single_operator_precedence_expression_statement.txt"
            ];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}
#[test]
fn operator_precedence_with_grouped_expressions() {
    let code: &'static str = r#"(5 + 5) * 5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file![
                "./../../tests/expect_test_results/parser/operator_precedence_with_grouped_expressions.txt"
            ];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn single_int_expression() {
    let code: &'static str = r#"35"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/single_int_expression.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn basic_return() {
    let code: &'static str = r#"return 35;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/basic_return.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn identifier_expression() {
    let code: &'static str = r#"foobar"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/identifier_expression.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn double_let() {
    let code: &'static str = r#"let x = 5;
        let y = 3;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file!["./../../tests/expect_test_results/parser/double_let.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_elseif_else() {
    let code: &'static str = r#"
        if x {
            x
        } else if (y) {
            y
        } else {
            foobar
        }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/if_elseif_else.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

// Ahh yeah this is better
#[test]
fn if_elseif_else_again() {
    let code: &'static str = r#"
        if x {
            x
        } else if (y) {
            y
        } else {
            foobar
        }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/if_elseif_else_again.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn if_with_bool_expr() {
    let code: &'static str = r#"
        if x {
            true
        } else {
            false
        }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/if_with_bool_expr.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn new_scope() {
    let code: &'static str = r#"
        {

        }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file!["./../../tests/expect_test_results/parser/new_scope.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn nested_scope() {
    let code: &'static str = r#"
        {
            {

            }
        }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/nested_scope.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn function_literal() {
    let code: &'static str = r#"fn(x, y) {
        x + y
    }"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/function_literal.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn call_expression() {
    let code: &'static str = r#"add(x, y)"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected =
                expect_file!["./../../tests/expect_test_results/parser/call_expression.txt"];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn call_expression_no_args() {
    let code: &'static str = r#"add();"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file![
                "./../../tests/expect_test_results/parser/call_expression_no_args.txt"
            ];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}

#[test]
fn call_expression_with_expression_args() {
    let code: &'static str = r#"call_func(2, 3, fn(x,y){x + y})"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let expected = expect_file![
                "./../../tests/expect_test_results/parser/call_expression_with_expression_args.txt"
            ];
            expected.assert_eq(&format!("{statements:?}"));
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    }
}
