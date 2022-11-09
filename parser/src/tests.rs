#![cfg(test)]
use lexer::Lexer;
use lexer::PeekLex;

use super::*;
use expect_test::expect_file;

mod results {
    use super::*;

    #[test]
    fn simple_prefix_op_expression_statement() {
        let code: &'static str = r#"-5"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file![
                    "./../tests/expect_test_results/simple_prefix_op_expression_statement.txt"
                ];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/single_let.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn assign_statement() {
        let code: &'static str = r#"x = true;"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/assign_statement.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/single_let_with_bool.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/single_let_with_add.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file![
                "./../tests/expect_test_results/single_operator_precedence_expression_statement.txt"
            ];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file![
                "./../tests/expect_test_results/operator_precedence_with_grouped_expressions.txt"
            ];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/single_int_expression.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/basic_return.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn return_no_expression() {
        let code: &'static str = r#"return;"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/return_no_expression.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/identifier_expression.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/double_let.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/if_elseif_else.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/if_elseif_else_again.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/if_with_bool_expr.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/new_scope.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e:?}");
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/nested_scope.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/function_literal.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn let_ident_string() {
        let code: &'static str = r#"let x = "foo bar"; "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/let_ident_string.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn nested_if() {
        let code: &'static str = r#"if if true {true} else {false} {true} else {false}"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/nested_if.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn comprehensive() {
        let code: &'static str = r#"
    let x = 5;
    foobar;
    let y = (3 + 3) * 5;
    if a {
      b
    } else if c {
      d
    } else {
      e
    };
    let multiply = fn(x, y){x * y};
    100 / 20
    "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/comprehensive.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e:?}");
                assert!(false);
            }
        }
    }

    #[test]
    fn call_expression() {
        let code: &'static str = r#"add(x, y)"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/call_expression.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/call_expression_no_args.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
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
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file![
                    "./../tests/expect_test_results/call_expression_with_expression_args.txt"
                ];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }
    #[test]
    fn bool_and() {
        let code: &'static str = r#"3 == 3 && 5 == 6"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/bool_and.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn bool_or() {
        let code: &'static str = r#"3 == 3 || 5 == 6"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/bool_or.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn bit_and() {
        let code: &'static str = r#"3 & 6"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/bit_and.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn bit_or() {
        let code: &'static str = r#"3 | 6"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/bit_or.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn string_method_call() {
        let code: &'static str = r#" "foobar".len() "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/string_method_call.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn int_method_call() {
        let code: &'static str = r#" 55.abs() "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/int_method_call.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn int_array() {
        let code: &'static str = r#" [15, 30, 90] "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/int_array.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn empty_array() {
        let code: &'static str = r#" [] "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/empty_array.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn int_array_index() {
        let code: &'static str = r#" [15, 30, 90][1] "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected = expect_file!["./../tests/expect_test_results/int_array_index.txt"];
                expected.assert_eq(&format!("{statements:#?}"));
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }

    #[test]
    fn operator_precedence() {
        let code: &'static str = r#"5 + 5 * 3 | 2 && 4 - -2 || 3 | 1"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                let expected =
                    expect_file!["./../tests/expect_test_results/operator_precedence.txt"];
                let mut ret_str = String::from("[ ");
                for statement in statements {
                    ret_str.push_str(&format!("{statement}"));
                }
                ret_str.push_str(" ]");
                expected.assert_eq(&ret_str);
            }
            Err(e) => {
                eprintln!("{e}");
                assert!(false);
            }
        }
    }
}
mod errors {
    use super::*;

    #[test]
    fn func_no_body() {
        let code: &'static str = r#"fn(x,y)"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected = expect_file!["./../tests/expect_test_results/func_no_body.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn two_expressions() {
        let code: &'static str = r#"x fn(){true}"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected = expect_file!["./../tests/expect_test_results/two_expressions.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn call_expression_bad_semicolon() {
        let code: &'static str = r#"add(x; y)"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected = expect_file![
                    "./../tests/expect_test_results/call_expression_bad_semicolon.txt"
                ];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn multiple_non_terminated_exprs() {
        let code: &'static str = r#"x x"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected = expect_file![
                    "./../tests/expect_test_results/multiple_non_terminated_exprs.txt"
                ];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn new_scope_bad_semicolon() {
        let code: &'static str = r#"
        {
            ;
        }"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected =
                    expect_file!["./../tests/expect_test_results/new_scope_bad_semicolon.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn assign_statement_no_semicolon() {
        let code: &'static str = r#"x = true"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected = expect_file![
                    "./../tests/expect_test_results/assign_statement_no_semicolon.txt"
                ];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn let_statement_no_semicolon() {
        let code: &'static str = r#"let x = true"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected =
                    expect_file!["./../tests/expect_test_results/let_statement_no_semicolon.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn return_no_semicolon() {
        let code: &'static str = r#"return 5"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected =
                    expect_file!["./../tests/expect_test_results/return_no_semicolon.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }

    #[test]
    fn return_no_expr_no_semicolon() {
        let code: &'static str = r#"return"#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        match parse(&mut peek_lex) {
            Ok(statements) => {
                assert!(false, "Expected an error, found {statements:#?}");
            }
            Err(e) => {
                let err_string = format!("{e:#?}");
                let expected =
                    expect_file!["./../tests/expect_test_results/return_no_expr_no_semicolon.txt"];
                expected.assert_eq(&err_string);
            }
        }
    }
}
