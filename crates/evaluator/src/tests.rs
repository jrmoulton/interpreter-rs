#![cfg(test)]
use std::sync::Arc;

use lexer::{Lexer, PeekLex};
use parser::parse;

#[allow(unused_imports)]
use crate::{eval, Environment};

mod results {

    use super::*;
    use crate::EvalObj;

    fn get_inner_helper(code: &'static str) -> EvalObj {
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        let env = Arc::new(Environment::default());
        let statements = parse(&mut peek_lex).unwrap();
        eval(statements, env.clone()).unwrap()
    }

    #[test]
    fn single_int_expr() {
        let code: &'static str = r#"5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&5)
        );
    }

    #[test]
    fn bang_prefix_int_expr() {
        let code: &'static str = r#"!3"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&-4)
        );
    }

    #[test]
    fn single_bool_expr() {
        let code: &'static str = r#"false"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn bang_prefix_bool_expr() {
        let code: &'static str = r#"!true"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn recursive_bang_prefix_bool_expr() {
        let code: &'static str = r#"!!true"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&true)
        );
    }

    #[test]
    fn minus_prefix_int_expr() {
        let code: &'static str = r#"-5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&-5)
        );
    }

    #[test]
    fn recursive_minus_prefix_int_expr() {
        let code: &'static str = r#"----5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&5)
        );
    }

    #[test]
    fn int_binary_expression() {
        let code: &'static str = r#"(5 + 10 * 2 + 15 / 3) * 2 + -10"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&50)
        );
    }

    #[test]
    fn int_greater_than_int() {
        let code: &'static str = r#"3 > 5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn int_less_than_int() {
        let code: &'static str = r#"3 < 5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&true)
        );
    }

    #[test]
    fn int_equal_to_int() {
        let code: &'static str = r#"3 == 5"#;

        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn int_not_equal_to_int() {
        let code: &'static str = r#"3 != 5"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&true)
        );
    }

    #[test]
    fn true_equal_to_true() {
        let code: &'static str = r#"true == true"#;

        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&true)
        );
    }

    #[test]
    fn true_not_equal_to_true() {
        let code: &'static str = r#"true != true"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn false_equal_to_false() {
        let code: &'static str = r#"false == false"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&true)
        );
    }

    #[test]
    fn false_equal_to_true() {
        let code: &'static str = r#"false == true"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<bool>(),
            Some(&false)
        );
    }

    #[test]
    fn if_true() {
        let code: &'static str = r#"if true {10}"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&10)
        );
    }

    #[test]
    fn if_false_else() {
        let code: &'static str = r#"if false {10} else {22}"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&22)
        );
    }

    #[test]
    fn else_if() {
        let code: &'static str = r#"if false {10} else if false {22} else {56}"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&56)
        );
    }

    #[test]
    fn early_return() {
        let code: &'static str = r#"5 * 5 * 5; return 10; 9 * 9 * 9;"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&10)
        );
    }

    #[test]
    fn early_return_nested_scope() {
        let code: &'static str = r#"
        {
        let x = 5 + {
                return 3;
            };
        }
        "#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&3)
        );
    }

    #[test]
    fn let_ident_x() {
        let code: &'static str = r#"let x = 29; x"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&29)
        );
    }

    #[test]
    fn let_idents_x_y() {
        let code: &'static str = r#"let x = 29; let y = 29; x / y"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&1)
        );
    }

    #[test]
    fn function_with_call() {
        let code: &'static str = r#"let add = fn(a, b, c, d) {a + b + c + d}; add(1, 2, 3, 4)"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&10)
        );
    }

    #[test]
    fn closure_call() {
        let code: &'static str =
            r#"let new_adder = fn(x) { fn(y) {x + y}}; let add_two = new_adder(2); add_two(2)"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&4)
        );
    }

    #[test]
    fn string() {
        let code: &'static str = r#"let x = "foo bar"; x"#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<String>(),
            Some(&"foo bar".into())
        );
    }

    #[test]
    fn string_addition() {
        let code: &'static str = r#" "3" + "3" "#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<String>(),
            Some(&"33".into())
        );
    }

    // #[test]
    // fn string_len_method() {
    //     let code: &'static str = r#" "Hello world!".len() "#;
    //     assert_eq!(
    //         get_inner_helper(code).inner().downcast_ref::<i64>(),
    //         Some(&12)
    //     );
    // }

    #[test]
    fn int_array_index() {
        let code: &'static str = r#" [1, 2, 3, 4][2] "#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<i64>(),
            Some(&3)
        );
    }

    #[test]
    fn string_index() {
        let code: &'static str = r#" "foobar"[0] "#;
        assert_eq!(
            get_inner_helper(code).inner().downcast_ref::<String>(),
            Some(&"f".into())
        );
    }
}

mod errors {
    use expect_test::expect_file;

    use super::*;

    #[test]
    fn index_out_of_bounds_string() {
        let code: &'static str = r#" "foo"[3] "#;
        let lexer = Lexer::new(code.into());
        let mut peek_lex = PeekLex::new(lexer);
        let statements = parse(&mut peek_lex).unwrap();
        let env = Arc::new(Environment::default());
        match eval(statements, env.clone()) {
            Ok(object) => {
                assert!(false, "Expected an error, found object{object:#?}");
            },
            Err(errs) => {
                let err_string = format!("{errs}");
                let expected =
                    expect_file!["./../tests/expect_test_results/index_out_of_bounds_string.txt"];
                expected.assert_eq(&err_string);
            },
        }
    }
}
