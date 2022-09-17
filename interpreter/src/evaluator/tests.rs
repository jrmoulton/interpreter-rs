#![cfg(test)]

#[allow(unused_imports)]
use crate::{
    evaluator::eval,
    lexer::Lexer,
    object::{self, ObjectTrait},
    parser::parse,
};

#[test]
fn single_int_expr() {
    let code: &'static str = r#"5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<i64>(),
        Some(&5)
    );
}

#[test]
fn bang_prefix_int_expr() {
    let code: &'static str = r#"!3"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<i64>(),
        Some(&-4)
    );
}

#[test]
fn single_bool_expr() {
    let code: &'static str = r#"false"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn bang_prefix_bool_expr() {
    let code: &'static str = r#"!true"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn recursive_bang_prefix_bool_expr() {
    let code: &'static str = r#"!!true"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn minus_prefix_int_expr() {
    let code: &'static str = r#"-5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<i64>(),
        Some(&-5)
    );
}

#[test]
fn recursive_minus_prefix_int_expr() {
    let code: &'static str = r#"----5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<i64>(),
        Some(&5)
    );
}

#[test]
fn int_binary_expression() {
    let code: &'static str = r#"(5 + 10 * 2 + 15 / 3) * 2 + -10"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<i64>(),
        Some(&50)
    );
}

#[test]
fn int_greater_than_int() {
    let code: &'static str = r#"3 > 5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn int_less_than_int() {
    let code: &'static str = r#"3 < 5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn int_equal_to_int() {
    let code: &'static str = r#"3 == 5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn int_not_equal_to_int() {
    let code: &'static str = r#"3 != 5"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn true_equal_to_true() {
    let code: &'static str = r#"true == true"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn true_not_equal_to_true() {
    let code: &'static str = r#"true != true"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn false_equal_to_false() {
    let code: &'static str = r#"false == false"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn false_equal_to_true() {
    let code: &'static str = r#"false == true"#;
    let lexer = Lexer::new(code);
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements).unwrap().inner().downcast_ref::<bool>(),
        Some(&false)
    );
}
