#![cfg(test)]
use crate::evaluator::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&5)
    );
}

#[test]
fn bang_prefix_int_expr() {
    let code: &'static str = r#"!3"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&-4)
    );
}

#[test]
fn single_bool_expr() {
    let code: &'static str = r#"false"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn bang_prefix_bool_expr() {
    let code: &'static str = r#"!true"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn recursive_bang_prefix_bool_expr() {
    let code: &'static str = r#"!!true"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn minus_prefix_int_expr() {
    let code: &'static str = r#"-5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&-5)
    );
}

#[test]
fn recursive_minus_prefix_int_expr() {
    let code: &'static str = r#"----5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&5)
    );
}

#[test]
fn int_binary_expression() {
    let code: &'static str = r#"(5 + 10 * 2 + 15 / 3) * 2 + -10"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&50)
    );
}

#[test]
fn int_greater_than_int() {
    let code: &'static str = r#"3 > 5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn int_less_than_int() {
    let code: &'static str = r#"3 < 5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn int_equal_to_int() {
    let code: &'static str = r#"3 == 5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn int_not_equal_to_int() {
    let code: &'static str = r#"3 != 5"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn true_equal_to_true() {
    let code: &'static str = r#"true == true"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn true_not_equal_to_true() {
    let code: &'static str = r#"true != true"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn false_equal_to_false() {
    let code: &'static str = r#"false == false"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&true)
    );
}

#[test]
fn false_equal_to_true() {
    let code: &'static str = r#"false == true"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env)
            .unwrap()
            .inner()
            .downcast_ref::<bool>(),
        Some(&false)
    );
}

#[test]
fn if_true() {
    let code: &'static str = r#"if true {10}"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&10)
    );
}

#[test]
fn if_false_else() {
    let code: &'static str = r#"if false {10} else {22}"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&22)
    );
}

#[test]
fn else_if() {
    let code: &'static str = r#"if false {10} else if false {22} else {56}"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&56)
    );
}

#[test]
fn early_return() {
    let code: &'static str = r#"5 * 5 * 5; return 10; 9 * 9 * 9;"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&10)
    );
}

#[test]
fn let_ident_x() {
    let code: &'static str = r#"let x = 29; x"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&29)
    );
}

#[test]
fn let_idents_x_y() {
    let code: &'static str = r#"let x = 29; let y = 29; x / y"#;
    let lexer = Lexer::new(code);
    let env: Environment = Rc::new(RefCell::new(HashMap::new()));
    let statements = parse(lexer).unwrap();
    assert_eq!(
        eval(statements, env).unwrap().inner().downcast_ref::<i64>(),
        Some(&1)
    );
}
