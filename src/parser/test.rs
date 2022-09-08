use crate::lexer::Lexer;

use super::*;
use assert_matches::assert_matches;
use expect_test::{expect, expect_file};
use pretty_assertions::assert_eq;

#[test]
fn simple_prefix_op_expression_statement() {
    let code: &'static str = r#"-5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let statement = statements.iter().next();
            match statement {
                Some(Statement::Expression(Expr::PrefixExpression(PreExpr {
                    operator:
                        LocTok {
                            token: Token::Minus,
                            ..
                        },
                    expression,
                }))) => match expression.as_ref() {
                    Expr::IntLiteral(LocTok { token, .. }) => {
                        assert_eq!(*token, Token::Int(5))
                    }
                    _ => assert!(false, "Expected a 5 after the minus"),
                },
                _ => {
                    assert!(false, "Expected a minus (-) 5, found {statement:?}")
                }
            };
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}
#[test]
fn single_let() {
    let code: &'static str = r#"let x = 5;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            assert_matches!(
                statements.iter().next(),
                Some(Statement::Let(LetStatement {
                    ident: LocTok {
                        token: Token::Ident(_),
                        ..
                    },
                    expr: Expr::IntLiteral(LocTok {
                        token: Token::Int(5),
                        ..
                    }),
                }))
            );
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn single_let_with_bool() {
    let code: &'static str = r#"let x = true;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            assert_matches!(
                statements.iter().next(),
                Some(Statement::Let(LetStatement {
                    ident: LocTok {
                        token: Token::Ident(_),
                        ..
                    },
                    expr: Expr::BoolLiteral(LocTok {
                        token: Token::True,
                        ..
                    }),
                }))
            );
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn single_let_with_add() {
    let code: &'static str = r#"let x = 10 + 3;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let next = statements.iter().next();
            match next {
                Some(statement) => {
                    match statement {
                        Statement::Let(LetStatement { ident, expr }) => {
                            assert_eq!(ident.token, Token::Ident("x".into()));
                            match expr {
                                Expr::BinaryExpression(BinExp { lhs, operator, rhs }) => {
                                    assert_eq!(operator.token, Token::Plus);
                                    match lhs.as_ref() {
                                        Expr::IntLiteral(LocTok { token, .. }) => {
                                            assert_eq!(*token, Token::Int(10));
                                        }
                                        _ => {
                                            assert!(false);
                                        }
                                    };
                                    match rhs.as_ref() {
                                        Expr::IntLiteral(LocTok { token, .. }) => {
                                            assert_eq!(*token, Token::Int(3));
                                        }
                                        _ => {
                                            assert!(false);
                                        }
                                    };
                                }
                                _ => {
                                    assert!(false, "Found {expr:?} should have been an AddExpr");
                                }
                            };
                        }
                        _ => {
                            assert!(false, "Found {statement:?} should have been a Statement::Let(LetStatement)");
                        }
                    }
                }
                _ => {
                    assert!(false, "The parser didn't return any values");
                }
            };
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn single_operator_precedence_expression_statement() {
    let code: &'static str = r#"5 + 5 * 5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            match statements.next() {
                Some(Statement::Expression(Expr::BinaryExpression(BinExp {
                    lhs,
                    operator,
                    rhs,
                }))) => {
                    match lhs.as_ref() {
                        Expr::IntLiteral(LocTok { token, .. }) => {
                            assert_eq!(*token, Token::Int(5))
                        }
                        _ => assert!(false, "Expected lhs in addition to be a 5 got a {lhs:?}"),
                    }
                    assert_eq!(operator.token, Token::Plus);
                    match rhs.as_ref() {
                        Expr::BinaryExpression(BinExp { lhs, operator, rhs }) => {
                            match lhs.as_ref() {
                                Expr::IntLiteral(LocTok { token, .. }) => {
                                    assert_eq!(*token, Token::Int(5))
                                }
                                _ => assert!(
                                    false,
                                    "Expected lhs in multiply to be a 5 got a {lhs:?}"
                                ),
                            }
                            assert_eq!(operator.token, Token::Asterisk);
                            match rhs.as_ref() {
                                Expr::IntLiteral(LocTok { token, .. }) => {
                                    assert_eq!(*token, Token::Int(5))
                                }
                                _ => assert!(
                                    false,
                                    "Expected rhs in multiply to be a 5 got a {rhs:?}"
                                ),
                            }
                        }
                        _ => assert!(
                            false,
                            "Expected rhs in addition to be a multiply expression got a {rhs:?}"
                        ),
                    }
                }
                _ => assert!(false, "Expected an expression"),
            };
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}
#[test]
fn operator_precedence_with_grouped_expressions() {
    let code: &'static str = r#"(5 + 5) * 5"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            match statements.next() {
                Some(Statement::Expression(Expr::BinaryExpression(BinExp {
                    lhs,
                    operator,
                    rhs,
                }))) => {
                    match lhs.as_ref() {
                        Expr::BinaryExpression(BinExp { lhs, operator, rhs }) => {
                            match lhs.as_ref() {
                                Expr::IntLiteral(LocTok { token, .. }) => {
                                    assert_eq!(*token, Token::Int(5))
                                }
                                _ => assert!(
                                    false,
                                    "Expected lhs in grouped addition to be a 5 got a {lhs:?}"
                                ),
                            }
                            assert_eq!(operator.token, Token::Plus);
                            match rhs.as_ref() {
                                Expr::IntLiteral(LocTok { token, .. }) => {
                                    assert_eq!(*token, Token::Int(5))
                                }
                                _ => assert!(
                                    false,
                                    "Expected rhs in grouped addition to be a 5 got a {rhs:?}"
                                ),
                            }
                        }
                        _ => assert!(false, "Expected lhs in expression to be a grouped addition expression got a {lhs:?}"),
                    }
                    assert_eq!(operator.token, Token::Asterisk);
                    match rhs.as_ref() {
                        Expr::IntLiteral(LocTok { token, .. }) => {
                            assert_eq!(*token, Token::Int(5))
                        }
                        _ => assert!(
                            false,
                            "Expected rhs in multiplication to be a 5 got a {rhs:?}"
                        ),
                    }
                }
                _ => assert!(false, "Expected an expression"),
            };
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn single_int_expression() {
    let code: &'static str = r#"35"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            assert_matches!(
                statements.next(),
                Some(Statement::Expression(Expr::IntLiteral(LocTok {
                    token: Token::Int(35),
                    ..
                }),))
            );
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn basic_return() {
    let code: &'static str = r#"return 35;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            assert_matches!(
                statements.next(),
                Some(Statement::Return(Expr::IntLiteral(LocTok {
                    token: Token::Int(35),
                    ..
                }),))
            );
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn identifier_expression() {
    let code: &'static str = r#"foobar"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            let statement = statements.next();
            match statement {
                Some(Statement::Expression(Expr::Ident(LocTok { token, .. }))) => {
                    assert_eq!(*token, Token::Ident("foobar".into()));
                }
                _ => assert!(false),
            };
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

#[test]
fn double_let() {
    let code: &'static str = r#"let x = 5;
        let y = 3;"#;
    let lexer = Lexer::new(code.as_bytes(), code.len());
    match parse(lexer) {
        Ok(statements) => {
            let mut statements = statements.iter();
            assert_matches!(
                statements.next(),
                Some(Statement::Let(LetStatement {
                    ident: LocTok {
                        token: Token::Ident(_),
                        ..
                    },
                    expr: Expr::IntLiteral(LocTok {
                        token: Token::Int(5),
                        ..
                    }),
                }))
            );
            assert_matches!(
                statements.next(),
                Some(Statement::Let(LetStatement {
                    ident: LocTok {
                        token: Token::Ident(_),
                        ..
                    },
                    expr: Expr::IntLiteral(LocTok {
                        token: Token::Int(3),
                        ..
                    }),
                }))
            );
        }
        Err(e) => {
            eprintln!("{e}");
            assert!(false);
        }
    };
}

// This test makes it obvious that I need to invest in better testing infrastructure
// This test alone is 123 lines of code
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
            let statement = statements.iter().next().expect("At least 1 statement");
            match statement {
                Statement::Expression(Expr::If(IfExpr {
                    condition,
                    consequence,
                    alternative,
                })) => {
                    match condition.as_ref() {
                        Expr::Ident(lok_tok) => {
                            assert_eq!(lok_tok.token, Token::Ident("x".into()));
                        }
                        _ => {
                            assert!(false, "Expected an identifier `x`, found a {condition:?}")
                        }
                    };
                    let consequence = consequence
                        .first()
                        .expect("At least 1 statement in the consequence");
                    match consequence {
                        Statement::Expression(Expr::Ident(ident)) => {
                            assert_eq!(ident.token, Token::Ident("x".into()))
                        }
                        _ => {
                            assert!(
                                false,
                                "Expected an identifier expression `x`, found {consequence:?}"
                            )
                        }
                    };
                    match alternative.as_ref().expect("An else if alternative") {
                        ElseIfExpr::ElseIf(inner) => match inner.as_ref() {
                            Expr::If(IfExpr {
                                condition,
                                consequence,
                                alternative,
                            }) => {
                                match condition.as_ref() {
                                    Expr::Ident(lok_tok) => {
                                        assert_eq!(lok_tok.token, Token::Ident("y".into()));
                                    }
                                    _ => {
                                        assert!(
                                            false,
                                            "Expected an identifier `y`, found a {condition:?}"
                                        )
                                    }
                                };
                                let consequence = consequence
                                    .first()
                                    .expect("At least 1 statement in the consequence");
                                match consequence {
                                    Statement::Expression(Expr::Ident(ident)) => {
                                        assert_eq!(ident.token, Token::Ident("y".into()))
                                    }
                                    _ => {
                                        assert!(
                                false,
                                "Expected an identifier expression `y`, found {consequence:?}"
                            )
                                    }
                                };
                                match alternative.as_ref().expect("An else alternative") {
                                    ElseIfExpr::Else(statements) => {
                                        dbg!(alternative);
                                        let statement =
                                            statements.iter().next().expect("At least 1 statement");
                                        match statement {
                                            Statement::Expression(Expr::Ident(ident)) => {
                                                assert_eq!(
                                                    ident.token,
                                                    Token::Ident("foobar".into())
                                                )
                                            }
                                            _ => {
                                                assert!(false, "Expected an identifier `foobar, found a {statement:?}")
                                            }
                                        }
                                    }
                                    _ => {
                                        assert!(
                                            false,
                                            "Expected an Else If expression, found a {statement:?}"
                                        )
                                    }
                                };
                            }

                            _ => {
                                assert!(false, "Expected an If expression, found something else")
                            }
                        },
                        _ => {
                            assert!(
                                false,
                                "Expected an Else If expression, found something else"
                            )
                        }
                    };
                }
                _ => {
                    assert!(false, "Expected an If expression, found a {statement:?}")
                }
            }
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
