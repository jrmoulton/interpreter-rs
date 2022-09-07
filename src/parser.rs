use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

use crate::lexer::{Lexer, LocTok, Precedence, Token};
use error_stack::{Context, Report, Result};

#[derive(Debug)]
pub(crate) struct BinExp {
    lhs: Box<Expr>,
    operator: LocTok,
    rhs: Box<Expr>,
}

#[derive(Debug)]
pub(crate) struct PreExpr {
    operator: LocTok,
    expression: Box<Expr>,
}

#[derive(Debug)]
pub(crate) enum Expr {
    IntLiteral(LocTok),
    BoolLiteral(LocTok),
    Ident(LocTok),
    PrefixExpression(PreExpr),
    BinaryExpression(BinExp),
}

#[derive(Debug)]
pub(crate) struct LetStatement {
    ident: LocTok,
    expr: Expr,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(Expr),
    Expression(Expr),
}

#[derive(Debug)]
pub(crate) enum ParseError {
    UnexpectedToken(LocTok),
    Eof,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for ParseError {}
impl From<Report<ParseError>> for ParseErrors {
    fn from(parse_error: Report<ParseError>) -> Self {
        ParseErrors(vec![parse_error])
    }
}
#[derive(Debug)]
pub(crate) struct ParseErrors(Vec<Report<ParseError>>);
impl Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for ParseErrors {}

type LexerPeekRef<'a> = Rc<RefCell<Peekable<Lexer<'a>>>>;

pub(crate) fn parse(lexer: Lexer) -> Result<Vec<Statement>, ParseErrors> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    let lexer_ref = Rc::new(RefCell::new(lexer.peekable()));
    let mut start_statement_peek = lexer_ref.borrow_mut().peek().map(|val| val.to_owned());
    while let Some(lok_tok) = start_statement_peek {
        match lok_tok.token {
            Token::Let => match parse_let_statement(lexer_ref.clone()) {
                Ok(statement) => statements.push(Statement::Let(statement)),
                Err(e) => errors.extend(e.0),
            },
            Token::Return => match parse_return_statement(lexer_ref.clone()) {
                Ok(statement) => statements.push(Statement::Return(statement)),
                Err(e) => errors.extend(e.0),
            },
            Token::Int(_) | Token::If | Token::Ident(_) | Token::LParen => {
                match parse_expression_statement(lexer_ref.clone()) {
                    Ok(statement) => statements.push(Statement::Expression(statement)),
                    Err(e) => errors.extend(e.0),
                }
            }
            _ => errors.push(
                Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach_printable("Expected a statement or an expression"),
            ),
        };
        // Need to call next because the semicolon was only ever peeked
        lexer_ref.borrow_mut().next();
        start_statement_peek = lexer_ref.borrow_mut().peek().map(|val| val.to_owned());
    }
    if !errors.is_empty() {
        Err(Report::new(ParseErrors(errors)))
    } else {
        Ok(statements)
    }
}

fn parse_return_statement(lexer: LexerPeekRef) -> std::result::Result<Expr, ParseErrors> {
    lexer
        .borrow_mut()
        .next()
        .expect("The return keyword was already peeked and matched");
    let mut errors = Vec::new();
    let expr = match parse_expression(lexer.clone(), Precedence::Lowest) {
        Ok(expr) => Some(expr),
        Err(e) => {
            errors.extend(e.0);
            None
        }
    };
    match expect_peek(lexer.clone(), Token::Semicolon) {
        Ok(_) => {}
        Err(e) => errors.push(e),
    }
    if errors.is_empty() {
        Ok(expr.expect("Expression there because there are no errors"))
    } else {
        Err(ParseErrors(errors))
    }
}

/// This assumes that the let keyword has already been checked and that the next token in the
/// iterator is an identifier
fn parse_let_statement(lexer: LexerPeekRef) -> std::result::Result<LetStatement, ParseErrors> {
    lexer
        .borrow_mut()
        .next()
        .expect("The let keyword was already peeked and matched");
    let mut errors = Vec::new();
    let peek = expect_peek(lexer.clone(), Token::Ident(String::default()));
    let ident = match peek {
        Ok(_) => match parse_identifier(lexer.clone()) {
            Ok(ident) => Some(ident),
            Err(e) => {
                errors.push(e);
                None
            }
        },
        Err(e) => {
            errors.push(e);
            None
        }
    };
    let expect_assign = expect_peek(lexer.clone(), Token::Assign);
    match expect_assign {
        Ok(_) => {
            lexer.borrow_mut().next();
        }
        Err(e) => {
            errors.push(e);
        }
    };
    let expr = match parse_expression(lexer.clone(), Precedence::Lowest) {
        Ok(expr) => Some(expr),
        Err(e) => {
            errors.extend(e.0);
            None
        }
    };
    match expect_peek(lexer.clone(), Token::Semicolon) {
        Ok(_) => {}
        Err(e) => errors.push(e),
    }

    if errors.is_empty() {
        Ok(LetStatement {
            ident: ident.expect("Some because there are no errors"),
            expr: expr.expect("Some because there are no errors"),
        })
    } else {
        Err(ParseErrors(errors))
    }
}

fn expect_peek(lexer: LexerPeekRef, expected: Token) -> Result<(), ParseError> {
    let peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    match peek {
        Some(lok_tok) => {
            if lok_tok.token.token_matches(&expected) {
                Ok(())
            } else {
                Err(Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach_printable(format!("Expected a {expected:?}")))
            }
        }
        None => {
            Err(Report::new(ParseError::Eof).attach_printable(format!("Expected a {expected:?}")))
        }
    }
}

fn parse_identifier(lexer: LexerPeekRef) -> Result<LocTok, ParseError> {
    let next = lexer.borrow_mut().next();
    match next {
        Some(lok_tok) => match lok_tok.token {
            Token::Ident(_) => Ok(lok_tok),
            _ => Err(Report::new(ParseError::UnexpectedToken(lok_tok))
                .attach_printable("Expected an identifier")),
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an identifier")),
    }
}

fn parse_expression_statement(lexer: LexerPeekRef) -> std::result::Result<Expr, ParseErrors> {
    parse_expression(lexer.clone(), Precedence::Lowest)
    // TODO: Handle optional semicolon here
}

fn parse_expression(
    lexer: LexerPeekRef,
    precedence: Precedence,
) -> std::result::Result<Expr, ParseErrors> {
    use Token::*;
    let lhs_val_peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    let mut left_exp = match lhs_val_peek {
        Some(left_lok_tok) => match left_lok_tok.token {
            // All Literals, identifiers and prefix operators should be matched here
            Ident(_) => {
                lexer.borrow_mut().next();
                Expr::Ident(left_lok_tok)
            }
            Int(_) => {
                lexer.borrow_mut().next();
                Expr::IntLiteral(left_lok_tok)
            }
            True | False => {
                lexer.borrow_mut().next(); // The token was only peeked but we are now handling it
                                           // so skip it here
                Expr::BoolLiteral(left_lok_tok)
            }
            Bang | Token::Minus => {
                lexer.borrow_mut().next(); // This skips the operator
                parse_prefix_expression(lexer.clone())?
            }
            LParen => {
                lexer.borrow_mut().next(); // This skips the lparen
                parse_grouped_expression(lexer.clone())?
            }
            _ => Err(Report::new(ParseError::UnexpectedToken(left_lok_tok))
                .attach_printable("Expected an expression"))?,
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression"))?,
    };

    // In correct code the peek token is an operator
    let mut peek_op_token = match lexer.borrow_mut().peek().map(|val| val.to_owned()) {
        Some(token) => token,
        None => return Ok(left_exp),
    };
    while precedence < peek_op_token.token.precedence() {
        left_exp = match peek_op_token.token {
            // All binary tokens should be matched here
            Plus | Minus | Slash | Asterisk | Eq | Ne | LT | GT | Assign => {
                parse_binary_expression(lexer.clone(), left_exp)?
            }
            _ => Err(Report::new(ParseError::UnexpectedToken(peek_op_token))
                .attach_printable("Expected a binary operator"))?,
        };
        peek_op_token = match lexer.borrow_mut().peek().map(|val| val.to_owned()) {
            Some(token) => token,
            None => return Ok(left_exp),
        };
    }
    Ok(left_exp)
}

fn parse_grouped_expression(lexer: LexerPeekRef) -> std::result::Result<Expr, ParseErrors> {
    let mut errors = Vec::new();
    // let _lparen = lexer.borrow_mut().next();
    let exp = match parse_expression(lexer.clone(), Precedence::Lowest) {
        Ok(exp) => Some(exp),
        Err(e) => {
            errors.extend(e.0);
            None
        }
    };
    if let Err(e) = expect_peek(lexer.clone(), Token::RParen) {
        errors.push(e);
    } else {
        let _rparen = lexer.borrow_mut().next();
    }
    if !errors.is_empty() {
        Err(ParseErrors(errors))
    } else {
        Ok(exp.expect("If there are no errors then the expression is present"))
    }
}

fn parse_prefix_expression(lexer: LexerPeekRef) -> std::result::Result<Expr, ParseErrors> {
    let operator = lexer
        .borrow_mut()
        .next()
        .expect("The operator was already peeked and found");
    let right = lexer.borrow_mut().peek().map(|val| val.to_owned());
    let expression = match right {
        Some(_) => parse_expression(lexer, operator.token.precedence())?,
        None => Err(Report::new(ParseError::Eof).attach_printable(format!(
            "Expected an operand after the prefix operator {:?} at {operator:?}",
            operator.token
        )))?,
    };
    Ok(Expr::PrefixExpression(PreExpr {
        operator,
        expression: Box::new(expression),
    }))
}

fn parse_binary_expression(
    lexer: LexerPeekRef,
    left: Expr,
) -> std::result::Result<Expr, ParseErrors> {
    let operator = lexer
        .borrow_mut()
        .next()
        .expect("The operator was already peeked and found");
    let op_precedence = operator.token.precedence();
    Ok(Expr::BinaryExpression(BinExp {
        lhs: Box::new(left),
        operator,
        rhs: Box::new(parse_expression(lexer.clone(), op_precedence)?),
    }))
}

/// This is a test mod
/// I think I want to move it to it's own folder
#[cfg(test)]
mod test {
    use crate::lexer::Lexer;

    use super::*;
    use assert_matches::assert_matches;

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
                println!("{e}");
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
                println!("{e}");
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
                    Some(statement) => match statement {
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
                    },
                    _ => {
                        assert!(false, "The parser didn't return any values");
                    }
                };
            }
            Err(e) => {
                println!("{e}");
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
                            _ => assert!(false, "Expected rhs in addition to be a multiply expression got a {rhs:?}"),
                        }
                    }
                    _ => assert!(false, "Expected an expression"),
                };
            }
            Err(e) => {
                println!("{e}");
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
                println!("{e}");
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
                println!("{e}");
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
                println!("{e}");
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
                println!("{e}");
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
                println!("{e}");
                assert!(false);
            }
        };
    }
}
