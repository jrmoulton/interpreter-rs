use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

use crate::lexer::{Lexer, LocTok, Token};
use error_stack::{Context, Report, Result};

#[derive(Debug)]
struct AddExpr {
    lhs: LocTok,
    rhs: Box<Expr>,
}

#[derive(Debug)]
struct SubExpr {
    lhs: LocTok,
    op: LocTok,
    rhs: Expr,
}

#[derive(Debug)]
struct MultExpression {
    lhs: LocTok,
    op: LocTok,
    rhs: Expr,
}

#[derive(Debug)]
struct DivExpression {
    lhs: LocTok,
    op: LocTok,
    rhs: Expr,
}

#[derive(Debug)]
enum Expr {
    Base(LocTok),
    AdditionExpression(AddExpr),
}

#[derive(Debug)]
pub(crate) struct LetStatement {
    ident: LocTok,
    expr: Expr,
}

#[derive(Debug)]
pub(crate) struct ReturnStatement {
    expr: Expr,
}

#[derive(Debug)]
pub(crate) enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
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
    let mut next = lexer_ref.borrow_mut().next();
    while let Some(lok_tok) = next {
        match lok_tok.token {
            Token::Let => match parse_let_statement(lexer_ref.clone()) {
                Ok(statement) => statements.push(Statement::Let(statement)),
                Err(e) => errors.extend(e.0),
            },
            Token::Return => match parse_return_statement(lexer_ref.clone()) {
                Ok(statement) => statements.push(Statement::Return(statement)),
                Err(e) => errors.extend(e.0),
            },
            _ => {
                errors.push(
                    Report::new(ParseError::UnexpectedToken(lok_tok))
                        .attach_printable("Expected a statement such as `Let` or `Return` "),
                );
            }
        };
        // Need to call twice because the semicolon was only ever peeked
        lexer_ref.borrow_mut().next();
        next = lexer_ref.borrow_mut().next();
    }
    if !errors.is_empty() {
        Err(Report::new(ParseErrors(errors)))
    } else {
        Ok(statements)
    }
}

fn parse_return_statement(
    lexer: LexerPeekRef,
) -> std::result::Result<ReturnStatement, ParseErrors> {
    let mut errors = Vec::new();
    let expr = match parse_expression(lexer.clone()) {
        Ok(expr) => Some(expr),
        Err(e) => {
            errors.push(e);
            None
        }
    };
    if errors.is_empty() {
        Ok(ReturnStatement {
            expr: expr.expect("Some because there are no errors"),
        })
    } else {
        Err(ParseErrors(errors))
    }
}

/// This assumes that the let keyword has already been checked and that the next token in the
/// iterator is an identifier
fn parse_let_statement(lexer: LexerPeekRef) -> std::result::Result<LetStatement, ParseErrors> {
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
    let peek = expect_peek(lexer.clone(), Token::Assign);
    match peek {
        Ok(_) => {
            lexer.borrow_mut().next();
        }
        Err(e) => {
            errors.push(e);
        }
    };
    let expr = match parse_expression(lexer.clone()) {
        Ok(expr) => Some(expr),
        Err(e) => {
            errors.push(e);
            None
        }
    };

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
fn parse_expression(lexer: LexerPeekRef) -> Result<Expr, ParseError> {
    let next = lexer.borrow_mut().next();
    match next {
        Some(lhs_lok_tok) => match lhs_lok_tok.token {
            Token::Int(_) | Token::If | Token::Func => {
                let peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
                match peek {
                    // When do expressions end?
                    Some(peek_lok_tok) => match peek_lok_tok.token {
                        Token::Semicolon => {
                            Ok(Expr::Base(lhs_lok_tok))
                        }
                        _ => Ok(parse_expression_op(lexer.clone(), lhs_lok_tok)?),
                    },
                    None => {
                        Err(Report::new(ParseError::Eof).attach_printable("Expected a semicolon"))
                    }
                }
            }
            _ => Err(Report::new(ParseError::UnexpectedToken(lhs_lok_tok))
                .attach_printable("Expected the lhs of an expression to be a valid lvalue (evaluatable to a single value)")),
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression")),
    }
}

fn parse_expression_op(lexer: LexerPeekRef, lhs: LocTok) -> Result<Expr, ParseError> {
    let next = lexer
        .borrow_mut()
        .next()
        .expect("The op token was previously found")
        .token;
    match next {
        // Token::If => {},
        Token::Plus => Ok(parse_expression_plus(lexer.clone(), lhs)?),
        // Token::Minus => {},
        // Token::Mult => {},
        _ => unimplemented!(),
    }
}

fn parse_expression_plus(lexer: LexerPeekRef, lhs: LocTok) -> Result<Expr, ParseError> {
    let next = lexer.borrow_mut().next();
    match next {
        Some(expr_lok_tok) => match lexer.borrow_mut().peek() {
            Some(qsemi_lok_tok) => match qsemi_lok_tok.token {
                Token::Semicolon => Ok(Expr::AdditionExpression(AddExpr {
                    lhs,
                    rhs: Box::new(Expr::Base(expr_lok_tok)),
                })),
                _ => Ok(Expr::AdditionExpression(AddExpr {
                    lhs,
                    rhs: Box::new(parse_expression_op(lexer.clone(), expr_lok_tok)?),
                })),
            },
            None => match expr_lok_tok.token {
                Token::Semicolon => {
                    Err(Report::new(ParseError::Eof).attach_printable("Expected an expression"))
                }
                _ => Err(Report::new(ParseError::Eof).attach_printable("Expected a semicolon")),
            },
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression")),
    }
}

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
                        expr: Expr::Base(LocTok {
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
                                Expr::AdditionExpression(AddExpr { lhs, rhs }) => {
                                    assert_eq!(lhs.token, Token::Int(10));
                                    match rhs.as_ref() {
                                        Expr::Base(LocTok { token, .. }) => {
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
    fn basic_return() {
        let code: &'static str = r#"return 35;"#;
        let lexer = Lexer::new(code.as_bytes(), code.len());
        match parse(lexer) {
            Ok(statements) => {
                let mut statements = statements.iter();
                assert_matches!(
                    statements.next(),
                    Some(Statement::Return(ReturnStatement {
                        expr: Expr::Base(LocTok {
                            token: Token::Int(35),
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
                        expr: Expr::Base(LocTok {
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
                        expr: Expr::Base(LocTok {
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
