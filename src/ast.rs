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
pub(crate) enum Statement {
    Let(LetStatement),
}

#[derive(Debug)]
pub(crate) enum ParseError {
    UnexpectedToken(LocTok),
    Eof,
}
impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("self:?"))
    }
}
impl Context for ParseError {}
#[derive(Debug)]
pub(crate) struct ParseErrors(Vec<Report<ParseError>>);
impl Display for ParseErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("self:?"))
    }
}
impl Context for ParseErrors {}

type LexerPeekRef<'a> = Rc<RefCell<Peekable<Lexer<'a>>>>;

pub(crate) fn parse(lexer: Lexer) -> Result<Vec<Statement>, ParseErrors> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    let lexer_ref = Rc::new(RefCell::new(lexer.peekable()));
    let next = lexer_ref.borrow_mut().next();
    if let Some(lok_tok) = next {
        match lok_tok.token {
            Token::Let => match parse_let_statement(lexer_ref.clone()) {
                Ok(statement) => statements.push(Statement::Let(statement)),
                Err(e) => errors.push(e),
            },
            _ => {
                errors.push(
                    Report::new(ParseError::UnexpectedToken(lok_tok))
                        .attach("Expected a `Let` keyword"),
                );
            }
        };
    }
    if !errors.is_empty() {
        Err(Report::new(ParseErrors(errors)))
    } else {
        Ok(statements)
    }
}

fn parse_let_statement(lexer: LexerPeekRef) -> Result<LetStatement, ParseError> {
    let ident = parse_identifier(lexer.clone())?;
    let next_eq = lexer.borrow_mut().next();
    match next_eq {
        Some(eq_lok_tok) => match eq_lok_tok.token {
            Token::Assign => {}
            _ => return Err(Report::new(ParseError::UnexpectedToken(eq_lok_tok))),
        },
        None => return Err(Report::new(ParseError::Eof)),
    };
    let expr = parse_expression(lexer.clone())?;

    Ok(LetStatement { ident, expr })
}

fn parse_identifier(lexer: LexerPeekRef) -> Result<LocTok, ParseError> {
    match lexer.borrow_mut().next() {
        Some(lok_tok) => {
            match lok_tok.token {
                Token::Ident(_) => Ok(lok_tok),
                _ => Err(Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach("Expected and identifier")),
            }
        }
        None => Err(Report::new(ParseError::Eof)),
    }
}
fn parse_expression(lexer: LexerPeekRef) -> Result<Expr, ParseError> {
    let next = lexer.borrow_mut().next();
    match next {
        Some(lhs_lok_tok) => {
            let peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
            match peek {
                Some(peek_lok_tok) => match peek_lok_tok.token {
                    Token::Semicolon => Ok(Expr::Base(lhs_lok_tok)),
                    _ => Ok(parse_expression_op(lexer.clone(), lhs_lok_tok)?),
                },
                None => Err(Report::new(ParseError::Eof).attach("Expected a semicolon")),
            }
        }
        None => Err(Report::new(ParseError::Eof).attach("Expected an expression")),
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
    let _plux_lok_tok = lexer
        .borrow_mut()
        .next()
        .expect("The plus token was previously found in the parse_expression method");
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
            None => Err(Report::new(ParseError::Eof).attach("Expected a semicolon")),
        },
        None => Err(Report::new(ParseError::Eof).attach("Expected an expression")),
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::Lexer;

    use super::*;
    use assert_matches::assert_matches;

    // #[test]
    // fn single_expr() {
    //     let code: &'static str = r#"5 + 5"#;
    //     let lexer = Lexer::new(code.as_bytes(), code.len());
    //     let program = Program::parse(lexer);
    //     dbg!(program);
    //     assert!(false);
    // }

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
            #[allow(unused_must_use)]
            Err(e) => {
                dbg!(e);
                assert!(false);
            }
        };
    }
}
