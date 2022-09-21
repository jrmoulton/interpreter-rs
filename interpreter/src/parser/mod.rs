pub mod structs;
mod tests;

use error_stack::Report;
use std::{cell::RefCell, rc::Rc};

use crate::lexer::{Lexer, LocTok, Precedence, Token};
use structs::*;

pub(crate) fn parse(lexer: Lexer) -> Result<Vec<Statement>, ParseErrors> {
    parse_statements(Rc::new(RefCell::new(lexer.peekable())), false)
}

fn parse_statements(
    lexer: LexerPeekRef,
    inside_scope: bool,
) -> Result<Vec<Statement>, ParseErrors> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    let mut start_statement_peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    while let Some(lok_tok) = start_statement_peek {
        match lok_tok.token {
            Token::Let => match parse_let_statement(lexer.clone()) {
                Ok(statement) => statements.push(Statement::Let(statement)),
                Err(e) => errors.extend(e.errors),
            },
            Token::Return => match parse_return_statement(lexer.clone()) {
                Ok(statement) => statements.push(Statement::Return(statement)),
                Err(e) => errors.extend(e.errors),
            },
            Token::Ident(_) => {
                let mut lexer_clone = lexer.borrow_mut().clone();
                let _ident = lexer_clone.next();
                match is_peek(Rc::new(RefCell::new(lexer_clone.clone())), Token::Assign) {
                    Ok(_) => match parse_assign_statement(lexer.clone()) {
                        Ok(statement) => statements.push(Statement::Assign(statement)),
                        Err(errs) => errors.extend(errs.errors),
                    },
                    Err(_) => match parse_expression(lexer.clone(), Precedence::Lowest, true) {
                        Ok(statement) => statements.push(Statement::Expression(statement)),
                        Err(e) => errors.extend(e.errors),
                    },
                }
            }
            Token::Int(_)
            | Token::If
            | Token::LParen
            | Token::LBrace
            | Token::Minus
            | Token::Bang
            | Token::True
            | Token::False
            | Token::Func
            | Token::String(_) => match parse_expression(lexer.clone(), Precedence::Lowest, true) {
                Ok(statement) => statements.push(Statement::Expression(statement)),
                Err(e) => errors.extend(e.errors),
            },
            Token::RBrace => {
                // If there is a brace it is time to yeet out while keeping any errors
                lexer.borrow_mut().next();
                if !errors.is_empty() {
                    return Err(ParseErrors { errors });
                } else {
                    return Ok(statements);
                }
            }
            _ => {
                errors.push(
                    Report::new(ParseError::UnexpectedToken(lok_tok))
                        .attach_printable("Expected a statement or an expression"),
                );
                lexer.borrow_mut().next();
            }
        };
        start_statement_peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    }
    if inside_scope {
        errors.push(expect_peek(lexer.clone(), Token::RBrace).unwrap_err());
    }
    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(statements)
    }
}

fn parse_assign_statement(lexer: LexerPeekRef) -> Result<AssignStatement, ParseErrors> {
    let ident = lexer
        .borrow_mut()
        .next()
        .expect("The ident was already peeked and matched");
    let _assign_tok = lexer
        .borrow_mut()
        .next()
        .expect("The assign token was already matched in the cloned lexer");
    let expr = parse_expression(lexer.clone(), Precedence::Lowest, true)?;
    let expr_base = match expr {
        Expr::Terminated(expr) => expr,
        Expr::NonTerminated(_) => Err(Report::new(ParseError::ExpectedTerminatedExpr(expr))
            .attach(Suggestion("Add a semicolon to the end of this expression")))?,
    };
    let assign_statement = AssignStatement {
        ident,
        expr: Box::new(expr_base),
    };
    Ok(assign_statement)
}

fn parse_return_statement(lexer: LexerPeekRef) -> Result<Option<Expr>, ParseErrors> {
    lexer
        .borrow_mut()
        .next()
        .expect("The return keyword was already peeked and matched");
    if expect_peek(lexer.clone(), Token::Semicolon).is_ok() {
        // A return with no expression is valid if there is a semicolon
        return Ok(None);
    }
    let expr = parse_expression(lexer.clone(), Precedence::Lowest, true)?;
    let expr_base = match expr {
        Expr::Terminated(expr) => expr,
        Expr::NonTerminated(_) => Err(Report::new(ParseError::ExpectedTerminatedExpr(expr))
            .attach(Suggestion("Add a semicolon to the end of this expression")))?,
    };
    Ok(Some(Expr::Terminated(expr_base)))
}

fn parse_let_statement(lexer: LexerPeekRef) -> Result<LetStatement, ParseErrors> {
    lexer
        .borrow_mut()
        .next()
        .expect("The let keyword was already peeked and matched");
    let mut errors = Vec::new();
    let ident = match parse_identifier(lexer.clone()) {
        Ok(ident) => Some(ident),
        Err(e) => {
            errors.push(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer.clone(), Token::Assign) {
        errors.push(e);
    };
    let expr = match parse_expression(lexer.clone(), Precedence::Lowest, true) {
        Ok(expr) => match expr {
            Expr::Terminated(_) => Some(expr),
            Expr::NonTerminated(_) => {
                errors.push(
                    Report::new(ParseError::ExpectedTerminatedExpr(expr))
                        .attach(Suggestion("Add a semicolon to the end of this expression")),
                );
                None
            }
        },
        Err(e) => {
            errors.extend(e.errors);
            None
        }
    };

    if errors.is_empty() {
        Ok(LetStatement {
            ident: ident.expect("Some because there are no errors"),
            expr: expr.expect("Some because there are no errors"),
        })
    } else {
        Err(ParseErrors { errors })
    }
}

fn parse_identifier(lexer: LexerPeekRef) -> error_stack::Result<LocTok, ParseError> {
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

/// This function uses a recursive descent alrorithm to parse expressions. First the left hand side
/// (lhs) is parsed. This may end up being the lhs in a larger expression or it could be the entire
/// expression it self and be returned by itself at the end of the function. This lhs parsing work
/// by matching tokens to functions that can properly parse the next token. This includes binary
/// expressions, prefix expression and literals. The next section will see if there is an operator
/// token and if the coming operator token has a higher precedence than the previous operator token
/// (with the first call to this function just passing Precedence::Lowest) the lhs will get sucked
/// into the expression with that higher precedence. If it is equal or lower precedence it will
/// just be yeeted out as is which may be into a recursive call or out of the original function
/// call. the This ensures proper parsing of oprator precedence. This includes the lparen token
/// which in a way acts as a binary expression for a function call. But it matches to it's own
/// function.
fn parse_expression(
    lexer: LexerPeekRef,
    precedence: Precedence,
    match_semicolon: bool,
) -> Result<Expr, ParseErrors> {
    use Token::*;
    let lhs_val_peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    let mut left_exp = match lhs_val_peek {
        Some(left_lok_tok) => match left_lok_tok.token {
            // All Literals, identifiers and prefix operators should be matched here
            Ident(_) => {
                // An ident can either be an expression all on its own or the start of an assign
                // expression
                lexer.borrow_mut().next();
                ExprBase::Identifier(structs::Ident(left_lok_tok))
            }
            Int(_) => {
                lexer.borrow_mut().next();
                ExprBase::IntLiteral(left_lok_tok)
            }
            True | False => {
                lexer.borrow_mut().next(); // The token was only peeked but we are now handling it
                                           // so skip it here
                ExprBase::BoolLiteral(left_lok_tok)
            }
            String(_) => {
                lexer.borrow_mut().next();
                ExprBase::StringLiteral(left_lok_tok)
            }
            Bang | Token::Minus => {
                // Don't skip the operator because it is needed
                parse_prefix_expression(lexer.clone())?
            }
            LParen => {
                lexer.borrow_mut().next(); // This skips the lparen
                parse_grouped_expression(lexer.clone())?
            }
            If => {
                lexer.borrow_mut().next();
                parse_if_expression(lexer.clone())?
            }
            Func => {
                lexer.borrow_mut().next();
                parse_func_literal(lexer.clone())?
            }
            Token::LBrace => {
                let _lbrace = lexer.borrow_mut().next();
                ExprBase::Scope(parse_statements(lexer.clone(), true)?)
            }
            _ => {
                lexer.borrow_mut().next();
                Err(Report::new(ParseError::UnexpectedToken(left_lok_tok))
                    .attach_printable("Expected an expression"))?
            }
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression"))?,
    };

    let mut peek_op_token = match lexer.borrow_mut().peek().map(|val| val.to_owned()) {
        Some(token) => token,
        None => return Ok(Expr::NonTerminated(left_exp)),
    };
    while precedence < peek_op_token.token.precedence() {
        left_exp = match peek_op_token.token {
            // All binary tokens should be matched here. This includes LParen but LParen will match
            // to a different function because it is the start of a function call which needs
            // a few more checks than just regular binary expressions
            Plus | Minus | Slash | Asterisk | Eq | Ne | LT | GT | Assign | BitOr | Or | BitAnd
            | And => parse_binary_expression(lexer.clone(), left_exp)?,
            LParen => parse_call_expression(lexer.clone(), left_exp)?,
            _ => Err(Report::new(ParseError::UnexpectedToken(peek_op_token))
                .attach_printable("Expected a binary operator"))?,
        };
        peek_op_token = match lexer.borrow_mut().peek().map(|val| val.to_owned()) {
            Some(token) => token,
            None => return Ok(Expr::NonTerminated(left_exp)),
        };
    }
    // inner expressions should never be terminated so this check to match_semicolon checks if the
    // calling function wants the expression to have the option of being terminated or not.
    if match_semicolon && peek_op_token.token.token_matches(&Token::Semicolon) {
        lexer.borrow_mut().next();
        Ok(Expr::Terminated(left_exp))
    } else {
        Ok(Expr::NonTerminated(left_exp))
    }
}

fn parse_call_expression(lexer: LexerPeekRef, function: ExprBase) -> Result<ExprBase, ParseErrors> {
    let mut errors = Vec::new();
    if let Err(e) = expect_peek(lexer.clone(), Token::LParen) {
        errors.push(e);
    }
    let args = match parse_call_args(lexer.clone()) {
        Ok(args) => Some(args),
        Err(errs) => {
            errors.extend(errs.errors);
            None
        }
    };
    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(ExprBase::CallExpression(CallExpr {
            function: Box::new(function),
            args: args.expect("If there are no errors then the parsed args are present"),
        }))
    }
}

// A function to parse the arguments to a function when it is being called
fn parse_call_args(lexer: LexerPeekRef) -> Result<Vec<Expr>, ParseErrors> {
    // A enum as  a state machine to track what the previous token was. This gives better options
    // for error messages
    enum ArgState {
        Empty,
        Arg,
        Comma,
    }
    let mut errors = Vec::new();
    let mut arguments: Vec<Expr> = Vec::new();
    let mut arg_state = ArgState::Empty;
    let mut again = true;

    let mut peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.token {
                Token::Comma => {
                    lexer.borrow_mut().next();
                    match arg_state {
                        ArgState::Empty | ArgState::Comma => {
                            errors.push(
                                Report::new(ParseError::UnexpectedToken(lok_tok)).attach_printable(
                                    "There should be an argument before the comma",
                                ),
                            );
                        }
                        ArgState::Arg => {}
                    };
                    arg_state = ArgState::Comma;
                }
                Token::RParen => {
                    again = false;
                    lexer.borrow_mut().next();
                }
                _ => {
                    match arg_state {
                        ArgState::Arg => {
                            errors.push(
                                Report::new(ParseError::UnexpectedToken(lok_tok))
                                    .attach_printable("Identifiers should be separated by commas"),
                            );
                            lexer.borrow_mut().next();
                        }
                        _ => {
                            arg_state = ArgState::Arg;
                        }
                    };
                    match parse_expression(lexer.clone(), Precedence::Lowest, false) {
                        Ok(arg) => {
                            arguments.push(arg);
                        }
                        Err(errs) => {
                            errors.extend(errs.errors);
                        }
                    }
                }
            },
            None => errors.push(
                Report::new(ParseError::Eof)
                    .attach_printable("Expected function parameters or a closing parentheses"),
            ),
        }
        peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    }

    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(arguments)
    }
}

fn parse_func_literal(lexer: LexerPeekRef) -> Result<ExprBase, ParseErrors> {
    let mut errors = Vec::new();
    if let Err(e) = expect_peek(lexer.clone(), Token::LParen) {
        errors.push(e);
    }
    let identifiers = match parse_function_parameters(lexer.clone()) {
        Ok(identifiers) => Some(identifiers),
        Err(e) => {
            errors.extend(e.errors);
            None
        }
    };
    if let Err(e) = expect_peek(lexer.clone(), Token::LBrace) {
        errors.push(e);
    }
    let body = match parse_statements(lexer.clone(), true) {
        Ok(statements) => Some(Scope::new(statements)),
        Err(e) => {
            errors.extend(e.errors);
            None
        }
    };
    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(ExprBase::FuncLiteral(FnLiteral {
            parameters: identifiers
                .expect("If there are no errors then the identifers are present"),
            body: body.expect("If there are no errors then the function body is present"),
        }))
    }
}

// A function to parse formal parameters of a function definition
fn parse_function_parameters(lexer: LexerPeekRef) -> std::result::Result<Vec<Ident>, ParseErrors> {
    // A enum as  a state machine to track what the previous token was. This gives better options
    // for error messages
    enum ParamState {
        Empty,
        Ident,
        Comma,
    }
    let mut errors = Vec::new();
    let mut identifiers = Vec::new();
    let mut param_state = ParamState::Empty;
    let mut again = true;

    let mut peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.token {
                Token::Ident(_) => {
                    lexer.borrow_mut().next();
                    match param_state {
                        ParamState::Ident => {
                            errors.push(
                                Report::new(ParseError::UnexpectedToken(lok_tok))
                                    .attach_printable("Identifiers should be separated by commas"),
                            );
                        }
                        _ => {
                            param_state = ParamState::Ident;
                            identifiers.push(Ident(lok_tok));
                        }
                    }
                }
                Token::Comma => {
                    lexer.borrow_mut().next();
                    match param_state {
                        ParamState::Empty | ParamState::Comma => {
                            errors.push(
                                Report::new(ParseError::UnexpectedToken(lok_tok)).attach_printable(
                                    "There should be an identifier before the comma",
                                ),
                            );
                        }
                        ParamState::Ident => {}
                    };
                    param_state = ParamState::Comma;
                }
                Token::RParen => {
                    again = false;
                    lexer.borrow_mut().next();
                }
                _ => {
                    errors.push(
                        Report::new(ParseError::UnexpectedToken(lok_tok))
                            .attach_printable("Expected an identifier or a closing parentheses"),
                    );
                    again = false;
                }
            },
            None => errors.push(
                Report::new(ParseError::Eof)
                    .attach_printable("Expected function parameters or a closing parentheses"),
            ),
        }
        peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    }

    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(identifiers)
    }
}

fn parse_grouped_expression(lexer: LexerPeekRef) -> Result<ExprBase, ParseErrors> {
    let mut errors = Vec::new();
    // let _lparen = lexer.borrow_mut().next();
    let exp = match parse_expression(lexer.clone(), Precedence::Lowest, true) {
        Ok(exp) => match exp {
            Expr::Terminated(_) => {
                errors.push(
                    Report::new(ParseError::UnexpectedTerminatedExpr(exp)).attach(Suggestion(
                        "Try removing the semicolon from this expression",
                    )),
                );
                None
            }
            Expr::NonTerminated(exp) => Some(exp),
        },
        Err(errs) => {
            errors.extend(errs.errors);
            None
        }
    };
    if let Err(e) = expect_peek(lexer.clone(), Token::RParen) {
        errors.push(e);
    }
    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(exp.expect("If there are no errors then the expression is present"))
    }
}

fn parse_if_expression(lexer: LexerPeekRef) -> Result<ExprBase, ParseErrors> {
    let mut errors = Vec::new();
    let condition: Option<ExprBase> =
        match parse_expression(lexer.clone(), Precedence::Lowest, true) {
            Ok(condition) => match condition {
                Expr::Terminated(_) => {
                    errors.push(
                        Report::new(ParseError::UnexpectedTerminatedExpr(condition)).attach(
                            Suggestion("Try removing the semicolon from this expression"),
                        ),
                    );
                    None
                }
                Expr::NonTerminated(cond_base) => Some(cond_base),
            },
            Err(e) => {
                errors.extend(e.errors);
                None
            }
        };
    if let Err(e) = expect_peek(lexer.clone(), Token::LBrace) {
        errors.push(e);
    };
    let consequence = match parse_statements(lexer.clone(), true) {
        Ok(statements) => Some(statements),
        Err(e) => {
            errors.extend(e.errors);
            None
        }
    };
    let alternate_opt = expect_peek(lexer.clone(), Token::Else);
    let alternative = match alternate_opt {
        Err(_) => None,
        Ok(_) => {
            let if_or_lbrace = expect_peek(lexer.clone(), Token::If);
            match if_or_lbrace {
                Err(_) => {
                    if let Err(e) = expect_peek(lexer.clone(), Token::LBrace) {
                        errors.push(e);
                    }
                    match parse_statements(lexer.clone(), true) {
                        Ok(statements) => Some(ElseIfExpr::Else(Scope::new(statements))),
                        Err(e) => {
                            errors.extend(e.errors);
                            None
                        }
                    }
                }
                Ok(_) => match parse_if_expression(lexer.clone()) {
                    Ok(if_expr) => Some(ElseIfExpr::ElseIf(Box::new(if_expr))),
                    Err(e) => {
                        errors.extend(e.errors);
                        None
                    }
                },
            }
        }
    };
    if !errors.is_empty() {
        Err(ParseErrors { errors })
    } else {
        Ok(ExprBase::If(IfExpr {
            condition: Box::new(
                condition.expect("If there are no errors then the expression is present"),
            ),
            consequence: Scope::new(
                consequence.expect("If there are no errors then the expression is present"),
            ),
            alternative,
        }))
    }
}

fn parse_prefix_expression(lexer: LexerPeekRef) -> Result<ExprBase, ParseErrors> {
    let operator = lexer
        .borrow_mut()
        .next()
        .expect("The operator was already peeked and found");
    let right = lexer.borrow_mut().peek().map(|val| val.to_owned());
    let expression = match right {
        Some(_) => parse_expression(lexer, operator.token.precedence(), false)?,
        None => Err(Report::new(ParseError::Eof).attach_printable(format!(
            "Expected an operand after the prefix operator {:?} at {operator:?}",
            operator.token
        )))?,
    };
    Ok(ExprBase::PrefixExpression(PreExpr {
        operator,
        expression: Box::new(expression),
    }))
}

fn parse_binary_expression(lexer: LexerPeekRef, left: ExprBase) -> Result<ExprBase, ParseErrors> {
    let operator = lexer
        .borrow_mut()
        .next()
        .expect("The operator was already peeked and found");
    let op_precedence = operator.token.precedence();
    let rhs_expr = parse_expression(lexer.clone(), op_precedence, false)?;
    let rhs = match rhs_expr {
        Expr::Terminated(_) => {
            return Err(Report::new(ParseError::UnexpectedTerminatedExpr(rhs_expr))
                .attach(Suggestion(
                    "Try removing the semicolon from this expression",
                ))
                .into());
        }
        Expr::NonTerminated(rhs) => rhs,
    };
    Ok(ExprBase::BinaryExpression(BinExp {
        lhs: Box::new(left),
        operator,
        rhs: Box::new(rhs),
    }))
}

// A function that
fn expect_peek(lexer: LexerPeekRef, expected: Token) -> error_stack::Result<(), ParseError> {
    let peek = lexer.borrow_mut().peek().map(|val| val.to_owned());
    match peek {
        Some(lok_tok) => {
            if lok_tok.token.token_matches(&expected) {
                lexer.borrow_mut().next();
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

fn is_peek(lexer: LexerPeekRef, expected: Token) -> error_stack::Result<(), ParseError> {
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
