pub mod structs;
mod tests;

use error_stack::{Report, Result};
use lexer::{PeekLex, Precedence, Token, TokenKind};
use owo_colors::OwoColorize;
use structs::*;

pub fn parse(lexer: &mut PeekLex) -> Result<Vec<Statement>, ParseError> {
    if cfg!(test) {
        owo_colors::set_override(false);
    }
    Report::install_debug_hook::<Suggestion>(|value, context| context.push_body(value.to_string()));
    if cfg!(any(not(debug_assertions), test)) {
        use std::panic::Location;
        Report::install_debug_hook::<Location>(|_value, _context| {});
    }
    let statements = parse_statements(lexer, false);
    lexer.next();
    statements
}

fn parse_statements(lexer: &mut PeekLex, inside_scope: bool) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::new();
    let mut error: Option<Report<ParseError>> = None;
    let mut start_statement_peek = lexer.peek().cloned();
    let mut term_state = TermState::None;
    while let Some(lok_tok) = start_statement_peek {
        use TokenKind::*;
        match lok_tok.kind {
            // Handle all tokens that could be the start of an expression
            Int(_) | If | LParen | LBrace | LBracket | Minus | Bang | True | False | Func
            | String(_) => match parse_expression(lexer, Precedence::Lowest, true) {
                Ok(statement) => match term_state {
                    // Matching on the temination state improves error messages and hnadling
                    TermState::None | TermState::Term => {
                        term_state = match statement {
                            Expr::Terminated(_) => TermState::Term,
                            Expr::NonTerminated(_) => TermState::NonTerm,
                        };
                        statements.push(Statement::Expression(statement));
                    }
                    TermState::NonTerm => match statement {
                        Expr::Terminated(_) => statements.push(Statement::Expression(statement)),
                        Expr::NonTerminated(_) => {
                            let e = Report::new(ParseError::MultipleUnterminatedExpressions)
                                .attach(Suggestion("Add a semicolon before the expression"))
                                .attach_printable(pretty_output(
                                    statement.get_span(),
                                    lexer.get_input(),
                                ));
                            error.extend_assign(e);
                        }
                    },
                },
                Err(e) => {
                    error.extend_assign(e);
                }
            },

            // Match on the tokens that could start statements [Let, Return, Ident]
            Let => {
                match parse_let_statement(lexer) {
                    Ok(statement) => statements.push(statement),
                    Err(e) => {
                        error.extend_assign(e);
                    }
                }
                term_state = TermState::Term;
            }
            Return => {
                term_state = TermState::Term;
                match parse_return_statement(lexer) {
                    Ok(statement) => statements.push(Statement::Return(statement)),
                    Err(e) => {
                        error.extend_assign(e);
                    }
                }
            }
            Ident(_) => match parse_ident_statement(lexer, &mut term_state) {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(e) => {
                    error.extend_assign(e);
                }
            },

            // Skip over comments
            Comment(_) => {
                lexer.next();
            }

            // If there is a brace it is time to yeet out while keeping any errors because this
            // function is also used to parse scopes
            RBrace => {
                if let Some(e) = error {
                    return Err(e);
                } else {
                    return Ok(statements);
                }
            }

            // Handle any tokens that don't start an expression or a statement
            _ => {
                lexer.next();
                let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach_printable("Expected a statement or an expression");
                error.extend_assign(e);
            }
        };
        start_statement_peek = lexer.peek().cloned();
    }
    if inside_scope {
        let e = expect_peek(lexer, TokenKind::RBrace).unwrap_err();
        error.extend_assign(e);
    }
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(statements)
    }
}

fn parse_ident_statement(
    lexer: &mut PeekLex,
    term_state: &mut TermState,
) -> Result<Statement, ParseError> {
    let mut lexer_clone = lexer.clone();
    let _ident = lexer_clone.next();
    match is_peek(&mut lexer_clone, TokenKind::Assign) {
        Ok(_) => match parse_assign_statement(lexer) {
            Ok(statement) => Ok(statement),
            Err(e) => Err(e),
        },
        Err(_) => match parse_expression(lexer, Precedence::Lowest, true) {
            Ok(statement) => match term_state {
                TermState::None | TermState::Term => {
                    *term_state = TermState::NonTerm;
                    Ok(Statement::Expression(statement))
                }
                TermState::NonTerm => match statement {
                    Expr::Terminated(_) => Ok(Statement::Expression(statement)),
                    Expr::NonTerminated(_) => {
                        let e = Report::new(ParseError::MultipleUnterminatedExpressions)
                            .attach(Suggestion("Add a semicolon before this expression"))
                            .attach_printable(pretty_output(
                                statement.get_span(),
                                lexer.get_input(),
                            ));
                        Err(e)
                    }
                },
            },
            Err(e) => Err(e),
        },
    }
}

fn parse_assign_statement(lexer: &mut PeekLex) -> Result<Statement, ParseError> {
    let ident = lexer
        .next()
        .expect("The ident was already peeked and matched");
    let _assign_tok = lexer.next();
    let expr = parse_expression(lexer, Precedence::Lowest, true)?;
    let expr_base = match expr {
        Expr::Terminated(expr) => expr,
        Expr::NonTerminated(_) => Err(Report::new(ParseError::ExpectedTerminatedExpr(expr))
            .attach(Suggestion("Add a semicolon to the end of this expression")))?,
    };
    let assign_statement = Statement::Assign {
        span: ident.span + expr_base.get_span(),
        ident,
        expr: expr_base,
    };
    Ok(assign_statement)
}

fn parse_return_statement(lexer: &mut PeekLex) -> Result<Option<Expr>, ParseError> {
    lexer
        .next()
        .expect("The return keyword was already peeked and matched");
    if expect_peek(lexer, TokenKind::Semicolon).is_ok() {
        // A return with no expression is valid if there is a semicolon
        return Ok(None);
    }
    let expr = parse_expression(lexer, Precedence::Lowest, true)?;
    let expr_base = match expr {
        Expr::Terminated(expr) => expr,
        Expr::NonTerminated(_) => {
            let span = expr.get_span();
            Err(Report::new(ParseError::ExpectedTerminatedExpr(expr))
                .attach(Suggestion("Add a semicolon to the end of this expression"))
                .attach_printable(pretty_output(span, lexer.get_input())))?
        }
    };
    Ok(Some(Expr::Terminated(expr_base)))
}

fn pretty_output(span: lexer::Span, source: &[String]) -> String {
    let mut ret = Vec::new();
    for row in span.get_row_range(0, 2) {
        if row > 0 {
            if let Some(line) = source.get(row - 1) {
                let mut first = if cfg!(not(test)) {
                    format!("{:<5}| ", row).bright_blue().bold().to_string()
                } else {
                    format!("{:<5}| ", row)
                };
                let mut line = line.to_owned();
                if span.get_start_row() != 0 && row == span.get_start_row() - 1 {
                    line.insert(span.get_end_col() - 3, ';');
                };
                first.push_str(&line);
                ret.push(first);
            }
        }
    }
    ret.join("\n")
}

fn parse_let_statement(lexer: &mut PeekLex) -> Result<Statement, ParseError> {
    let let_tok = lexer
        .next()
        .expect("The let keyword was already peeked and matched");
    let mut error: Option<Report<ParseError>> = None;
    let ident = match parse_identifier(lexer) {
        Ok(ident) => Some(ident),
        Err(e) => {
            if is_peek(lexer, TokenKind::Assign).is_err() {
                lexer.next();
            };
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, TokenKind::Assign) {
        error.extend_assign(e);
    };
    let expr_base = match parse_expression(lexer, Precedence::Lowest, true) {
        Ok(expr) => match expr {
            Expr::Terminated(expr_base) => Some(expr_base),
            Expr::NonTerminated(ref expr_base) => {
                let span = expr_base.get_span();
                let e = Report::new(ParseError::ExpectedTerminatedExpr(expr))
                    .attach(Suggestion("Add a semicolon to the end of this expression"))
                    .attach_printable(pretty_output(span, lexer.get_input()));
                if let Some(error) = error.as_mut() {
                    error.extend_one(e);
                } else {
                    error = Some(e);
                };
                None
            }
        },
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Some(e) = error {
        Err(e)
    } else {
        let expr = expr_base.expect("Some because there are no errors");
        Ok(Statement::Let {
            span: let_tok.span + expr.get_span(),
            ident: ident.expect("Some because there are no errors"),
            expr,
        })
    }
}

fn parse_identifier(lexer: &mut PeekLex) -> error_stack::Result<Token, ParseError> {
    let next = lexer.peek().cloned();
    match next {
        Some(lok_tok) => match lok_tok.kind {
            TokenKind::Ident(_) => {
                lexer.next();
                Ok(lok_tok)
            }
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
    lexer: &mut PeekLex,
    precedence: Precedence,
    match_semicolon: bool,
) -> Result<Expr, ParseError> {
    use TokenKind::*;

    let mut left_exp = parse_left_expression(lexer)?;

    let mut peek_op_token = match lexer.peek().cloned() {
        Some(token) => token,
        None => return Ok(Expr::NonTerminated(left_exp)),
    };
    while precedence < peek_op_token.kind.precedence() {
        left_exp = match peek_op_token.kind {
            // All binary tokens should be matched here. This includes LParen but LParen will match
            // to a different function because it is the start of a function call which needs
            // a few more checks than just regular binary expressions
            Plus | Minus | Slash | Asterisk | Eq | Ne | LT | GT | Assign | BitOr | Or | BitAnd
            | And => parse_binary_expression(lexer, left_exp)?,
            LParen => parse_call_expression(lexer, left_exp)?,
            LBracket => parse_array_index(lexer, left_exp)?,
            Dot => parse_method_expression(lexer, left_exp)?,
            _ => Err(Report::new(ParseError::UnexpectedToken(peek_op_token))
                .attach_printable("Expected a binary operator"))?,
        };
        peek_op_token = match lexer.peek().cloned() {
            Some(token) => token,
            None => return Ok(Expr::NonTerminated(left_exp)),
        };
    }

    // inner expressions should never be terminated so this check to match_semicolon checks if the
    // calling function wants the expression to have the option of being terminated or not.
    if match_semicolon && peek_op_token.kind.token_matches(&TokenKind::Semicolon) {
        lexer.next();
        Ok(Expr::Terminated(left_exp))
    } else {
        Ok(Expr::NonTerminated(left_exp))
    }
}

fn parse_left_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    use TokenKind::*;
    let lhs_val_peek = lexer.peek().cloned();
    Ok(match lhs_val_peek {
        Some(left_lok_tok) => match left_lok_tok.kind {
            // All Literals, identifiers and prefix operators should be matched here
            Ident(_) => {
                // An ident can either be an expression all on its own or the start of an assign
                // expression
                lexer.next();
                ExprBase::Identifier(left_lok_tok)
            }
            Int(_) => {
                lexer.next();
                ExprBase::IntLiteral(left_lok_tok)
            }
            True | False => {
                lexer.next(); // The token was only peeked but we are now handling it
                              // so skip it here
                ExprBase::BoolLiteral(left_lok_tok)
            }
            String(_) => {
                lexer.next();
                ExprBase::StringLiteral(left_lok_tok)
            }
            Bang | TokenKind::Minus | Plus => {
                // Don't skip the operator because it is needed
                parse_prefix_expression(lexer)?
            }
            LParen => {
                lexer.next(); // This skips the lparen
                parse_grouped_expression(lexer)?
            }
            If => parse_if_expression(lexer)?.into(),
            Func => parse_func_literal(lexer)?,
            TokenKind::LBrace => parse_scope(lexer)?.into(),
            TokenKind::LBracket => parse_array(lexer)?,
            _ => {
                lexer.next();
                Err(Report::new(ParseError::UnexpectedToken(left_lok_tok))
                    .attach_printable("Expected an expression"))?
            }
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression"))?,
    })
}

fn parse_array_index(lexer: &mut PeekLex, left: ExprBase) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    let lbracket = lexer
        .next()
        .expect("The lbracket should already peeked and found");
    let index = match parse_expression(lexer, Precedence::Lowest, false) {
        Ok(expr) => Some(expr),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    }
    .map(|val| val.expect_non_terminated());
    let rbracket = match is_peek(lexer, TokenKind::RBracket) {
        Ok(_) => lexer.next().expect("already matched"),
        Err(e) => {
            error.extend_assign(e);
            Err(error.expect("should definitely be an error"))?
        }
    };
    Ok(ExprBase::Index {
        array: Box::new(left),
        index: Box::new(index.unwrap()),
        span: lbracket.span + rbracket.span,
    })
}

fn parse_array(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let lbracket = lexer.next().expect("should be lbracket");
    let mut error: Option<Report<ParseError>> = None;
    match parse_call_args(lexer, TokenKind::RBracket) {
        Ok(exprs) => {
            let rbracket = lexer
                .next()
                .expect("should already be matched because this is the ok branch");
            Ok(ExprBase::Array {
                exprs,
                span: lbracket.span + rbracket.span,
            })
        }
        Err(e) => {
            error.extend_assign(e);
            Err(error.expect("should be some because an error was added"))
        }
    }
}

fn parse_method_expression(
    lexer: &mut PeekLex,
    instance: ExprBase,
) -> Result<ExprBase, ParseError> {
    let operator_dot = lexer
        .next()
        .expect("The operator should already peeked and found and should always be a Dot");
    let op_precedence = operator_dot.kind.precedence();
    let method = parse_expression(lexer, op_precedence, false)?.expect_non_terminated();
    Ok(ExprBase::MethodCall {
        span: operator_dot.span + method.get_span(),
        instance: Box::new(instance),
        method: Box::new(method),
    })
}

fn parse_call_expression(lexer: &mut PeekLex, function: ExprBase) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    let _lparen = lexer.next().expect("already matched");
    let args = match parse_call_args(lexer, TokenKind::RParen) {
        Ok(args) => args,
        Err(e) => {
            error.extend_assign(e);
            Err(error.expect("definitely an error"))?
        }
    };
    let rparen = lexer
        .next()
        .expect("already matched and checked in parse_call_args");
    Ok(ExprBase::Call {
        span: function.get_span() + rparen.span,
        function: Box::new(function),
        args,
    })
}

// A function to parse the arguments to a function when it is being called
fn parse_call_args(lexer: &mut PeekLex, end_token: TokenKind) -> Result<Vec<ExprBase>, ParseError> {
    // A enum as  a state machine to track what the previous token was. This gives better options
    // for error messages
    enum ArgState {
        Empty,
        Arg,
        Comma,
    }
    let mut error: Option<Report<ParseError>> = None;
    let mut arguments: Vec<ExprBase> = Vec::new();
    let mut arg_state = ArgState::Empty;
    let mut again = true;

    let mut peek = lexer.peek().cloned();
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.kind {
                TokenKind::Comma => {
                    lexer.next();
                    match arg_state {
                        ArgState::Empty | ArgState::Comma => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("There should be an expression before the comma");
                            error.extend_assign(e);
                        }
                        ArgState::Arg => {}
                    };
                    arg_state = ArgState::Comma;
                }
                token if token.token_matches(&end_token) => {
                    again = false;
                }
                _ => {
                    match arg_state {
                        ArgState::Arg => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("expressions should be separated by commas");
                            error.extend_assign(e);
                            lexer.next();
                        }
                        _ => {
                            arg_state = ArgState::Arg;
                        }
                    };
                    match parse_expression(lexer, Precedence::Lowest, false) {
                        Ok(arg) => {
                            arguments.push(arg.expect_non_terminated());
                        }
                        Err(e) => {
                            error.extend_assign(e);
                        }
                    }
                }
            },
            None => {
                let e = Report::new(ParseError::Eof)
                    .attach_printable("Expected function parameters or a closing parentheses");
                error.extend_assign(e);
            }
        }
        peek = lexer.peek().cloned();
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(arguments)
    }
}

fn parse_func_literal(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let fn_tok = lexer
        .next()
        .expect("fn token should already be already matched");
    let mut error: Option<Report<ParseError>> = None;
    if let Err(e) = expect_peek(lexer, TokenKind::LParen) {
        if let Some(error) = error.as_mut() {
            error.extend_one(e);
        } else {
            error = Some(e);
        };
    }
    let identifiers = match parse_function_parameters(lexer) {
        Ok(identifiers) => {
            let _rparent = lexer.next();
            Some(identifiers)
        }
        Err(e) => {
            let _rparent = lexer.next();
            error.extend_assign(e);
            None
        }
    };
    let body = match parse_scope(lexer) {
        Ok(scope) => Some(scope),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Some(e) = error {
        Err(e)
    } else {
        let body = body.expect("If there are no errors then the function body is present");
        Ok(ExprBase::Func {
            parameters: identifiers
                .expect("If there are no errors then the identifers are present"),
            span: fn_tok.span + body.span,
            body,
        })
    }
}

// A function to parse formal parameters of a function definition
fn parse_function_parameters(lexer: &mut PeekLex) -> Result<Vec<Ident>, ParseError> {
    // A enum as  a state machine to track what the previous token was. This gives better options
    // for error messages
    enum ParamState {
        Empty,
        Ident,
        Comma,
    }
    let mut error: Option<Report<ParseError>> = None;
    let mut identifiers = Vec::new();
    let mut param_state = ParamState::Empty;
    let mut again = true;

    let mut peek = lexer.peek().cloned();
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.kind {
                TokenKind::Ident(_) => {
                    lexer.next();
                    match param_state {
                        ParamState::Ident => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("Identifiers should be separated by commas");
                            error.extend_assign(e);
                        }
                        _ => {
                            param_state = ParamState::Ident;
                            identifiers.push(Ident(lok_tok));
                        }
                    }
                }
                TokenKind::Comma => {
                    lexer.next();
                    match param_state {
                        ParamState::Empty | ParamState::Comma => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("There should be an identifier before the comma");
                            if let Some(error) = error.as_mut() {
                                error.extend_one(e);
                            } else {
                                error = Some(e);
                            };
                        }
                        ParamState::Ident => {}
                    };
                    param_state = ParamState::Comma;
                }
                TokenKind::RParen => {
                    again = false;
                }
                _ => {
                    let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                        .attach_printable("Expected an identifier or a closing parentheses");
                    error.extend_assign(e);
                    again = false;
                }
            },
            None => {
                let e = Report::new(ParseError::Eof)
                    .attach_printable("Expected function parameters or a closing parentheses");
                error.extend_assign(e);
            }
        }
        peek = lexer.peek().cloned();
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(identifiers)
    }
}

fn parse_grouped_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    let exp = match parse_expression(lexer, Precedence::Lowest, true) {
        Ok(exp) => match exp {
            Expr::Terminated(_) => {
                let e = Report::new(ParseError::UnexpectedTerminatedExpr(exp)).attach(Suggestion(
                    "Try removing the semicolon from this expression",
                ));
                error.extend_assign(e);
                None
            }
            Expr::NonTerminated(exp) => Some(exp),
        },
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, TokenKind::RParen) {
        error.extend_assign(e);
    }
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(exp.expect("If there are no errors then the expression is present"))
    }
}

fn parse_if_expression(lexer: &mut PeekLex) -> Result<IfExpr, ParseError> {
    let if_tok = lexer
        .next()
        .expect("the if token should already be matched");
    let mut error: Option<Report<ParseError>> = None;
    let condition: Option<ExprBase> = match parse_expression(lexer, Precedence::Lowest, true) {
        Ok(condition) => match condition {
            Expr::Terminated(_) => {
                let e = Report::new(ParseError::UnexpectedTerminatedExpr(condition)).attach(
                    Suggestion("Try removing the semicolon from this expression"),
                );
                error.extend_assign(e);
                None
            }
            Expr::NonTerminated(cond_base) => Some(cond_base),
        },
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let consequence = match parse_scope(lexer) {
        Ok(scope) => Some(scope),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let alternate_opt = expect_peek(lexer, TokenKind::Else);
    let alternative: Option<_> = match alternate_opt {
        Err(_) => None,
        Ok(_) => {
            let if_or_lbrace = is_peek(lexer, TokenKind::If);
            match if_or_lbrace {
                // if ok then is if/ if else
                // if err then might be lbrace
                Err(_) => match parse_scope(lexer) {
                    Ok(scope) => Some(ElseIfExpr::Else(scope)),
                    Err(e) => {
                        error.extend_assign(e);
                        None
                    }
                },
                Ok(_) => match parse_if_expression(lexer) {
                    Ok(if_expr) => Some(ElseIfExpr::ElseIf(Box::new(if_expr))),
                    Err(e) => {
                        error.extend_assign(e);
                        None
                    }
                },
            }
        }
    };
    if let Some(e) = error {
        Err(e)
    } else {
        let consequence =
            consequence.expect("If there are no errors then the expression is present");
        Ok(IfExpr {
            condition: Box::new(
                condition.expect("If there are no errors then the expression is present"),
            ),
            span: if_tok.span
                + match alternative {
                    Some(_) => alternative.as_ref().expect("is some").get_span(),
                    None => consequence.span,
                },
            consequence,
            alternative,
        })
    }
}

fn parse_scope(lexer: &mut PeekLex) -> Result<Scope, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    let lbrace = match is_peek(lexer, TokenKind::LBrace) {
        Ok(_) => Some(lexer.next().expect("already matched")),
        Err(e) => {
            error.extend_assign(e);
            lexer.next();
            None
        }
    };
    match parse_statements(lexer, true) {
        Ok(statements) => match lbrace {
            Some(_) => {
                let rbrace = lexer
                    .next()
                    .expect("rbrace token should be matched in parse_statements");
                Ok(Scope {
                    statements,
                    span: lbrace.expect("already matched").span + rbrace.span,
                })
            }
            None => Err(error.expect("if none then error should be present"))?,
        },
        Err(e) => {
            let _rbrace = lexer.next();
            error.extend_assign(e);
            Err(error.expect("should definitely be some"))?
        }
    }
}

fn parse_prefix_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let right = lexer.peek().cloned();
    let expression = match right {
        Some(_) => {
            let rhs = parse_expression(lexer, operator.kind.precedence(), false)?;
            match rhs {
                Expr::Terminated(_) => {
                    return Err(
                        Report::new(ParseError::UnexpectedTerminatedExpr(rhs)).attach(Suggestion(
                            "Try removing the semicolon from this expression",
                        )),
                    );
                }
                Expr::NonTerminated(rhs) => rhs,
            }
        }
        None => Err(Report::new(ParseError::Eof).attach_printable(format!(
            "Expected an operand after the prefix operator {:?} at {operator:?}",
            operator.kind
        )))?,
    };
    let span = operator.span + expression.get_span();
    Ok(ExprBase::Prefix {
        operator,
        expression: Box::new(expression),
        span,
    })
}

fn parse_binary_expression(lexer: &mut PeekLex, left: ExprBase) -> Result<ExprBase, ParseError> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let op_precedence = operator.kind.precedence();
    let rhs_expr = parse_expression(lexer, op_precedence, false)?;
    let rhs = match rhs_expr {
        Expr::Terminated(_) => {
            return Err(
                Report::new(ParseError::UnexpectedTerminatedExpr(rhs_expr)).attach(Suggestion(
                    "Try removing the semicolon from this expression",
                )),
            );
        }
        Expr::NonTerminated(rhs) => rhs,
    };
    let span = left.get_span() + rhs.get_span();
    Ok(ExprBase::Binary {
        lhs: Box::new(left),
        operator,
        rhs: Box::new(rhs),
        span,
    })
}

// A function that
fn expect_peek(lexer: &mut PeekLex, expected: TokenKind) -> error_stack::Result<(), ParseError> {
    let peek = lexer.peek().cloned();
    match peek {
        Some(lok_tok) => {
            if lok_tok.kind.token_matches(&expected) {
                lexer.next();
                Ok(())
            } else {
                Err(Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach_printable(format!("Expected a {expected}")))
            }
        }
        None => {
            Err(Report::new(ParseError::Eof).attach_printable(format!("Expected a {expected}")))
        }
    }
}

fn is_peek(lexer: &mut PeekLex, expected: TokenKind) -> error_stack::Result<(), ParseError> {
    let peek = lexer.peek().cloned();
    match peek {
        Some(lok_tok) => {
            if lok_tok.kind.token_matches(&expected) {
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
