pub mod structs;
mod tests;

use error_stack::Report;
use lexer::{PeekLex, Precedence, Token, TokenKind};
use owo_colors::OwoColorize;
use structs::*;

pub fn parse(lexer: &mut PeekLex) -> ParseResult<Vec<Statement>> {
    if cfg!(test) {
        owo_colors::set_override(false);
    }
    Report::install_debug_hook::<Suggestion>(|value, context| context.push_body(value.to_string()));
    Report::install_debug_hook::<Help>(|value, context| context.push_body(value.to_string()));
    if cfg!(any(not(debug_assertions), test)) {
        use std::panic::Location;
        Report::install_debug_hook::<Location>(|_value, _context| {});
    }
    let statements = parse_statements(lexer);
    lexer.next();
    statements
}

fn parse_statements(lexer: &mut PeekLex) -> ParseResult<Vec<Statement>> {
    let mut statements = Vec::new();
    let mut error: Option<Report<ParseError>> = None;
    let mut start_statement_peek = lexer.peek().cloned();
    let mut term_state = TermState::None;

    while let Some(lok_tok) = start_statement_peek {
        use TokenKind::*;
        match lok_tok.kind {
            // Match on the tokens that could start statements [Let, Return, Ident, expr_start]
            Let => {
                parse_let_statement(lexer).handle_statement_err(
                    lexer,
                    &mut statements,
                    &mut error,
                    &mut term_state,
                );
            }
            Return => {
                parse_return_statement(lexer).handle_statement_err(
                    lexer,
                    &mut statements,
                    &mut error,
                    &mut term_state,
                );
            }
            Ident(_) => {
                // Need to decide if ident is start of expression or an assign statement
                let mut lex_clone = lexer.clone();
                let _ident = lex_clone.next();
                if matches!(is_peek(&mut lex_clone, TokenKind::Assign), Ok(_)) {
                    parse_assign_statement(lexer).handle_statement_err(
                        lexer,
                        &mut statements,
                        &mut error,
                        &mut term_state,
                    );
                } else {
                    parse_expression(lexer, Precedence::Lowest)
                        .map(|val| Statement::Expression {
                            span: val.get_span(),
                            expr: val,
                            terminated: false,
                        })
                        .handle_statement_err(lexer, &mut statements, &mut error, &mut term_state);
                }
            }
            t if is_expr_start(&t) => parse_expression(lexer, Precedence::Lowest)
                .map(|val| Statement::Expression {
                    span: val.get_span(),
                    expr: val,
                    terminated: false,
                })
                .handle_statement_err(lexer, &mut statements, &mut error, &mut term_state),

            // Skip over comments
            Comment(_) => {
                lexer.next();
            }

            // If there is a brace it is time to yeet out while keeping any errors because this
            // function is also used to parse scopes
            RBrace => {
                break;
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
    let Some(e) = error else {
        return Ok(statements);
    };
    Err(e)
}

fn parse_assign_statement(lexer: &mut PeekLex) -> ParseResult<Statement> {
    let mut error: Option<Report<ParseError>> = None;
    let ident = match parse_identifier(lexer) {
        Ok(ident_str) => Some(ident_str),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, TokenKind::Assign) {
        error.extend_assign(e);
    };
    let expr = match parse_expression(lexer, Precedence::Lowest) {
        Ok(expr) => Some(expr),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let Some(e)  = error else {
        let ExprBase::StringLiteral { val, span } = ident.unwrap() else {
            unreachable!();
        };
        let expr = expr.unwrap();
        return Ok(Statement::Assign {
            span: span + expr.get_span(),
            ident: val,
            expr,
        });
    };
    Err(e)
}

fn parse_let_statement(lexer: &mut PeekLex) -> ParseResult<Statement> {
    let let_tok = lexer
        .next()
        .expect("The let keyword was already peeked and matched");
    let mut error: Option<Report<ParseError>> = None;
    let ident = match parse_identifier(lexer) {
        Ok(ident_str) => Some(ident_str),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, TokenKind::Assign) {
        error.extend_assign(e);
    };
    let expr = match parse_expression(lexer, Precedence::Lowest) {
        Ok(expr) => Some(expr),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let Some(e)  = error else {
        let ExprBase::StringLiteral { val, .. } = ident.unwrap() else {
            unreachable!();
        };
        let expr = expr.unwrap();
        return Ok(Statement::Assign {
            span: let_tok.span + expr.get_span(),
            ident: val,
            expr,
        });
    };
    Err(e)
}

fn parse_return_statement(lexer: &mut PeekLex) -> ParseResult<Statement> {
    let ret = lexer
        .next()
        .expect("The return keyword was already peeked and matched");
    if is_peek(lexer, TokenKind::Semicolon).is_ok() {
        // A return with no expression is valid if there is a semicolon
        return Ok(Statement::Return {
            expr: None,
            span: ret.span,
        });
    }
    let expr = parse_expression(lexer, Precedence::Lowest)?;
    Ok(Statement::Return {
        span: ret.span + expr.get_span(),
        expr: Some(expr),
    })
}

fn parse_identifier(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
    let next = lexer.peek().cloned();
    match next {
        Some(lok_tok) => match lok_tok.kind {
            TokenKind::Ident(ident_str) => {
                lexer.next();
                Ok(ExprBase::StringLiteral {
                    val: ident_str,
                    span: lok_tok.span,
                })
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
fn parse_expression(lexer: &mut PeekLex, prec: Precedence) -> ParseResult<ExprBase> {
    use TokenKind::*;

    let mut left_exp = parse_left_expression(lexer)?;

    let mut peek_op_token = match lexer.peek().cloned() {
        Some(token) => token,
        None => return Ok(left_exp),
    };
    while prec < peek_op_token.kind.precedence() {
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
            None => return Ok(left_exp),
        };
    }

    // inner expressions should never be terminated so this check to match_semicolon checks if the
    // calling function wants the expression to have the option of being terminated or not.
    Ok(left_exp)
}

fn parse_left_expression(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
    use TokenKind::*;
    let lhs_val_peek = lexer.peek().cloned();
    Ok(match lhs_val_peek {
        Some(left_lok_tok) => match left_lok_tok.kind {
            // All Literals, identifiers and prefix operators should be matched here
            Ident(ident) => {
                // An ident can either be an expression all on its own or the start of an assign
                // expression
                lexer.next();
                ExprBase::Identifier {
                    ident,
                    span: left_lok_tok.span,
                }
            }
            Int(val) => {
                lexer.next();
                ExprBase::IntLiteral {
                    val,
                    span: left_lok_tok.span,
                }
            }
            True => {
                lexer.next();
                ExprBase::BoolLiteral {
                    val: true,
                    span: left_lok_tok.span,
                }
            }
            False => {
                lexer.next();
                ExprBase::BoolLiteral {
                    val: false,
                    span: left_lok_tok.span,
                }
            }
            String(val) => {
                lexer.next();
                ExprBase::StringLiteral {
                    val,
                    span: left_lok_tok.span,
                }
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

fn parse_array_index(lexer: &mut PeekLex, left: ExprBase) -> ParseResult<ExprBase> {
    let mut error: Option<Report<ParseError>> = None;
    let lbracket = lexer
        .next()
        .expect("The lbracket should already peeked and found");
    let index = match parse_expression(lexer, Precedence::Lowest) {
        Ok(expr) => Some(expr),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
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

fn parse_array(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
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

fn parse_method_expression(lexer: &mut PeekLex, instance: ExprBase) -> ParseResult<ExprBase> {
    let operator_dot = lexer
        .next()
        .expect("The operator should already peeked and found and should always be a Dot");
    let op_precedence = operator_dot.kind.precedence();
    let method = parse_expression(lexer, op_precedence)?;
    Ok(ExprBase::MethodCall {
        span: operator_dot.span + method.get_span(),
        instance: Box::new(instance),
        method: Box::new(method),
    })
}

fn parse_call_expression(lexer: &mut PeekLex, function: ExprBase) -> ParseResult<ExprBase> {
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
fn parse_call_args(lexer: &mut PeekLex, end_token: TokenKind) -> ParseResult<Vec<ExprBase>> {
    // A enum as  a state machine to track what the previous token was. This gives better options
    // for error messages
    enum ArgState {
        Empty,
        Arg,
        Comma,
    }
    use ArgState::*;
    let mut error: Option<Report<ParseError>> = None;
    let mut arguments: Vec<ExprBase> = Vec::new();
    let mut arg_state = Empty;
    let mut again = true;

    let mut peek = lexer.peek().cloned();
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.kind {
                TokenKind::Comma => {
                    lexer.next();
                    match arg_state {
                        Empty | Comma => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("There should be an expression before the comma");
                            error.extend_assign(e);
                        }
                        Arg => {}
                    };
                    arg_state = Comma;
                }
                token if token.is(&end_token) => {
                    again = false;
                }
                _ => {
                    match arg_state {
                        Arg => {
                            let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                                .attach_printable("expressions should be separated by commas");
                            error.extend_assign(e);
                            lexer.next();
                        }
                        _ => {
                            arg_state = Arg;
                        }
                    };
                    match parse_expression(lexer, Precedence::Lowest) {
                        Ok(arg) => {
                            arguments.push(arg);
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

fn parse_func_literal(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
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
fn parse_function_parameters(lexer: &mut PeekLex) -> ParseResult<Vec<Ident>> {
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
        let Some(lok_tok) = peek else {
                return Err(Report::new(ParseError::Eof)
                    .attach_printable("Expected function parameters or a closing parentheses"));
        };
        match lok_tok.kind {
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
        }
        peek = lexer.peek().cloned();
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(identifiers)
    }
}

fn parse_grouped_expression(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
    let mut error: Option<Report<ParseError>> = None;
    let exp = match parse_expression(lexer, Precedence::Lowest) {
        Ok(exp) => Some(exp),
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

fn parse_if_expression(lexer: &mut PeekLex) -> ParseResult<IfExpr> {
    let if_tok = lexer
        .next()
        .expect("the if token should already be matched");
    let mut error: Option<Report<ParseError>> = None;
    let condition: Option<ExprBase> = match parse_expression(lexer, Precedence::Lowest) {
        Ok(condition) => Some(condition),
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

fn parse_scope(lexer: &mut PeekLex) -> ParseResult<Scope> {
    let mut error: Option<Report<ParseError>> = None;
    let lbrace = match expect_peek(lexer, TokenKind::LBrace) {
        Ok(lbrace) => Some(lbrace),
        Err(e) => {
            error.extend_assign(e);
            lexer.next();
            None
        }
    };
    let statements = match parse_statements(lexer) {
        Ok(statements) => Some(statements),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let rbrace = match expect_peek(lexer, TokenKind::RBrace) {
        Ok(rbrace) => Some(rbrace),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let Some(e) = error else {
        return Ok(
            Scope {
                statements: statements.unwrap(),
                span: lbrace.unwrap().span + rbrace.unwrap().span,
            }
        );
    };
    Err(e)
}

fn parse_prefix_expression(lexer: &mut PeekLex) -> ParseResult<ExprBase> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let right = lexer.peek().cloned();
    let expression = match right {
        Some(_) => parse_expression(lexer, operator.kind.precedence())?,
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

fn parse_binary_expression(lexer: &mut PeekLex, left: ExprBase) -> ParseResult<ExprBase> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let op_precedence = operator.kind.precedence();
    let rhs = parse_expression(lexer, op_precedence)?;
    let span = left.get_span() + rhs.get_span();
    Ok(ExprBase::Binary {
        lhs: Box::new(left),
        operator,
        rhs: Box::new(rhs),
        span,
    })
}

fn expect_peek(lexer: &mut PeekLex, expected: TokenKind) -> ParseResult<Token> {
    let peek = lexer.peek().cloned();
    match peek {
        Some(lok_tok) => {
            if lok_tok.kind.is(&expected) {
                Ok(lexer.next().unwrap())
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

pub fn is_peek(lexer: &mut PeekLex, expected: TokenKind) -> ParseResult<()> {
    let peek = lexer.peek().cloned();
    match peek {
        Some(lok_tok) => {
            if lok_tok.kind.is(&expected) {
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

pub fn is_expr_start(token: &TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        token,
        Int(_) | String(_) | If | LParen | LBrace | LBracket | Minus | Bang | True | False | Func
    )
}

#[allow(dead_code)]
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
