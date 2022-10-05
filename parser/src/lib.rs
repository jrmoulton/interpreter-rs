pub mod structs;
mod tests;

trait ExtendAssign {
    fn extend_assign(&mut self, e: Report<ParseError>);
}
impl ExtendAssign for Option<Report<ParseError>> {
    fn extend_assign(&mut self, e: Report<ParseError>) {
        if let Some(error) = self.as_mut() {
            error.extend_one(e);
        } else {
            *self = Some(e);
        }
    }
}

enum TermState {
    None,
    Term,
    NonTerm,
}

use error_stack::{Report, Result};
use lexer::{Lexer, LocTok, Precedence, Token};
use structs::*;

pub fn parse(lexer: Lexer) -> Result<Vec<Statement>, ParseError> {
    Report::install_debug_hook::<LocTok>(|value, context| {
        context.push_body(format!("Token: {value}"));
    });
    Report::install_debug_hook::<Suggestion>(|value, context| {
        context.push_body(format!("suggestion: {}", value.0));
    });
    parse_statements(&mut lexer.peekable(), false)
}

fn parse_statements(lexer: &mut PeekLex, inside_scope: bool) -> Result<Vec<Statement>, ParseError> {
    let mut statements = Vec::new();
    let mut error: Option<Report<ParseError>> = None;
    let mut start_statement_peek = lexer.peek().map(|val| val.to_owned());
    let mut term_state = TermState::None;
    while let Some(lok_tok) = start_statement_peek {
        match lok_tok.token {
            Token::Let => {
                match parse_let_statement(lexer) {
                    Ok(statement) => statements.push(Statement::Let(statement)),
                    Err(e) => {
                        error.extend_assign(e);
                    }
                }
                term_state = TermState::Term;
            }
            Token::Return => {
                term_state = TermState::Term;
                match parse_return_statement(lexer) {
                    Ok(statement) => statements.push(Statement::Return(statement)),
                    Err(e) => {
                        error.extend_assign(e);
                    }
                }
            }
            Token::Ident(_) => match parse_ident_statement(lexer, &mut term_state) {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(e) => {
                    error.extend_assign(e);
                }
            },
            Token::Int(_)
            | Token::If
            | Token::LParen
            | Token::LBrace
            | Token::LBracket
            | Token::Minus
            | Token::Bang
            | Token::True
            | Token::False
            | Token::Func
            | Token::String(_) => match parse_expression(lexer, Precedence::Lowest, true) {
                Ok(statement) => match term_state {
                    TermState::None | TermState::Term => {
                        term_state = TermState::NonTerm;
                        statements.push(Statement::Expression(statement));
                    }
                    TermState::NonTerm => match statement {
                        Expr::Terminated(_) => statements.push(Statement::Expression(statement)),
                        Expr::NonTerminated(_) => {
                            let e =
                                Report::new(ParseError::MultipleUnterminatedExpressions(statement));
                            error.extend_assign(e);
                        }
                    },
                },
                Err(e) => {
                    error.extend_assign(e);
                }
            },
            Token::RBrace => {
                // If there is a brace it is time to yeet out while keeping any errors
                lexer.next();
                if let Some(e) = error {
                    return Err(e);
                } else {
                    return Ok(statements);
                }
            }
            _ => {
                lexer.next();
                let e = Report::new(ParseError::UnexpectedToken(lok_tok))
                    .attach_printable("Expected a statement or an expression");
                error.extend_assign(e);
            }
        };
        start_statement_peek = lexer.peek().map(|val| val.to_owned());
    }
    if inside_scope {
        let e = expect_peek(lexer, Token::RBrace).unwrap_err();
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
    match is_peek(&mut lexer_clone, Token::Assign) {
        Ok(_) => match parse_assign_statement(lexer) {
            Ok(statement) => Ok(Statement::Assign(statement)),
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
                        let e = Report::new(ParseError::MultipleUnterminatedExpressions(statement));
                        Err(e)
                    }
                },
            },
            Err(e) => Err(e),
        },
    }
}

fn parse_assign_statement(lexer: &mut PeekLex) -> Result<AssignStatement, ParseError> {
    let ident = lexer
        .next()
        .expect("The ident was already peeked and matched");
    let _assign_tok = lexer
        .next()
        .expect("The assign token was already matched in the cloned lexer");
    let expr = parse_expression(lexer, Precedence::Lowest, true)?;
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

fn parse_return_statement(lexer: &mut PeekLex) -> Result<Option<Expr>, ParseError> {
    lexer
        .next()
        .expect("The return keyword was already peeked and matched");
    if expect_peek(lexer, Token::Semicolon).is_ok() {
        // A return with no expression is valid if there is a semicolon
        return Ok(None);
    }
    let expr = parse_expression(lexer, Precedence::Lowest, true)?;
    let expr_base = match expr {
        Expr::Terminated(expr) => expr,
        Expr::NonTerminated(_) => Err(Report::new(ParseError::ExpectedTerminatedExpr(expr))
            .attach(Suggestion("Add a semicolon to the end of this expression")))?,
    };
    Ok(Some(Expr::Terminated(expr_base)))
}

fn parse_let_statement(lexer: &mut PeekLex) -> Result<LetStatement, ParseError> {
    lexer
        .next()
        .expect("The let keyword was already peeked and matched");
    let mut error: Option<Report<ParseError>> = None;
    let ident = match parse_identifier(lexer) {
        Ok(ident) => Some(ident),
        Err(e) => {
            if is_peek(lexer, Token::Assign).is_err() {
                lexer.next();
            };
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, Token::Assign) {
        error.extend_assign(e);
    };
    let expr = match parse_expression(lexer, Precedence::Lowest, true) {
        Ok(expr) => match expr {
            Expr::Terminated(_) => Some(expr),
            Expr::NonTerminated(_) => {
                let e = Report::new(ParseError::ExpectedTerminatedExpr(expr))
                    .attach(Suggestion("Add a semicolon to the end of this expression"));
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
        Ok(LetStatement {
            ident: ident.expect("Some because there are no errors"),
            expr: expr.expect("Some because there are no errors"),
        })
    }
}

fn parse_identifier(lexer: &mut PeekLex) -> error_stack::Result<LocTok, ParseError> {
    let next = lexer.peek().cloned();
    match next {
        Some(lok_tok) => match lok_tok.token {
            Token::Ident(_) => {
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
    use Token::*;
    let lhs_val_peek = lexer.peek().map(|val| val.to_owned());
    let mut left_exp = match lhs_val_peek {
        Some(left_lok_tok) => match left_lok_tok.token {
            // All Literals, identifiers and prefix operators should be matched here
            Ident(_) => {
                // An ident can either be an expression all on its own or the start of an assign
                // expression
                lexer.next();
                ExprBase::Identifier(structs::Ident(left_lok_tok))
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
            Bang | Token::Minus => {
                // Don't skip the operator because it is needed
                parse_prefix_expression(lexer)?
            }
            LParen => {
                lexer.next(); // This skips the lparen
                parse_grouped_expression(lexer)?
            }
            If => {
                lexer.next();
                parse_if_expression(lexer)?
            }
            Func => {
                lexer.next();
                parse_func_literal(lexer)?
            }
            Token::LBrace => {
                let _lbrace = lexer.next();
                ExprBase::Scope(parse_statements(lexer, true)?)
            }
            Token::LBracket => {
                let _lbracket = lexer.next();
                ExprBase::Array(parse_array(lexer)?)
            }
            _ => {
                lexer.next();
                Err(Report::new(ParseError::UnexpectedToken(left_lok_tok))
                    .attach_printable("Expected an expression"))?
            }
        },
        None => Err(Report::new(ParseError::Eof).attach_printable("Expected an expression"))?,
    };

    let mut peek_op_token = match lexer.peek().map(|val| val.to_owned()) {
        Some(token) => token,
        None => return Ok(Expr::NonTerminated(left_exp)),
    };
    while precedence < peek_op_token.token.precedence() {
        left_exp = match peek_op_token.token {
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
        peek_op_token = match lexer.peek().map(|val| val.to_owned()) {
            Some(token) => token,
            None => return Ok(Expr::NonTerminated(left_exp)),
        };
    }

    // inner expressions should never be terminated so this check to match_semicolon checks if the
    // calling function wants the expression to have the option of being terminated or not.
    if match_semicolon && peek_op_token.token.token_matches(&Token::Semicolon) {
        lexer.next();
        Ok(Expr::Terminated(left_exp))
    } else {
        Ok(Expr::NonTerminated(left_exp))
    }
}

fn parse_array_index(lexer: &mut PeekLex, left: ExprBase) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    let _lbracket = lexer
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
    if let Err(e) = expect_peek(lexer, Token::RBracket) {
        error.extend_assign(e);
    };
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(ExprBase::IndexExpression(IndExpr {
            array: Box::new(left),
            index: Box::new(index.unwrap()),
        }))
    }
}

fn parse_array(lexer: &mut PeekLex) -> Result<Vec<ExprBase>, ParseError> {
    parse_call_args(lexer, Token::RBracket)
}

fn parse_method_expression(
    lexer: &mut PeekLex,
    instance: ExprBase,
) -> Result<ExprBase, ParseError> {
    let operator_dot = lexer
        .next()
        .expect("The operator should already peeked and found and should always be a Dot");
    let op_precedence = operator_dot.token.precedence();
    let method = parse_expression(lexer, op_precedence, false)?.expect_non_terminated();
    Ok(ExprBase::MethodCall(MethCall {
        instance: Box::new(instance),
        method: Box::new(method),
    }))
}

fn parse_call_expression(lexer: &mut PeekLex, function: ExprBase) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    if let Err(e) = expect_peek(lexer, Token::LParen) {
        error.extend_assign(e);
    }
    let args = match parse_call_args(lexer, Token::RParen) {
        Ok(args) => Some(args),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(ExprBase::CallExpression(CallExpr {
            function: Box::new(function),
            args: args.expect("If there are no errors then the parsed args are present"),
        }))
    }
}

// A function to parse the arguments to a function when it is being called
fn parse_call_args(lexer: &mut PeekLex, end_token: Token) -> Result<Vec<ExprBase>, ParseError> {
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

    let mut peek = lexer.peek().map(|val| val.to_owned());
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.token {
                Token::Comma => {
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
                    lexer.next();
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
        peek = lexer.peek().map(|val| val.to_owned());
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(arguments)
    }
}

fn parse_func_literal(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    if let Err(e) = expect_peek(lexer, Token::LParen) {
        if let Some(error) = error.as_mut() {
            error.extend_one(e);
        } else {
            error = Some(e);
        };
    }
    let identifiers = match parse_function_parameters(lexer) {
        Ok(identifiers) => Some(identifiers),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Err(e) = expect_peek(lexer, Token::LBrace) {
        error.extend_assign(e);
    }
    let body = match parse_statements(lexer, true) {
        Ok(statements) => Some(Scope::new(statements)),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(ExprBase::FuncLiteral(FnLiteral {
            parameters: identifiers
                .expect("If there are no errors then the identifers are present"),
            body: body.expect("If there are no errors then the function body is present"),
        }))
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

    let mut peek = lexer.peek().map(|val| val.to_owned());
    while again {
        match peek {
            Some(lok_tok) => match lok_tok.token {
                Token::Ident(_) => {
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
                Token::Comma => {
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
                Token::RParen => {
                    again = false;
                    lexer.next();
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
        peek = lexer.peek().map(|val| val.to_owned());
    }

    if let Some(e) = error {
        Err(e)
    } else {
        Ok(identifiers)
    }
}

fn parse_grouped_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let mut error: Option<Report<ParseError>> = None;
    // let _lparen = lexer.borrow_mut().next();
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
    if let Err(e) = expect_peek(lexer, Token::RParen) {
        error.extend_assign(e);
    }
    if let Some(e) = error {
        Err(e)
    } else {
        Ok(exp.expect("If there are no errors then the expression is present"))
    }
}

fn parse_if_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
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
    if let Err(e) = expect_peek(lexer, Token::LBrace) {
        error.extend_assign(e);
    };
    let consequence = match parse_statements(lexer, true) {
        Ok(statements) => Some(statements),
        Err(e) => {
            error.extend_assign(e);
            None
        }
    };
    let alternate_opt = expect_peek(lexer, Token::Else);
    let alternative = match alternate_opt {
        Err(_) => None,
        Ok(_) => {
            let if_or_lbrace = expect_peek(lexer, Token::If);
            match if_or_lbrace {
                Err(_) => {
                    if let Err(e) = expect_peek(lexer, Token::LBrace) {
                        error.extend_assign(e);
                    }
                    match parse_statements(lexer, true) {
                        Ok(statements) => Some(ElseIfExpr::Else(Scope::new(statements))),
                        Err(e) => {
                            error.extend_assign(e);
                            None
                        }
                    }
                }
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

fn parse_prefix_expression(lexer: &mut PeekLex) -> Result<ExprBase, ParseError> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let right = lexer.peek().map(|val| val.to_owned());
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

fn parse_binary_expression(lexer: &mut PeekLex, left: ExprBase) -> Result<ExprBase, ParseError> {
    let operator = lexer
        .next()
        .expect("The operator was already peeked and found");
    let op_precedence = operator.token.precedence();
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
    Ok(ExprBase::BinaryExpression(BinExp {
        lhs: Box::new(left),
        operator,
        rhs: Box::new(rhs),
    }))
}

// A function that
fn expect_peek(lexer: &mut PeekLex, expected: Token) -> error_stack::Result<(), ParseError> {
    let peek = lexer.peek().map(|val| val.to_owned());
    match peek {
        Some(lok_tok) => {
            if lok_tok.token.token_matches(&expected) {
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

fn is_peek(lexer: &mut PeekLex, expected: Token) -> error_stack::Result<(), ParseError> {
    let peek = lexer.peek().map(|val| val.to_owned());
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
