mod structs;
pub use structs::*;

use lexer::Span;
use parser::structs::{ElseIfExpr, ExprBase, Scope, Statement};

use bytecode::OpCode;

#[allow(dead_code, unused)]
impl Compiler {
    pub fn compile(&mut self, statements: impl IntoIterator<Item = Statement>) {
        for statement in statements.into_iter() {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Let { ident, expr, span } => {
                self.compile_expr_base(expr);
                self.compile_token(ident, OpType::Other);
                self.bytecode.push(OpCode::SetGlobal);
                self.bytecode.push(OpCode::Pop); // Pop the ident
                self.bytecode.push(OpCode::Pop); // Pop the expr (this pop matches the semicolon)
            }
            Statement::Assign { ident, expr, span } => todo!(),
            Statement::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.compile_expr_base(expr)
                }
                self.bytecode.push(OpCode::Jump(9999))
            }
            Statement::Expression(expr) => self.compile_expr_base(expr),
        }
    }

    fn compile_expression(&mut self, expr: parser::structs::Expr) {
        match expr {
            parser::structs::Expr::Terminated(expr_base) => {
                self.compile_expr_base(expr_base);
                self.bytecode.push(OpCode::Pop);
            }
            parser::structs::Expr::NonTerminated(expr_base) => self.compile_expr_base(expr_base),
        }
    }

    fn compile_expr_base(&mut self, expr_base: parser::structs::ExprBase) {
        use parser::structs::ExprBase::*;
        match expr_base {
            IntLiteral(token) => self.compile_token(token, OpType::Other),
            BoolLiteral(token) => self.compile_token(token, OpType::Other),
            StringLiteral(token) => self.compile_token(token, OpType::Other),
            Identifier(_) => todo!(),
            Array { exprs, span } => todo!(),
            Func {
                parameters,
                body,
                span,
            } => todo!(),
            Call {
                function,
                args,
                span,
            } => todo!(),
            Scope { statements, span } => todo!(),
            Prefix {
                operator,
                expression,
                span,
            } => self.compile_prefix_expr(operator, *expression, span),
            Binary {
                lhs,
                operator,
                rhs,
                span,
            } => self.compile_binary_expr(*lhs, operator, *rhs, span),
            Index { array, index, span } => todo!(),
            MethodCall {
                instance,
                method,
                span,
            } => todo!(),
            If {
                condition,
                consequence,
                alternative,
                span,
            } => {
                self.compile_if_expr(*condition, consequence, alternative, span);
            }
        }
    }

    fn compile_if_expr(
        &mut self,
        condition: ExprBase,
        consequence: Scope,
        alternative: Option<ElseIfExpr>,
        span: Span,
    ) {
        // compile the condition
        self.compile_expr_base(condition);

        // Push the conditional jump and store its position so it can be modified later
        self.bytecode.push(OpCode::JumpNotTruthy(9999));
        let test_jump_pos = self.bytecode.len() - 1;

        // compile the consequence scope/statements
        self.compile(consequence);

        // Push the Jump and store it's position so that it can be modified later
        self.bytecode.push(OpCode::Jump(9999));
        let jump_pos = self.bytecode.len() - 1;

        match alternative {
            // If there is an alternative compile it
            Some(alt) => {
                match alt {
                    // if expressions are recursively compiled
                    parser::structs::ElseIfExpr::ElseIf(if_expr) => {
                        self.compile_expr_base((*if_expr).into())
                    }
                    // the else condition is handled. Here we now know how long the conditional jump
                    // needs to be. The conditional jump go to the start of the else
                    parser::structs::ElseIfExpr::Else(scope) => {
                        let curr_len = self.bytecode.len();
                        std::mem::replace(
                            &mut self.bytecode[test_jump_pos],
                            OpCode::JumpNotTruthy(curr_len),
                        );
                        // Start of the else statements. Else compiled
                        self.compile(scope);
                    }
                }
            }
            // If there is no alternate then we set the conditional jump to this spot and then add
            // an empty object (so that popping doesn't cause an underflow on the stack). This
            // makes it so that even if there is no alternate an object is created there for
            // consistency
            None => {
                let curr_len = self.bytecode.len();
                std::mem::replace(
                    &mut self.bytecode[test_jump_pos],
                    OpCode::JumpNotTruthy(curr_len),
                );
                self.bytecode.push(OpCode::Const(0));
            }
        }
        // Finally set the final jump positon to after the end of the if statement
        let curr_len = self.bytecode.len();
        std::mem::replace(&mut self.bytecode[jump_pos], OpCode::Jump(curr_len));
    }

    fn compile_prefix_expr(
        &mut self,
        operator: lexer::Token,
        expression: parser::structs::ExprBase,
        span: lexer::Span,
    ) {
        self.compile_expr_base(expression);
        self.compile_token(operator, OpType::Prefix);
    }

    fn compile_binary_expr(
        &mut self,
        lhs: parser::structs::ExprBase,
        operator: lexer::Token,
        rhs: parser::structs::ExprBase,
        span: lexer::Span,
    ) {
        self.compile_expr_base(lhs);
        self.compile_expr_base(rhs);
        self.compile_token(operator, OpType::Binary);
    }

    fn compile_token(&mut self, token: lexer::Token, op_type: OpType) {
        let len = self.constants.len();
        match token.kind {
            lexer::TokenKind::Illegal => todo!(),
            lexer::TokenKind::Eof => todo!(),
            lexer::TokenKind::Comment(_) => todo!(),
            lexer::TokenKind::Let => todo!(),
            lexer::TokenKind::Mut => todo!(),
            lexer::TokenKind::Func => todo!(),
            lexer::TokenKind::True => {
                let idx = self.constants.entry(true.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx))
            }
            lexer::TokenKind::False => {
                let idx = self.constants.entry(false.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx))
            }
            lexer::TokenKind::If => todo!(),
            lexer::TokenKind::Else => todo!(),
            lexer::TokenKind::Return => todo!(),
            lexer::TokenKind::For => todo!(),
            lexer::TokenKind::In => todo!(),
            lexer::TokenKind::Break => todo!(),
            lexer::TokenKind::Continue => todo!(),
            lexer::TokenKind::Loop => todo!(),
            lexer::TokenKind::While => todo!(),
            lexer::TokenKind::Ident(ident_str) => {
                let idx = self.constants.entry(ident_str.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx));
            }
            lexer::TokenKind::Int(int_const) => {
                let idx = self.constants.entry(int_const.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx))
            }
            lexer::TokenKind::String(str_constant) => {
                let idx = self.constants.entry(str_constant.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx))
            }
            lexer::TokenKind::Assign => todo!(),
            lexer::TokenKind::Plus => match op_type {
                OpType::Prefix => {}
                OpType::Binary => self.bytecode.push(OpCode::Add),
                _ => unreachable!(),
            },
            lexer::TokenKind::Minus => match op_type {
                OpType::Binary => self.bytecode.push(OpCode::Sub),
                OpType::Prefix => self.bytecode.push(OpCode::Neg),
                _ => unreachable!(),
            },
            lexer::TokenKind::Slash => self.bytecode.push(OpCode::Div),
            lexer::TokenKind::Asterisk => match op_type {
                OpType::Binary => self.bytecode.push(OpCode::Mul),
                OpType::Prefix => unimplemented!(),
                _ => unreachable!(),
            },
            lexer::TokenKind::Bang => self.bytecode.push(OpCode::Bang),
            lexer::TokenKind::BitAnd => todo!(),
            lexer::TokenKind::BitOr => todo!(),
            lexer::TokenKind::Or => todo!(),
            lexer::TokenKind::And => todo!(),
            lexer::TokenKind::LT => self.bytecode.push(OpCode::LessThan),
            lexer::TokenKind::GT => self.bytecode.push(OpCode::GreaterThan),
            lexer::TokenKind::Eq => self.bytecode.push(OpCode::Equal),
            lexer::TokenKind::Ne => self.bytecode.push(OpCode::NotEqual),
            lexer::TokenKind::Comma => todo!(),
            lexer::TokenKind::Semicolon => unreachable!(),
            lexer::TokenKind::LParen => todo!(),
            lexer::TokenKind::RParen => todo!(),
            lexer::TokenKind::LBrace => todo!(),
            lexer::TokenKind::RBrace => todo!(),
            lexer::TokenKind::LBracket => todo!(),
            lexer::TokenKind::RBracket => todo!(),
            lexer::TokenKind::Dot => todo!(),
        }
    }
}

#[cfg(test)]
mod tests;
