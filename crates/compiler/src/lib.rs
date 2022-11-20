mod structs;
mod symbol_table;
use std::rc::Rc;

use bytecode::OpCode;
use error_stack::Result;
use lexer::{Span, Token};
use parser::structs::{ElseIfExpr, Expr, Scope, Statement};
pub use structs::*;

#[derive(Debug)]
pub enum CompileError {
    UnknownIdentifier,
}
impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
impl std::error::Error for CompileError {}

#[allow(dead_code, unused)]
impl Compiler {
    pub fn compile(&mut self, statements: impl IntoIterator<Item = Statement>) {
        for statement in statements.into_iter() {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), CompileError> {
        match statement {
            Statement::Let { ident, expr, span } => {
                let ident: Rc<str> = Rc::from(ident.as_str());
                self.compile_expr_base(expr);
                self.symbol_table.define(ident);
                self.bytecode.push(OpCode::CreateGlobal);
            },
            Statement::Assign { ref ident, expr, span } => {
                self.compile_expr_base(expr);
                let symbol = self.symbol_table.resolve(ident).unwrap();
                self.bytecode.push(OpCode::SetGlobal(symbol.index));
                self.bytecode.push(OpCode::Pop); // Pop the expr (this pop
                                                 // matches the semicolon)
            },
            Statement::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.compile_expr_base(expr)?;
                }
                self.bytecode.push(OpCode::Jump(9999));
            },
            Statement::Expression { expr, terminated, .. } => {
                self.compile_expr_base(expr);
                if terminated {
                    self.bytecode.push(OpCode::Pop);
                }
            },
        };
        Ok(())
    }

    fn compile_expr_base(&mut self, expr_base: Expr) -> Result<(), CompileError> {
        use parser::structs::Expr::*;
        match expr_base {
            IntLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = self.constants.entry(val.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx));
            },
            BoolLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = if val {
                    self.constants.entry(true.into()).or_insert(len)
                } else {
                    self.constants.entry(false.into()).or_insert(len)
                };
                self.bytecode.push(OpCode::Const(*idx));
            },
            StringLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = self.constants.entry(val.into()).or_insert(len);
                self.bytecode.push(OpCode::Const(*idx))
            },
            Identifier { ref ident, .. } => {
                let symbol = self
                    .symbol_table
                    .resolve(ident)
                    .ok_or(CompileError::UnknownIdentifier)?;
                self.bytecode.push(OpCode::GetGlobal(symbol.index));
            },
            Array { exprs, span } => todo!(),
            FuncDef { parameters, body, span } => todo!(),
            FuncCall { function, args, span } => todo!(),
            Scope { statements, span } => todo!(),
            Prefix { operator, expression, span } => {
                self.compile_prefix_expr(operator, *expression, span)
            },
            Binary { lhs, operator, rhs, span } => {
                self.compile_binary_expr(*lhs, operator, *rhs, span)
            },
            Index { array, index, span } => todo!(),
            MethodCall { instance, method, span } => todo!(),
            If {
                condition,
                consequence,
                alternative,
                span,
            } => {
                self.compile_if_expr(*condition, consequence, alternative, span);
            },
        };
        Ok(())
    }

    fn compile_if_expr(
        &mut self, condition: Expr, consequence: Scope, alternative: Option<ElseIfExpr>, span: Span,
    ) -> Result<(), CompileError> {
        self.compile_expr_base(condition);
        // Push the conditional jump and store its position so it can be modified later
        self.bytecode.push(OpCode::JumpNotTruthy(9999));
        let test_jump_pos = self.bytecode.len() - 1;

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
                        self.compile_expr_base((*if_expr).into())?;
                    },
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
                    },
                };
            },
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
            },
        }
        // Finally set the final jump positon to after the end of the if statement
        let curr_len = self.bytecode.len();
        std::mem::replace(&mut self.bytecode[jump_pos], OpCode::Jump(curr_len));
        Ok(())
    }

    fn compile_prefix_expr(&mut self, operator: Token, expression: Expr, span: Span) {
        self.compile_expr_base(expression);
        self.compile_op_token(operator, OpType::Prefix);
    }

    fn compile_binary_expr(&mut self, lhs: Expr, operator: Token, rhs: Expr, span: Span) {
        self.compile_expr_base(lhs);
        self.compile_expr_base(rhs);
        self.compile_op_token(operator, OpType::Binary);
    }

    fn compile_op_token(&mut self, token: Token, op_type: OpType) {
        let len = self.constants.len();
        match token.kind {
            lexer::TokenKind::Assign => todo!(),
            lexer::TokenKind::Plus => match op_type {
                OpType::Prefix => {},
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
            lexer::TokenKind::LBracket => todo!(),
            lexer::TokenKind::RBracket => todo!(),
            lexer::TokenKind::Dot => todo!(),
            _ => unreachable!(),
        }
    }

    fn compile_string(&mut self, s: String) {
        let len = self.constants.len();
        let idx = self.constants.entry(s.into()).or_insert(len);
        self.bytecode.push(OpCode::Const(*idx))
    }
}

#[cfg(test)]
mod tests;