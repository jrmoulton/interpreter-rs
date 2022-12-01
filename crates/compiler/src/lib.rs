mod structs;
mod symbol_table;
use std::rc::Rc;

use bytecode::OpCode;
use error_stack::Result;
use lexer::{Span, Token};
use parser::structs::{ElseIfExpr, Expr, Scope, Statement};
pub use structs::*;
pub use symbol_table::Symbol;
use symbol_table::SymbolTable;

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
    pub fn compile(&mut self, statements: impl IntoIterator<Item = Statement>) -> Vec<OpCode> {
        let mut bytecode = Vec::new();
        let mut symbol_table = SymbolTable::new();
        for statement in statements.into_iter() {
            self.compile_statement(statement, &mut bytecode, &mut symbol_table);
        }
        self.fill_const_vec();
        bytecode
    }

    fn compile_statement(
        &mut self, statement: Statement, bytecode: &mut Vec<OpCode>, symbol_table: &mut SymbolTable,
    ) -> Result<(), CompileError> {
        match statement {
            Statement::Let { ident, expr, span } => {
                let ident: Rc<str> = Rc::from(ident.as_str());
                self.compile_expr_base(expr, bytecode, symbol_table);
                symbol_table.define(ident);
                bytecode.push(OpCode::CreateGlobal);
            },
            Statement::Assign { ref ident, expr, span } => {
                self.compile_expr_base(expr, bytecode, symbol_table);
                let symbol = symbol_table.resolve(ident).unwrap();
                bytecode.push(OpCode::SetGlobal(symbol.index));
                bytecode.push(OpCode::Pop); // Pop the expr (this pop
                                            // matches the semicolon)
            },
            Statement::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.compile_expr_base(expr, bytecode, symbol_table)?;
                }
                bytecode.push(OpCode::Jump(9999));
            },
            Statement::Expression { expr, terminated, .. } => {
                self.compile_expr_base(expr, bytecode, symbol_table);
                if terminated {
                    bytecode.push(OpCode::Pop);
                }
            },
        };
        Ok(())
    }

    fn compile_expr_base(
        &mut self, expr_base: Expr, bytecode: &mut Vec<OpCode>, symbol_table: &mut SymbolTable,
    ) -> Result<(), CompileError> {
        use parser::structs::Expr::*;
        match expr_base {
            IntLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = self.constants.entry(val.into()).or_insert(len);
                bytecode.push(OpCode::Const(*idx));
            },
            BoolLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = if val {
                    self.constants.entry(true.into()).or_insert(len)
                } else {
                    self.constants.entry(false.into()).or_insert(len)
                };
                bytecode.push(OpCode::Const(*idx));
            },
            StringLiteral { val, .. } => {
                let len = self.constants.len();
                let idx = self.constants.entry(val.into()).or_insert(len);
                bytecode.push(OpCode::Const(*idx))
            },
            Identifier { ref ident, .. } => {
                let symbol = symbol_table
                    .resolve(ident)
                    .ok_or(CompileError::UnknownIdentifier)?;
                bytecode.push(OpCode::GetGlobal(symbol.index));
            },
            Array { exprs, span } => todo!(),
            FuncDef { parameters, body, span } => {
                let func_bytecode = self.compile(body);
                let len = self.constants.len();
                let idx = self
                    .constants
                    .entry(object::Object::CompFunc(func_bytecode))
                    .or_insert(len);
                bytecode.push(OpCode::Const(*idx));
            },
            FuncCall { function, args, span } => {
                // self.bytecode.push(OpCode::Const(*idx));
                todo!()
            },
            Scope { statements, span } => todo!(),
            Prefix { operator, expression, span } => {
                self.compile_prefix_expr(operator, *expression, span, bytecode, symbol_table)
            },
            Binary { lhs, operator, rhs, span } => {
                self.compile_binary_expr(*lhs, operator, *rhs, span, bytecode, symbol_table)
            },
            Index { array, index, span } => todo!(),
            MethodCall { instance, method, span } => todo!(),
            If {
                condition,
                consequence,
                alternative,
                span,
            } => {
                self.compile_if_expr(
                    *condition,
                    consequence,
                    alternative,
                    span,
                    bytecode,
                    symbol_table,
                );
            },
        };
        Ok(())
    }

    fn compile_if_expr(
        &mut self, condition: Expr, consequence: Scope, alternative: Option<ElseIfExpr>,
        span: Span, bytecode: &mut Vec<OpCode>, symbol_table: &mut SymbolTable,
    ) -> Result<(), CompileError> {
        self.compile_expr_base(condition, bytecode, symbol_table);
        // Push the conditional jump and store its position so it can be modified later
        bytecode.push(OpCode::JumpNotTruthy(9999));
        let test_jump_pos = bytecode.len() - 1;

        bytecode.extend(self.compile(consequence));
        // Push the Jump and store it's position so that it can be modified later
        bytecode.push(OpCode::Jump(9999));
        let jump_pos = bytecode.len() - 1;

        match alternative {
            // If there is an alternative compile it
            Some(alt) => {
                match alt {
                    // if expressions are recursively compiled
                    parser::structs::ElseIfExpr::ElseIf(if_expr) => {
                        self.compile_expr_base((*if_expr).into(), bytecode, symbol_table)?;
                    },
                    // the else condition is handled. Here we now know how long the conditional jump
                    // needs to be. The conditional jump go to the start of the else
                    parser::structs::ElseIfExpr::Else(scope) => {
                        let curr_len = bytecode.len();
                        std::mem::replace(
                            &mut bytecode[test_jump_pos],
                            OpCode::JumpNotTruthy(curr_len),
                        );
                        // Start of the else statements. Else compiled
                        bytecode.extend(self.compile(scope));
                    },
                };
            },
            // If there is no alternate then we set the conditional jump to this spot and then add
            // an empty object (so that popping doesn't cause an underflow on the stack). This
            // makes it so that even if there is no alternate an object is created there for
            // consistency
            None => {
                let curr_len = bytecode.len();
                std::mem::replace(
                    &mut bytecode[test_jump_pos],
                    OpCode::JumpNotTruthy(curr_len),
                );
                bytecode.push(OpCode::Const(0));
            },
        }
        // Finally set the final jump positon to after the end of the if statement
        let curr_len = bytecode.len();
        std::mem::replace(&mut bytecode[jump_pos], OpCode::Jump(curr_len));
        Ok(())
    }

    fn compile_prefix_expr(
        &mut self, operator: Token, expression: Expr, span: Span, bytecode: &mut Vec<OpCode>,
        symbol_table: &mut SymbolTable,
    ) {
        self.compile_expr_base(expression, bytecode, symbol_table);
        self.compile_op_token(operator, OpType::Prefix, bytecode, symbol_table);
    }

    fn compile_binary_expr(
        &mut self, lhs: Expr, operator: Token, rhs: Expr, span: Span, bytecode: &mut Vec<OpCode>,
        symbol_table: &mut SymbolTable,
    ) {
        self.compile_expr_base(lhs, bytecode, symbol_table);
        self.compile_expr_base(rhs, bytecode, symbol_table);
        self.compile_op_token(operator, OpType::Binary, bytecode, symbol_table);
    }

    fn compile_op_token(
        &mut self, token: Token, op_type: OpType, bytecode: &mut Vec<OpCode>,
        symbol_table: &mut SymbolTable,
    ) {
        let len = self.constants.len();
        match token.kind {
            lexer::TokenKind::Assign => todo!(),
            lexer::TokenKind::Plus => match op_type {
                OpType::Prefix => {},
                OpType::Binary => bytecode.push(OpCode::Add),
                _ => unreachable!(),
            },
            lexer::TokenKind::Minus => match op_type {
                OpType::Binary => bytecode.push(OpCode::Sub),
                OpType::Prefix => bytecode.push(OpCode::Neg),
                _ => unreachable!(),
            },
            lexer::TokenKind::Slash => bytecode.push(OpCode::Div),
            lexer::TokenKind::Asterisk => match op_type {
                OpType::Binary => bytecode.push(OpCode::Mul),
                OpType::Prefix => unimplemented!(),
                _ => unreachable!(),
            },
            lexer::TokenKind::Bang => bytecode.push(OpCode::Bang),
            lexer::TokenKind::BitAnd => todo!(),
            lexer::TokenKind::BitOr => todo!(),
            lexer::TokenKind::Or => todo!(),
            lexer::TokenKind::And => todo!(),
            lexer::TokenKind::LT => bytecode.push(OpCode::LessThan),
            lexer::TokenKind::GT => bytecode.push(OpCode::GreaterThan),
            lexer::TokenKind::Eq => bytecode.push(OpCode::Equal),
            lexer::TokenKind::Ne => bytecode.push(OpCode::NotEqual),
            lexer::TokenKind::LBracket => todo!(),
            lexer::TokenKind::RBracket => todo!(),
            lexer::TokenKind::Dot => todo!(),
            _ => unreachable!(),
        }
    }

    fn compile_string(&mut self, s: String, bytecode: &mut Vec<OpCode>) {
        let len = self.constants.len();
        let idx = self.constants.entry(s.into()).or_insert(len);
        bytecode.push(OpCode::Const(*idx))
    }
}

#[cfg(test)]
mod tests;
