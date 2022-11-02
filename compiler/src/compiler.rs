use parser::structs::Statement;

use bytecode::OpCode;

#[derive(Debug)]
pub struct Compiler {
    pub constants: Vec<evaluator::object::Object>,
    pub bytecode: Vec<OpCode>,
}
enum OpType {
    Binary,
    Prefix,
    Other,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            bytecode: Vec::new(),
        }
    }
}
impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(dead_code, unused)]
impl Compiler {
    pub fn compile(&mut self, statements: &mut impl Iterator<Item = Statement>) {
        for statement in statements {
            self.compile_statement(statement);
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Let { ident, expr, span } => todo!(),
            Statement::Assign { ident, expr, span } => todo!(),
            Statement::Return(_) => todo!(),
            Statement::Expression(expr) => self.compile_expression(expr),
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
            BoolLiteral(_) => todo!(),
            StringLiteral(_) => todo!(),
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
            } => todo!(),
        }
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
        match token.kind {
            lexer::TokenKind::Illegal => todo!(),
            lexer::TokenKind::Eof => todo!(),
            lexer::TokenKind::Comment(_) => todo!(),
            lexer::TokenKind::Let => todo!(),
            lexer::TokenKind::Mut => todo!(),
            lexer::TokenKind::Func => todo!(),
            lexer::TokenKind::True => todo!(),
            lexer::TokenKind::False => todo!(),
            lexer::TokenKind::If => todo!(),
            lexer::TokenKind::Else => todo!(),
            lexer::TokenKind::Return => todo!(),
            lexer::TokenKind::For => todo!(),
            lexer::TokenKind::In => todo!(),
            lexer::TokenKind::Break => todo!(),
            lexer::TokenKind::Continue => todo!(),
            lexer::TokenKind::Loop => todo!(),
            lexer::TokenKind::While => todo!(),
            lexer::TokenKind::Ident(_) => todo!(),
            lexer::TokenKind::Int(int_const) => {
                self.constants
                    .push(evaluator::object::Integer::new(int_const).into());
                self.bytecode.push(OpCode::Const(self.constants.len() - 1));
            }
            lexer::TokenKind::String(_) => todo!(),
            lexer::TokenKind::Assign => todo!(),
            lexer::TokenKind::Plus => match op_type {
                OpType::Prefix => {}
                OpType::Binary => self.bytecode.push(OpCode::Add),
                _ => unreachable!(),
                // self.bytecode.push(Opcode::IAdd)
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
            lexer::TokenKind::Bang => todo!(),
            lexer::TokenKind::BitAnd => todo!(),
            lexer::TokenKind::BitOr => todo!(),
            lexer::TokenKind::Or => todo!(),
            lexer::TokenKind::And => todo!(),
            lexer::TokenKind::LT => todo!(),
            lexer::TokenKind::GT => todo!(),
            lexer::TokenKind::Eq => todo!(),
            lexer::TokenKind::Ne => todo!(),
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

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use expect_test::expect_file;
//     use lexer::{Lexer, PeekLex};
//     use parser::parse;

//     fn new_compiler(code: &'static str) -> VM {
//         let lexer = Lexer::new(code.into());
//         let mut peek_lex = PeekLex::new(lexer);
//         let mut ast = parse(&mut peek_lex).unwrap().into_iter();
//         let mut compiler = Compiler::new();
//         compiler.compile(&mut ast);
//         let mut bytecode = compiler.bytecode;
//         // bytecode.push(crate::bytecode::OpCode::Print);
//         VM::new(compiler.constants, bytecode)
//     }

//     #[test]
//     fn single_int() {
//         let code: &'static str = r#"57"#;
//         let mut vm = new_vm(code);
//         vm.run().unwrap();
//     }
// }
