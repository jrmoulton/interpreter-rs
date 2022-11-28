use std::{
    error::Error,
    fmt::{Debug, Display},
};

type CompObj = Object<()>;

use bytecode::OpCode;
use compiler::Compiler;
use error_stack::Result;
use object::Object;

pub struct VM {
    constants: Vec<CompObj>,
    globals: Vec<CompObj>,
    bytecode: Vec<OpCode>,
    stack: Vec<CompObj>,
    ip: usize,
    sp: usize,
}
impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VM")
            .field("constants", &self.constants)
            .field("bytecode", &self.bytecode)
            .field("sp", &self.sp)
            .field("top_of_stack", &self.stack[self.sp])
            .finish()
    }
}

#[derive(Debug)]
pub enum VMError {
    UnexpectedType,
}
impl Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Error for VMError {}

impl VM {
    pub fn new(compiler: Compiler) -> Self {
        let (bytecode, constants) = compiler.get_fields();
        Self {
            constants,
            bytecode,
            globals: Vec::new(),
            stack: vec![().into(); 100],
            sp: 0,
            ip: 0,
        }
    }

    pub fn run(&mut self) -> Result<CompObj, VMError> {
        let instruction_len = self.bytecode.len();
        while self.ip < instruction_len {
            let op = self.bytecode[self.ip];
            use OpCode::*;
            match op {
                Const(idx) => {
                    self.sp += 1;
                    self.stack[self.sp] = self.constants[idx].clone();
                },
                CreateGlobal => {
                    let obj = self.pop();
                    self.globals.push(obj);
                },
                SetGlobal(idx) => {
                    self.globals[idx] = self.pop();
                },
                GetGlobal(idx) => {
                    self.sp += 1;
                    self.stack[self.sp] = std::mem::replace(&mut self.globals[idx], ().into());
                },
                Add | Sub | Mul | Div | Equal | NotEqual | GreaterThan | LessThan => {
                    self.execute_binary_expression(op)
                },
                Neg | Bang => self.execute_prefix_expression(op),
                Positive => unreachable!("This opcode wont actually be generated"),
                Pop => self.sp -= 1,
                Jump(idx) => self.ip = idx - 1,
                JumpNotTruthy(idx) => {
                    // pop the bool and check if false
                    if !(self.pop().expect_bool().ok_or(VMError::UnexpectedType)?) {
                        self.ip = idx - 1;
                    }
                },
                Print => println!("{}", self.stack[self.sp]),
                NoOp => {}, // _ => todo!(),
            }
            self.ip += 1;
        }
        Ok(self.stack[self.sp].clone())
    }

    fn execute_prefix_expression(&mut self, op: OpCode) {
        let left = self.pop();
        self.sp += 1;
        self.stack[self.sp] = match left {
            CompObj::Integer(int) => match op {
                OpCode::Neg => (-int).into(),
                OpCode::Positive => unreachable!("Not actually ever going to get this instruction"),
                OpCode::Bang => (!int).into(),
                _ => unreachable!(),
            },
            CompObj::Boolean(bool) => match op {
                OpCode::Bang => (!bool).into(),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        };
    }

    fn execute_binary_expression(&mut self, op: OpCode) {
        let right = self.pop();
        let left = self.pop();
        self.sp += 1;
        self.stack[self.sp] = match (left, right) {
            (CompObj::Integer(left_int), CompObj::Integer(right_int)) => match op {
                OpCode::Add => (left_int + right_int).into(),
                OpCode::Sub => (left_int - right_int).into(),
                OpCode::Mul => (left_int * right_int).into(),
                OpCode::Div => (left_int / right_int).into(),
                OpCode::Equal => (left_int == right_int).into(),
                OpCode::NotEqual => (left_int != right_int).into(),
                OpCode::GreaterThan => (left_int > right_int).into(),
                OpCode::LessThan => (left_int < right_int).into(),
                _ => unreachable!(),
            },
            (CompObj::String(left_str), CompObj::String(right_str)) => match op {
                OpCode::Add => (left_str + &right_str).into(),
                OpCode::Equal => (left_str == right_str).into(),
                OpCode::NotEqual => (left_str != right_str).into(),
                _ => unimplemented!("Bad operator for string"),
            },
            _ => {
                unimplemented!("Add error here");
            },
        }
    }

    fn pop(&mut self) -> CompObj {
        let temp = std::mem::replace(&mut self.stack[self.sp], ().into());
        self.sp -= 1;
        temp
    }
}

#[cfg(test)]
mod tests;
