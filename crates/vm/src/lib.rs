use std::{
    error::Error,
    fmt::{Debug, Display},
};

use bytecode::OpCode;
use compiler::object::Object;
use error_stack::Result;

pub struct VM {
    constants: Vec<Object>,
    globals: Vec<Object>,
    stack: Vec<Object>,
    ip: usize,
    sp: usize,
}
impl Debug for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VM")
            .field("constants", &self.constants)
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
    pub fn new(constants: Vec<Object>) -> Self {
        Self {
            constants,
            globals: Vec::new(),
            stack: vec![().into(); 100],
            sp: 1,
            ip: 0,
        }
    }

    pub fn run(&mut self, bytecode: Vec<OpCode>) -> Result<Object, VMError> {
        let instruction_len = bytecode.len();
        while self.ip < instruction_len {
            let op = bytecode[self.ip];
            use OpCode::*;
            match op {
                Const(idx) => {
                    self.stack[self.sp] = self.constants[idx].clone();
                    self.sp += 1;
                },
                CreateGlobal => {
                    let obj = self.pop();
                    self.globals.push(obj);
                },
                SetGlobal(idx) => {
                    self.globals[idx] = self.pop();
                },
                GetGlobal(idx) => {
                    self.stack[self.sp] = std::mem::replace(&mut self.globals[idx], ().into());
                    self.sp += 1;
                },
                Add | Sub | Mul | Div | Equal | NotEqual | GreaterThan | LessThan => {
                    self.execute_binary_expression(op)
                },
                Neg | Bang => self.execute_prefix_expression(op),
                Positive => unreachable!("This opcode wont actually be generated"),
                Call => {
                    let func = self.pop();
                    match func {
                        Object::CompFunc(bytecode) => {
                            let temp_ip = self.ip;
                            self.ip = 0;
                            let temp = self.run(bytecode)?;
                            self.ip = temp_ip;
                            self.stack[self.sp] = temp;
                            self.sp += 1;
                        },
                        _ => unimplemented!(),
                    }
                },
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
        Ok(self.pop())
    }

    fn execute_prefix_expression(&mut self, op: OpCode) {
        let left = self.pop();
        self.stack[self.sp] = match left {
            Object::Integer(int) => match op {
                OpCode::Neg => (-int).into(),
                OpCode::Positive => unreachable!("Not actually ever going to get this instruction"),
                OpCode::Bang => (!int).into(),
                _ => unreachable!(),
            },
            Object::Boolean(bool) => match op {
                OpCode::Bang => (!bool).into(),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        };
        self.sp += 1;
    }

    fn execute_binary_expression(&mut self, op: OpCode) {
        let right = self.pop();
        let left = self.pop();
        self.stack[self.sp] = match (left, right) {
            (Object::Integer(left_int), Object::Integer(right_int)) => match op {
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
            (Object::String(left_str), Object::String(right_str)) => match op {
                OpCode::Add => (left_str + &right_str).into(),
                OpCode::Equal => (left_str == right_str).into(),
                OpCode::NotEqual => (left_str != right_str).into(),
                _ => unimplemented!("Bad operator for string"),
            },
            _ => {
                unimplemented!("Add error here");
            },
        };
        self.sp += 1;
    }

    fn pop(&mut self) -> Object {
        self.sp -= 1;
        std::mem::replace(&mut self.stack[self.sp], ().into())
    }
}

#[cfg(test)]
mod tests;
