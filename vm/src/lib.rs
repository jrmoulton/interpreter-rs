use std::{
    error::Error,
    fmt::{Debug, Display},
};

use error_stack::Result;
use evaluator::object::{EmptyWrapper, Object};

use bytecode::OpCode;

pub struct VM {
    constants: Vec<evaluator::object::Object>,
    bytecode: Vec<OpCode>,
    stack: Vec<evaluator::object::Object>,
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
    pub fn new(constants: Vec<evaluator::object::Object>, bytecode: Vec<OpCode>) -> Self {
        Self {
            constants,
            bytecode,
            stack: vec![
                evaluator::object::Empty::new(evaluator::object::EmptyWrapper::new()).into();
                100
            ],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<evaluator::object::Object, VMError> {
        let mut ip = 0;
        let instruction_len = self.bytecode.len();
        while ip < instruction_len {
            let op = self.bytecode[ip];
            use OpCode::*;
            match op {
                Const(idx) => {
                    self.sp += 1;
                    self.stack[self.sp] = self.constants[idx].clone();
                }
                Add | Sub | Mul | Div | Equal | NotEqual | GreaterThan | LessThan => {
                    self.execute_binary_expression(op)
                }
                Neg | Bang => self.execute_prefix_expression(op),
                Positive => unreachable!("This opcode wont actually be generated"),
                Pop => self.sp -= 1,
                Jump(idx) => ip = idx - 1,
                JumpNotTruthy(idx) => {
                    // pop the bool and check if false
                    if !(self.pop().expect_bool().ok_or(VMError::UnexpectedType)?) {
                        ip = idx - 1;
                    }
                }
                Print => println!("{}", self.stack[self.sp]),
                NoOp => {}
                _ => todo!(),
            }
            ip += 1;
        }
        Ok(self.stack[self.sp].clone())
    }

    fn execute_prefix_expression(&mut self, op: OpCode) {
        let left = self.pop();
        self.sp += 1;
        self.stack[self.sp] = match left {
            Object::Integer(int) => match op {
                OpCode::Neg => (-int.get_value()).into(),
                OpCode::Positive => unreachable!("Not actually ever going to get this instruction"),
                OpCode::Bang => (!int.get_value()).into(),
                _ => unreachable!(),
            },
            Object::Boolean(bool) => match op {
                OpCode::Bang => (!bool.get_value()).into(),
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
            (Object::Integer(left_int), Object::Integer(right_int)) => match op {
                OpCode::Add => (left_int.get_value() + right_int.get_value()).into(),
                OpCode::Sub => (left_int.get_value() - right_int.get_value()).into(),
                OpCode::Mul => (left_int.get_value() * right_int.get_value()).into(),
                OpCode::Div => (left_int.get_value() / right_int.get_value()).into(),
                OpCode::Equal => (left_int.get_value() == right_int.get_value()).into(),
                OpCode::NotEqual => (left_int.get_value() != right_int.get_value()).into(),
                OpCode::GreaterThan => (left_int.get_value() > right_int.get_value()).into(),
                OpCode::LessThan => (left_int.get_value() < right_int.get_value()).into(),
                _ => unreachable!(),
            },
            (Object::String(left_str), Object::String(right_str)) => match op {
                OpCode::Add => (left_str.get_value().to_owned() + right_str.get_value()).into(),
                OpCode::Equal => (left_str.get_value() == right_str.get_value()).into(),
                OpCode::NotEqual => (left_str.get_value() != right_str.get_value()).into(),
                _ => unimplemented!("Bad operator for string"),
            },
            _ => {
                unimplemented!("Add error here");
            }
        }
    }

    fn pop(&mut self) -> Object {
        let temp = std::mem::replace(&mut self.stack[self.sp], EmptyWrapper::new().into());
        self.sp -= 1;
        temp
    }
}

#[cfg(test)]
mod tests;
