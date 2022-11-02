use std::{
    error::Error,
    fmt::{Debug, Display},
};

use error_stack::Result;
use evaluator::object::Object;

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

    fn execute_binary_expression(&mut self, op: OpCode) {
        let right = self.pop();
        let left = self.pop();
        self.stack[self.sp] = match (left, right) {
            (Object::Integer(left_int), Object::Integer(right_int)) => match op {
                OpCode::Add => (left_int.get_value() + right_int.get_value()).into(),
                OpCode::Sub => (left_int.get_value() - right_int.get_value()).into(),
                OpCode::Mul => (left_int.get_value() * right_int.get_value()).into(),
                OpCode::Div => (left_int.get_value() / right_int.get_value()).into(),
                _ => unreachable!(),
            },
            (Object::String(_), Object::String(_)) => todo!(),
            _ => {
                unimplemented!("Add error here");
            }
        }
    }

    pub fn run(&mut self) -> Result<evaluator::object::Object, VMError> {
        let mut ip = 0;
        let instruction_len = self.bytecode.len();
        while ip < instruction_len {
            let op = self.bytecode[ip];
            match op {
                OpCode::Const(idx) => {
                    self.sp += 1;
                    self.stack[self.sp] = self.constants[idx].clone();
                }
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div => {
                    self.execute_binary_expression(op)
                }
                OpCode::Neg => {
                    let left = self.stack[self.sp]
                        .clone()
                        .expect_int()
                        .ok_or(VMError::UnexpectedType)?;
                    self.stack[self.sp] = (-left).into()
                }
                OpCode::Pos => unreachable!("This opcode wont actually be generated"),
                OpCode::Pop => self.sp -= 1,
                OpCode::Print => println!("{}", self.stack[self.sp]),
            }
            ip += 1;
        }
        Ok(self.stack[self.sp].clone())
    }

    fn pop(&mut self) -> Object {
        let temp = self.stack[self.sp].clone();
        self.sp -= 1;
        temp
    }
}

#[cfg(test)]
mod tests;
