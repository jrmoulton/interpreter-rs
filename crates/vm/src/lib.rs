use std::{
    error::Error,
    fmt::{Debug, Display},
};

use bytecode::OpCode;
use compiler::Compiler;
use error_stack::Result;
use evaluator::object::{EmptyWrapper, Object};

pub struct VM {
    constants: Vec<Object>,
    globals: Vec<Object>,
    bytecode: Vec<OpCode>,
    stack: Vec<Object>,
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
            stack: vec![evaluator::object::EmptyWrapper::new().into(); 100],
            sp: 0,
            ip: 0,
        }
    }

    pub fn run(&mut self) -> Result<Object, VMError> {
        let instruction_len = self.bytecode.len();
        while self.ip < instruction_len {
            let op = self.bytecode[self.ip];
            use OpCode::*;
            match op {
                Const(idx) => {
                    self.push(self.constants[idx].clone());
                },
                CreateGlobal => {
                    let obj = self.pop();
                    self.globals.push(obj);
                },
                SetGlobal(idx) => {
                    self.globals[idx] = self.pop();
                },
                GetGlobal(idx) => {
                    self.push(self.globals[idx].clone());
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
                Array(size) => {
                    let mut ret_vec = Vec::new();
                    for idx in self.sp - size + 1..self.sp + 1 {
                        ret_vec.push({
                            let temp =
                                std::mem::replace(&mut self.stack[idx], EmptyWrapper::new().into());
                            self.sp -= 1;
                            temp
                        });
                    }
                    self.push(evaluator::object::ArrayWrapper::from(ret_vec));
                },
                Index => {
                    use evaluator::object::*;
                    let index = self.pop().expect_int().unwrap();
                    match self.pop() {
                        Object::Array(Array { ref value, .. }) => match value.get(index as usize) {
                            Some(val) => self.push(val.clone()),
                            _ => unimplemented!(),
                        },
                        Object::String(String { ref value, .. }) => {
                            match value.as_bytes().get(index as usize) {
                                Some(val) => {
                                    self.push(Object::String(String::new(
                                        std::string::String::from(*val as char),
                                    )));
                                },
                                _ => unreachable!(),
                            }
                        },
                        _ => unreachable!(),
                    };
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
            },
        }
    }

    fn pop(&mut self) -> Object {
        let temp = std::mem::replace(&mut self.stack[self.sp], EmptyWrapper::new().into());
        self.sp -= 1;
        temp
    }

    fn push(&mut self, obj: impl Into<Object>) {
        self.sp += 1;
        self.stack[self.sp] = obj.into();
    }
}

#[cfg(test)]
mod tests;
