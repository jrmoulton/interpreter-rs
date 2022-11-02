#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Const(usize),
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Pos,
    Pop,
    Print,
}
