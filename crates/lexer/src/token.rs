use std::{
    fmt::{Debug, Display},
    mem::discriminant,
};

#[derive(PartialEq, Eq, PartialOrd)]
#[repr(u8)]
pub enum Precedence {
    Lowest,
    LogicOr,
    LogicAnd,
    BitOr,
    BitAnd,
    Equals,
    LessGreat,
    Sum,
    Product,
    Prefix,
    // This matches the Dot and it needs to be less than call
    Method,
    Call,
    Index,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Extra stuff
    Illegal,
    Eof,
    Comment(String),

    // keywords
    Let,
    Mut,
    Func,
    True,
    False,
    If,
    Else,
    Return,
    For,
    In,
    Break,
    Continue,
    Loop,
    While,

    // Ident
    Ident(String),

    // Literals
    Int(i64),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Bang,
    BitAnd,
    BitOr,
    Or,
    And,

    // Comparators
    Lt,
    Gt,
    Eq,
    Ne,

    // Separators
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Dot,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret: String = match self {
            Self::Illegal => "`Illegal`".into(),
            Self::Eof => "`End of File`".into(),
            Self::Let => "`let`".into(),
            Self::Mut => "`mut`".into(),
            Self::Func => "`fn`".into(),
            Self::True => "`true`".into(),
            Self::False => "`false`".into(),
            Self::If => "`if`".into(),
            Self::Else => "`else`".into(),
            Self::Return => "`return`".into(),
            Self::For => "`for`".into(),
            Self::In => "`in`".into(),
            Self::Break => "`break`".into(),
            Self::Continue => "`continue`".into(),
            Self::Loop => "`loop`".into(),
            Self::While => "`while`".into(),
            Self::Ident(ident) => format!("`{ident}`"),
            Self::Int(int) => format!("`{int}`"),
            Self::String(string) => format!("`{string}`"),
            Self::Comment(string) => format!("`//{string}`"),
            Self::Assign => "`=`".into(),
            Self::Plus => "`+`".into(),
            Self::Minus => "`-`".into(),
            Self::Slash => "`/`".into(),
            Self::Asterisk => "`*`".into(),
            Self::Bang => "`!`".into(),
            Self::BitAnd => "`&`".into(),
            Self::BitOr => "`|`".into(),
            Self::Or => "`||`".into(),
            Self::And => "`&&`".into(),
            Self::Lt => "`<`".into(),
            Self::Gt => "`>`".into(),
            Self::Eq => "`==`".into(),
            Self::Ne => "`!=`".into(),
            Self::Comma => "`,`".into(),
            Self::Semicolon => "`;`".into(),
            Self::LParen => "`(`".into(),
            Self::RParen => "`)`".into(),
            Self::LBrace => "`{`".into(),
            Self::RBrace => "`}`".into(),
            Self::LBracket => "`[`".into(),
            Self::RBracket => "`]`".into(),
            Self::Dot => "`.`".into(),
        };
        f.write_str(&ret)
    }
}
impl TokenKind {
    /// Checks if the token type matches without checking the internal data
    pub fn is(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }

    pub fn precedence(&self) -> Precedence {
        use Precedence::*;
        use TokenKind::*;
        match self {
            LBracket => Index,
            LParen => Call,
            Dot => Method,
            Plus => Sum,
            Minus => Sum,
            Slash => Product,
            Asterisk => Product,
            Lt => LessGreat,
            Gt => LessGreat,
            Eq => Equals,
            Ne => Equals,
            Or => LogicOr,
            And => LogicAnd,
            TokenKind::BitOr => Precedence::BitOr,
            TokenKind::BitAnd => Precedence::BitAnd,
            _ => Lowest,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub(crate) start_row: usize,
    pub(crate) start_col: usize,
    pub(crate) end_row: usize,
    pub(crate) end_col: usize,
}
impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({},{})->({},{})",
            self.start_row, self.start_col, self.end_row, self.end_col
        )
    }
}
impl core::ops::Add for Span {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start_row: self.start_row,
            start_col: self.start_col,
            end_row: rhs.end_row,
            end_col: rhs.end_col,
        }
    }
}
impl Span {
    pub fn get_row_range(&self, before: usize, after: usize) -> std::ops::RangeInclusive<usize> {
        self.start_row - before..=self.end_row + after
    }

    pub fn get_start_row(&self) -> usize {
        self.start_row
    }

    pub fn get_end_row(&self) -> usize {
        self.end_row
    }

    pub fn get_end_col(&self) -> usize {
        self.end_col
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Token({:?}, {:?})", self.kind, self.span))
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.kind))
    }
}
impl Token {
    pub fn default_from_row_col(row: usize, col: usize) -> Self {
        Self {
            span: Span {
                start_row: row,
                start_col: col,
                end_row: row,
                end_col: col,
            },
            kind: TokenKind::Illegal,
        }
    }
}
