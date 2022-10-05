use error_stack::{Context, IntoReport, Report, Result, ResultExt};
use std::{
    fmt::{Debug, Display},
    mem::discriminant,
};

mod tests;

#[derive(Debug)]
pub enum LexerError {
    InvalidUtf8,
    UnknownChar,
    IntegerOverflow,
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for LexerError {}

#[derive(PartialEq, Eq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    LogicOr = 1,
    LogicAnd = 2,
    BitOr = 3,
    BitAnd = 4,
    Equals = 5,
    LessGreat = 6,
    Sum = 7,
    Product = 8,
    Prefix = 9,
    // This matches the Dot and it needs to be less than call
    Method = 10,
    Call = 11,
    Index = 12,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    // Extra stuff
    Illegal,
    Eof,

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
    LT,
    GT,
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret: String = match self {
            Token::Illegal => "`Illegal`".into(),
            Token::Eof => "`End of File`".into(),
            Token::Let => "`let`".into(),
            Token::Mut => "`mut`".into(),
            Token::Func => "`fn`".into(),
            Token::True => "`true`".into(),
            Token::False => "`false`".into(),
            Token::If => "`if`".into(),
            Token::Else => "`else`".into(),
            Token::Return => "`return`".into(),
            Token::For => "`for`".into(),
            Token::In => "`in`".into(),
            Token::Break => "`break`".into(),
            Token::Continue => "`continue`".into(),
            Token::Loop => "`loop`".into(),
            Token::While => "`while`".into(),
            Token::Ident(ident) => format!("`{ident}`"),
            Token::Int(int) => format!("`{int}`"),
            Token::String(string) => format!("`{string}`"),
            Token::Assign => "`=`".into(),
            Token::Plus => "`+`".into(),
            Token::Minus => "`-`".into(),
            Token::Slash => "`/`".into(),
            Token::Asterisk => "`*`".into(),
            Token::Bang => "`!`".into(),
            Token::BitAnd => "`&`".into(),
            Token::BitOr => "`|`".into(),
            Token::Or => "`||`".into(),
            Token::And => "`&&`".into(),
            Token::LT => "`<`".into(),
            Token::GT => "`>`".into(),
            Token::Eq => "`==`".into(),
            Token::Ne => "`!=`".into(),
            Token::Comma => "`,`".into(),
            Token::Semicolon => "`;`".into(),
            Token::LParen => "`(`".into(),
            Token::RParen => "`)`".into(),
            Token::LBrace => "`{`".into(),
            Token::RBrace => "`}`".into(),
            Token::LBracket => "`[`".into(),
            Token::RBracket => "`]`".into(),
            Token::Dot => "`.`".into(),
        };
        f.write_str(&ret)
    }
}
impl Token {
    /// Checks if the token type matches without checking the internal data
    pub fn token_matches(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
    pub fn precedence(&self) -> Precedence {
        use Precedence::*;
        use Token::*;
        match self {
            LBracket => Index,
            LParen => Call,
            Dot => Method,
            Plus => Sum,
            Minus => Sum,
            Slash => Product,
            Asterisk => Product,
            LT => LessGreat,
            GT => LessGreat,
            Eq => Equals,
            Ne => Equals,
            Or => LogicOr,
            And => LogicAnd,
            Token::BitOr => Precedence::BitOr,
            Token::BitAnd => Precedence::BitAnd,
            _ => Lowest,
        }
    }
}

#[derive(Clone)]
pub struct LocTok {
    pub line: u32,
    pub column: usize,
    pub abs_pos: usize,
    pub len: usize,
    pub token: Token,
}
impl Debug for LocTok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.token))
    }
}
impl Display for LocTok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.token))
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a [u8],
    len: usize,
    line: u32,
    column: usize,
    pos: usize,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let input = input.as_bytes();
        let len = input.len();
        Self {
            input,
            len,
            line: 0,
            column: 0,
            pos: 0,
        }
    }
    fn next_token(&mut self) -> Result<Option<LocTok>, LexerError> {
        while self.pos < self.len && (self.input[self.pos] as char).is_whitespace() {
            if self.input[self.pos] as char == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
        let mut token = LocTok {
            line: self.line,
            column: self.column,
            abs_pos: self.pos,
            len: 1,
            token: Token::Illegal,
        };
        if let Some(ch) = self.input.get(self.pos) {
            match *ch as char {
                '0'..='9' => {
                    let mut len = 1;
                    while self.pos + len < self.len
                        && matches!(self.input[self.pos + len] as char, '0'..='9')
                    {
                        len += 1;
                    }
                    token.len = len;
                    token.token = Token::Int(
                        std::str::from_utf8(&self.input[self.pos..self.pos + len])
                            .into_report()
                            .change_context(LexerError::InvalidUtf8)?
                            .parse::<i64>()
                            .into_report()
                            .change_context(LexerError::IntegerOverflow)
                            .attach_printable(
                                "Got a number that doesn't fit into a signed 64 bit integer",
                            )?,
                    );
                    self.pos += len - 1;
                    self.column += len - 1;
                }
                '=' => {
                    token.token = Token::Assign;
                    if let Some(ch) = self.input.get(self.pos + 1) {
                        if *ch as char == '=' {
                            token.token = Token::Eq;
                            self.pos += 1;
                            self.column += 1;
                        }
                    }
                }
                '+' => {
                    token.token = Token::Plus;
                }
                '(' => {
                    token.token = Token::LParen;
                }
                ')' => {
                    token.token = Token::RParen;
                }
                '{' => {
                    token.token = Token::LBrace;
                }
                '}' => {
                    token.token = Token::RBrace;
                }
                '[' => {
                    token.token = Token::LBracket;
                }
                ']' => {
                    token.token = Token::RBracket;
                }
                ',' => {
                    token.token = Token::Comma;
                }
                ';' => {
                    token.token = Token::Semicolon;
                }
                '-' => {
                    token.token = Token::Minus;
                }
                '!' => {
                    token.token = Token::Bang;
                    if let Some(ch) = self.input.get(self.pos + 1) {
                        if *ch as char == '=' {
                            token.token = Token::Ne;
                            self.pos += 1;
                            self.column += 1;
                        }
                    }
                }
                '*' => {
                    token.token = Token::Asterisk;
                }
                '/' => {
                    token.token = Token::Slash;
                }
                '<' => {
                    token.token = Token::LT;
                }
                '>' => {
                    token.token = Token::GT;
                }
                '.' => {
                    token.token = Token::Dot;
                }
                '&' => {
                    if self.input[self.pos + 1] as char == '&' {
                        token.token = Token::And;
                        token.len = 2;
                        self.pos += 1;
                        self.column += 1;
                    } else {
                        token.token = Token::BitAnd;
                    }
                }
                '|' => {
                    if self.input[self.pos + 1] as char == '|' {
                        token.token = Token::Or;
                        token.len = 2;
                        self.pos += 1;
                        self.column += 1;
                    } else {
                        token.token = Token::BitOr;
                    }
                }
                '"' => {
                    let mut len = 1;
                    // While in bounds
                    while self.pos + len < self.len
                        // and char is not ending quote or new line
                        && !matches!(self.input[self.pos + len] as char, '"' | '\n')
                        // and if there is a ending quote a new line make sure it's not escaped
                        || (matches!(self.input[self.pos + len] as char, '"' | '\n') && self.input[self.pos + len - 1] as char == '\\')
                    {
                        len += 1;
                    }
                    token.len = len + 1;
                    token.token = Token::String(
                        std::str::from_utf8(&self.input[self.pos + 1..self.pos + len])
                            .map_err(|e| Report::new(e).change_context(LexerError::InvalidUtf8))?
                            .into(),
                    );
                    self.pos += len;
                    self.column += len;
                }
                'A'..='Z' | 'a'..='z' => {
                    let mut len = 1;
                    while self.pos + len < self.len
                        && matches!(self.input[self.pos+len] as char, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
                    {
                        len += 1;
                    }
                    token.len = len;
                    match std::str::from_utf8(&self.input[self.pos..self.pos + len]) {
                        Ok("let") => {
                            token.token = Token::Let;
                        }
                        Ok("fn") => {
                            token.token = Token::Func;
                        }
                        Ok("true") => {
                            token.token = Token::True;
                        }
                        Ok("false") => {
                            token.token = Token::False;
                        }
                        Ok("if") => {
                            token.token = Token::If;
                        }
                        Ok("else") => {
                            token.token = Token::Else;
                        }
                        Ok("return") => {
                            token.token = Token::Return;
                        }
                        Ok("for") => {
                            token.token = Token::For;
                        }
                        Ok("in") => {
                            token.token = Token::In;
                        }
                        Ok("mut") => {
                            token.token = Token::Mut;
                        }
                        Ok("break") => {
                            token.token = Token::Break;
                        }
                        Ok("continue") => {
                            token.token = Token::Continue;
                        }
                        Ok("loop") => {
                            token.token = Token::Loop;
                        }
                        Ok("while") => {
                            token.token = Token::While;
                        }
                        Ok(ident) => {
                            token.token = Token::Ident(ident.into());
                        }
                        Err(e) => {
                            return Err(Report::new(e).change_context(LexerError::InvalidUtf8));
                        }
                    };
                    self.pos += len - 1;
                    self.column += len - 1;
                }
                _ => {
                    token.token = Token::Illegal;
                }
            }; // End of main match statement matching on first chars
        } else {
            return Ok(None);
        }
        self.pos += 1;
        self.column += 1;
        Ok(Some(token))
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = LocTok;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(val) => val,
            Err(e) => {
                println!("{:?}", e);
                None
            }
        }
    }
}
