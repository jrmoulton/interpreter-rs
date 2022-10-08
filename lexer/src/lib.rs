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
pub enum TokenKInd {
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

impl Display for TokenKInd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ret: String = match self {
            TokenKInd::Illegal => "`Illegal`".into(),
            TokenKInd::Eof => "`End of File`".into(),
            TokenKInd::Let => "`let`".into(),
            TokenKInd::Mut => "`mut`".into(),
            TokenKInd::Func => "`fn`".into(),
            TokenKInd::True => "`true`".into(),
            TokenKInd::False => "`false`".into(),
            TokenKInd::If => "`if`".into(),
            TokenKInd::Else => "`else`".into(),
            TokenKInd::Return => "`return`".into(),
            TokenKInd::For => "`for`".into(),
            TokenKInd::In => "`in`".into(),
            TokenKInd::Break => "`break`".into(),
            TokenKInd::Continue => "`continue`".into(),
            TokenKInd::Loop => "`loop`".into(),
            TokenKInd::While => "`while`".into(),
            TokenKInd::Ident(ident) => format!("`{ident}`"),
            TokenKInd::Int(int) => format!("`{int}`"),
            TokenKInd::String(string) => format!("`{string}`"),
            TokenKInd::Assign => "`=`".into(),
            TokenKInd::Plus => "`+`".into(),
            TokenKInd::Minus => "`-`".into(),
            TokenKInd::Slash => "`/`".into(),
            TokenKInd::Asterisk => "`*`".into(),
            TokenKInd::Bang => "`!`".into(),
            TokenKInd::BitAnd => "`&`".into(),
            TokenKInd::BitOr => "`|`".into(),
            TokenKInd::Or => "`||`".into(),
            TokenKInd::And => "`&&`".into(),
            TokenKInd::LT => "`<`".into(),
            TokenKInd::GT => "`>`".into(),
            TokenKInd::Eq => "`==`".into(),
            TokenKInd::Ne => "`!=`".into(),
            TokenKInd::Comma => "`,`".into(),
            TokenKInd::Semicolon => "`;`".into(),
            TokenKInd::LParen => "`(`".into(),
            TokenKInd::RParen => "`)`".into(),
            TokenKInd::LBrace => "`{`".into(),
            TokenKInd::RBrace => "`}`".into(),
            TokenKInd::LBracket => "`[`".into(),
            TokenKInd::RBracket => "`]`".into(),
            TokenKInd::Dot => "`.`".into(),
        };
        f.write_str(&ret)
    }
}
impl TokenKInd {
    /// Checks if the token type matches without checking the internal data
    pub fn token_matches(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
    pub fn precedence(&self) -> Precedence {
        use Precedence::*;
        use TokenKInd::*;
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
            TokenKInd::BitOr => Precedence::BitOr,
            TokenKInd::BitAnd => Precedence::BitAnd,
            _ => Lowest,
        }
    }
}

#[derive(Clone)]
pub struct Span {
    pub line: u32,
    pub column: usize,
    pub abs_pos: usize,
    pub len: usize,
}
impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({},{})->({},{})",
            self.line,
            self.column,
            self.line,
            self.column + self.len
        )
    }
}

#[derive(Clone)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKInd,
}
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Token({:?}, {:?})", self.kind, self.span))
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}", self.kind))
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
    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        while self.pos < self.len && (self.input[self.pos] as char).is_whitespace() {
            if self.input[self.pos] as char == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
        let mut token = Token {
            span: Span {
                line: self.line,
                column: self.column,
                abs_pos: self.pos,
                len: 1,
            },
            kind: TokenKInd::Illegal,
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
                    token.span.len = len;
                    token.kind = TokenKInd::Int(
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
                    token.kind = TokenKInd::Assign;
                    if let Some(ch) = self.input.get(self.pos + 1) {
                        if *ch as char == '=' {
                            token.kind = TokenKInd::Eq;
                            self.pos += 1;
                            self.column += 1;
                        }
                    }
                }
                '+' => {
                    token.kind = TokenKInd::Plus;
                }
                '(' => {
                    token.kind = TokenKInd::LParen;
                }
                ')' => {
                    token.kind = TokenKInd::RParen;
                }
                '{' => {
                    token.kind = TokenKInd::LBrace;
                }
                '}' => {
                    token.kind = TokenKInd::RBrace;
                }
                '[' => {
                    token.kind = TokenKInd::LBracket;
                }
                ']' => {
                    token.kind = TokenKInd::RBracket;
                }
                ',' => {
                    token.kind = TokenKInd::Comma;
                }
                ';' => {
                    token.kind = TokenKInd::Semicolon;
                }
                '-' => {
                    token.kind = TokenKInd::Minus;
                }
                '!' => {
                    token.kind = TokenKInd::Bang;
                    if let Some(ch) = self.input.get(self.pos + 1) {
                        if *ch as char == '=' {
                            token.kind = TokenKInd::Ne;
                            self.pos += 1;
                            self.column += 1;
                        }
                    }
                }
                '*' => {
                    token.kind = TokenKInd::Asterisk;
                }
                '/' => {
                    token.kind = TokenKInd::Slash;
                }
                '<' => {
                    token.kind = TokenKInd::LT;
                }
                '>' => {
                    token.kind = TokenKInd::GT;
                }
                '.' => {
                    token.kind = TokenKInd::Dot;
                }
                '&' => {
                    if self.input[self.pos + 1] as char == '&' {
                        token.kind = TokenKInd::And;
                        token.span.len = 2;
                        self.pos += 1;
                        self.column += 1;
                    } else {
                        token.kind = TokenKInd::BitAnd;
                    }
                }
                '|' => {
                    if self.input[self.pos + 1] as char == '|' {
                        token.kind = TokenKInd::Or;
                        token.span.len = 2;
                        self.pos += 1;
                        self.column += 1;
                    } else {
                        token.kind = TokenKInd::BitOr;
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
                    token.span.len = len + 1;
                    token.kind = TokenKInd::String(
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
                    token.span.len = len;
                    match std::str::from_utf8(&self.input[self.pos..self.pos + len]) {
                        Ok("let") => {
                            token.kind = TokenKInd::Let;
                        }
                        Ok("fn") => {
                            token.kind = TokenKInd::Func;
                        }
                        Ok("true") => {
                            token.kind = TokenKInd::True;
                        }
                        Ok("false") => {
                            token.kind = TokenKInd::False;
                        }
                        Ok("if") => {
                            token.kind = TokenKInd::If;
                        }
                        Ok("else") => {
                            token.kind = TokenKInd::Else;
                        }
                        Ok("return") => {
                            token.kind = TokenKInd::Return;
                        }
                        Ok("for") => {
                            token.kind = TokenKInd::For;
                        }
                        Ok("in") => {
                            token.kind = TokenKInd::In;
                        }
                        Ok("mut") => {
                            token.kind = TokenKInd::Mut;
                        }
                        Ok("break") => {
                            token.kind = TokenKInd::Break;
                        }
                        Ok("continue") => {
                            token.kind = TokenKInd::Continue;
                        }
                        Ok("loop") => {
                            token.kind = TokenKInd::Loop;
                        }
                        Ok("while") => {
                            token.kind = TokenKInd::While;
                        }
                        Ok(ident) => {
                            token.kind = TokenKInd::Ident(ident.into());
                        }
                        Err(e) => {
                            return Err(Report::new(e).change_context(LexerError::InvalidUtf8));
                        }
                    };
                    self.pos += len - 1;
                    self.column += len - 1;
                }
                _ => {
                    token.kind = TokenKInd::Illegal;
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
    type Item = Token;
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
