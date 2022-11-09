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
            Self::LT => "`<`".into(),
            Self::GT => "`>`".into(),
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
            LT => LessGreat,
            GT => LessGreat,
            Eq => Equals,
            Ne => Equals,
            Or => LogicOr,
            And => LogicAnd,
            TokenKind::BitOr => Precedence::BitOr,
            TokenKind::BitAnd => Precedence::BitAnd,
            _ => Lowest,
        }
    }

    pub fn is_expr_start(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Int(_)
                | String(_)
                | If
                | LParen
                | LBrace
                | LBracket
                | Minus
                | Bang
                | True
                | False
                | Func
        )
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
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

#[derive(Debug, Clone)]
pub struct PeekLex {
    iter: Lexer,
    /// Remember a peeked value, even if it was None.
    peeked: Option<Option<Token>>,
}
impl Iterator for PeekLex {
    type Item = Token;
    fn next(&mut self) -> Option<Token> {
        match self.peeked.take() {
            Some(v) => v,
            None => match self.iter.next_token() {
                Ok(val) => val,
                Err(e) => {
                    println!("{:?}", e);
                    None
                }
            },
        }
    }
}
impl PeekLex {
    pub fn new(iter: Lexer) -> Self {
        Self { iter, peeked: None }
    }

    pub fn update(&mut self, input: String) {
        self.iter.update(input)
    }

    pub fn get_input(&self) -> &Vec<String> {
        &self.iter.input
    }
}
impl PeekLex
where
    PeekLex: Iterator,
{
    pub fn peek(&mut self) -> Option<&Token> {
        let iter = &mut self.iter;
        self.peeked
            .get_or_insert_with(|| iter.next_token().unwrap_or(None))
            .as_ref()
    }
}

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    input: Vec<String>,
    line: usize,
    column: usize,
}
impl Lexer {
    pub fn new(input: String) -> Self {
        #[cfg(any(not(debug_assertions), test))]
        {
            use std::panic::Location;
            Report::install_debug_hook::<Location>(|_value, _context| {});
        }
        Self {
            input: input.split('\n').map(String::from).collect::<Vec<String>>(),
            line: 0,
            column: 0,
        }
    }

    fn update(&mut self, input: String) {
        self.input.push(input);
    }

    // if at end of line move to next, eat white space, if at end of line move to next, eat white space
    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        let mut line = match self.input.get(self.line) {
            Some(line) => line.as_bytes(),
            None => {
                return Ok(None);
            }
        };
        while line
            .get(self.column)
            .map(|val| *val as char)
            .unwrap_or(' ')
            .is_whitespace()
            || self.column >= line.len()
        {
            if self.column >= line.len() {
                self.line += 1;
                self.column = 0;
                line = match self.input.get(self.line) {
                    Some(line) => line.as_bytes(),
                    None => {
                        return Ok(None);
                    }
                };
            }
            while self.column < line.len() && (line[self.column] as char).is_whitespace() {
                self.column += 1;
            }
        }

        let mut token = Token {
            span: Span {
                start_row: self.line,
                start_col: self.column,
                end_row: self.line,
                end_col: self.column,
            },
            kind: TokenKind::Illegal,
        };
        if let Some(ch) = line.get(self.column) {
            match *ch as char {
                '0'..='9' => {
                    let mut len = 1;
                    while self.column + len < line.len()
                        && matches!(line[self.column + len] as char, '0'..='9')
                    {
                        len += 1;
                    }
                    token.span.end_col += len - 1;
                    token.kind = TokenKind::Int(
                        std::str::from_utf8(&line[self.column..self.column + len])
                            .into_report()
                            .change_context(LexerError::InvalidUtf8)?
                            .parse::<i64>()
                            .into_report()
                            .change_context(LexerError::IntegerOverflow)
                            .attach_printable(
                                "Got a number that doesn't fit into a signed 64 bit integer",
                            )?,
                    );
                    self.column += len - 1;
                }
                '=' => {
                    token.kind = TokenKind::Assign;
                    if let Some(ch) = line.get(self.column + 1) {
                        if *ch as char == '=' {
                            token.kind = TokenKind::Eq;
                            self.column += 1;
                        }
                    }
                }
                '+' => {
                    token.kind = TokenKind::Plus;
                }
                '(' => {
                    token.kind = TokenKind::LParen;
                }
                ')' => {
                    token.kind = TokenKind::RParen;
                }
                '{' => {
                    token.kind = TokenKind::LBrace;
                }
                '}' => {
                    token.kind = TokenKind::RBrace;
                }
                '[' => {
                    token.kind = TokenKind::LBracket;
                }
                ']' => {
                    token.kind = TokenKind::RBracket;
                }
                ',' => {
                    token.kind = TokenKind::Comma;
                }
                ';' => {
                    token.kind = TokenKind::Semicolon;
                }
                '-' => {
                    token.kind = TokenKind::Minus;
                }
                '!' => {
                    token.kind = TokenKind::Bang;
                    if let Some(ch) = line.get(self.column + 1) {
                        if *ch as char == '=' {
                            token.kind = TokenKind::Ne;
                            self.column += 1;
                        }
                    }
                }
                '*' => {
                    token.kind = TokenKind::Asterisk;
                }
                '/' => {
                    if line[self.column + 1] as char == '/' {
                        let mut len = 1;
                        // While in bounds
                        while self.column + len < line.len()
                        // and char is not ending quote or new line
                        && line[self.column + len] as char != '\n'
                        {
                            len += 1;
                        }
                        token.span.end_col += len;
                        token.kind = TokenKind::Comment(
                            std::str::from_utf8(&line[self.column + 2..self.column + len])
                                .map_err(|e| {
                                    Report::new(e).change_context(LexerError::InvalidUtf8)
                                })?
                                .into(),
                        );
                        self.column += len;
                    } else {
                        token.kind = TokenKind::Slash;
                    }
                }
                '<' => {
                    token.kind = TokenKind::LT;
                }
                '>' => {
                    token.kind = TokenKind::GT;
                }
                '.' => {
                    token.kind = TokenKind::Dot;
                }
                '&' => {
                    if line[self.column + 1] as char == '&' {
                        token.kind = TokenKind::And;
                        token.span.end_col += 1;
                        self.column += 1;
                    } else {
                        token.kind = TokenKind::BitAnd;
                    }
                }
                '|' => {
                    if line[self.column + 1] as char == '|' {
                        token.kind = TokenKind::Or;
                        token.span.end_col += 1;
                        self.column += 1;
                    } else {
                        token.kind = TokenKind::BitOr;
                    }
                }
                '"' => {
                    let mut len = 1;
                    // While in bounds
                    while self.column + len < line.len()
                        // and char is not ending quote or new line
                        && !matches!(line[self.column + len] as char, '"' | '\n')
                        // and if there is a ending quote a new line make sure it's not escaped
                        || (matches!(line[self.column + len] as char, '"' | '\n') && line[self.column + len - 1] as char == '\\')
                    {
                        len += 1;
                    }
                    token.span.end_col += len;
                    token.kind = TokenKind::String(
                        std::str::from_utf8(&line[self.column + 1..self.column + len])
                            .map_err(|e| Report::new(e).change_context(LexerError::InvalidUtf8))?
                            .into(),
                    );
                    self.column += len;
                }
                'A'..='Z' | 'a'..='z' => {
                    let mut len = 1;
                    while self.column + len < line.len()
                        && matches!(line[self.column+len] as char, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
                    {
                        len += 1;
                    }
                    token.span.end_col += len - 1;
                    match std::str::from_utf8(&line[self.column..self.column + len]) {
                        Ok("let") => {
                            token.kind = TokenKind::Let;
                        }
                        Ok("fn") => {
                            token.kind = TokenKind::Func;
                        }
                        Ok("true") => {
                            token.kind = TokenKind::True;
                        }
                        Ok("false") => {
                            token.kind = TokenKind::False;
                        }
                        Ok("if") => {
                            token.kind = TokenKind::If;
                        }
                        Ok("else") => {
                            token.kind = TokenKind::Else;
                        }
                        Ok("return") => {
                            token.kind = TokenKind::Return;
                        }
                        Ok("for") => {
                            token.kind = TokenKind::For;
                        }
                        Ok("in") => {
                            token.kind = TokenKind::In;
                        }
                        Ok("mut") => {
                            token.kind = TokenKind::Mut;
                        }
                        Ok("break") => {
                            token.kind = TokenKind::Break;
                        }
                        Ok("continue") => {
                            token.kind = TokenKind::Continue;
                        }
                        Ok("loop") => {
                            token.kind = TokenKind::Loop;
                        }
                        Ok("while") => {
                            token.kind = TokenKind::While;
                        }
                        Ok(ident) => {
                            token.kind = TokenKind::Ident(ident.into());
                        }
                        Err(e) => {
                            return Err(Report::new(e).change_context(LexerError::InvalidUtf8));
                        }
                    };
                    self.column += len - 1;
                }
                _ => {
                    token.kind = TokenKind::Illegal;
                }
            }; // End of main match statement matching on first chars
        } else {
            return Ok(None);
        }
        self.column += 1;
        Ok(Some(token))
    }
}
impl Iterator for Lexer {
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
