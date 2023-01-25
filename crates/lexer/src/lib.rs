use std::{
    fmt::{Debug, Display},
    mem::discriminant,
};

use error_stack::{Context, IntoReport, Report, Result, ResultExt};

mod tests;

#[derive(Debug)]
pub enum LexerError {
    InvalidUtf8,
    UnknownChar,
    IntegerOverflow,
    UnterminatedString,
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for LexerError {}

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
impl Token {
    fn default_from_row_col(row: usize, col: usize) -> Self {
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

#[derive(Debug, Clone)]
pub struct PeekLex {
    iter: Lexer,
    /// Remember a peeked value, even if it was None.
    peeked: [Option<Option<Token>>; 2],
}
impl Iterator for PeekLex {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.peeked[0].take() {
            Some(p1) => {
                self.peeked[0] = self.peeked[1].take();
                p1
            },
            None => match self.iter.next_token() {
                Ok(val) => val,
                Err(_) => None,
            },
        }
    }
}
impl PeekLex {
    pub fn new(iter: Lexer) -> Self {
        Self { iter, peeked: [None, None] }
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
        match self.peeked.get(0).unwrap() {
            Some(_p1) => self.peeked.get(0).unwrap().as_ref().unwrap().as_ref(),
            None => match self.peeked.get(1).unwrap() {
                Some(_p2) => {
                    self.peeked[0] = self.peeked[1].take();
                    self.peeked.get(0).unwrap().as_ref().unwrap().as_ref()
                },
                None => {
                    self.peeked[0] = Some(iter.next_token().unwrap());
                    self.peeked.get(0).unwrap().as_ref().unwrap().as_ref()
                },
            },
        }
    }

    pub fn peek2(&mut self) -> Option<&Token> {
        let iter = &mut self.iter;
        match self.peeked.get(1).unwrap() {
            Some(_p2) => self.peeked.get(1).unwrap().as_ref().unwrap().as_ref(),
            None => match self.peeked.get(0).unwrap() {
                Some(_p1) => {
                    self.peeked[1] = Some(iter.next_token().unwrap());
                    self.peeked.get(1).unwrap().as_ref().unwrap().as_ref()
                },
                None => {
                    self.peeked[0] = Some(iter.next_token().unwrap());
                    self.peeked[1] = Some(iter.next_token().unwrap());
                    self.peeked.get(1).unwrap().as_ref().unwrap().as_ref()
                },
            },
        }
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
            input: input.split('\n').map(String::from).collect(),
            line: 0,
            column: 0,
        }
    }

    fn update(&mut self, input: String) {
        self.input
            .extend(input.split('\n').map(String::from).collect::<Vec<_>>());
    }

    // if at end of line move to next, eat white space, if at end of line move to
    // next, eat white space
    fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
        let cur_line = loop {
            match self.input.get(self.line) {
                Some(line_str) => {
                    let line = line_str.as_bytes();
                    while self.column < line.len() && (line[self.column] as char).is_whitespace() {
                        self.column += 1;
                    }
                    if self.column < line.len() {
                        break line;
                    } else {
                        self.line += 1;
                        self.column = 0;
                    }
                },
                None => return Ok(None),
            }
        };

        let mut token = Token::default_from_row_col(self.line, self.column);

        if let Some(ch) = cur_line.get(self.column) {
            match *ch as char {
                '0'..='9' => {
                    let mut len = 1;
                    while self.column + len < cur_line.len()
                        && matches!(cur_line[self.column + len] as char, '0'..='9')
                    {
                        len += 1;
                    }
                    token.span.end_col += len - 1;
                    token.kind = TokenKind::Int(
                        std::str::from_utf8(&cur_line[self.column..self.column + len])
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
                },
                '+' => {
                    token.kind = TokenKind::Plus;
                },
                '(' => {
                    token.kind = TokenKind::LParen;
                },
                ')' => {
                    token.kind = TokenKind::RParen;
                },
                '{' => {
                    token.kind = TokenKind::LBrace;
                },
                '}' => {
                    token.kind = TokenKind::RBrace;
                },
                '[' => {
                    token.kind = TokenKind::LBracket;
                },
                ']' => {
                    token.kind = TokenKind::RBracket;
                },
                ',' => {
                    token.kind = TokenKind::Comma;
                },
                ';' => {
                    token.kind = TokenKind::Semicolon;
                },
                '-' => {
                    token.kind = TokenKind::Minus;
                },
                '*' => {
                    token.kind = TokenKind::Asterisk;
                },
                '<' => {
                    token.kind = TokenKind::LT;
                },
                '>' => {
                    token.kind = TokenKind::GT;
                },
                '.' => {
                    token.kind = TokenKind::Dot;
                },
                '=' => match cur_line.get(self.column + 1) {
                    Some(c) if *c as char == '=' => {
                        token.kind = TokenKind::Eq;
                        self.column += 1;
                    },
                    _ => token.kind = TokenKind::Assign,
                },
                '!' => match cur_line.get(self.column + 1) {
                    Some(c) if *c as char == '=' => {
                        token.kind = TokenKind::Ne;
                        self.column += 1;
                    },
                    _ => token.kind = TokenKind::Bang,
                },
                '&' => match cur_line.get(self.column + 1) {
                    Some(c) if *c as char == '&' => {
                        token.kind = TokenKind::And;
                        token.span.end_col += 1;
                        self.column += 1;
                    },
                    _ => token.kind = TokenKind::BitAnd,
                },
                '|' => match cur_line.get(self.column + 1) {
                    Some(c) if *c as char == '|' => {
                        token.kind = TokenKind::Or;
                        token.span.end_col += 1;
                        self.column += 1;
                    },
                    _ => token.kind = TokenKind::BitOr,
                },
                '/' => match cur_line.get(self.column + 1) {
                    Some(c) if *c as char == '/' => {
                        let end_index = self.column
                            + 2
                            + cur_line[self.column + 2..]
                                .iter()
                                .take_while(|c| **c as char != '\n')
                                .count();
                        token.span.end_col += end_index - self.column;
                        token.kind = TokenKind::Comment(
                            std::str::from_utf8(&cur_line[self.column + 2..end_index])
                                .map_err(|e| {
                                    Report::new(e).change_context(LexerError::InvalidUtf8)
                                })?
                                .into(),
                        );
                        self.column = end_index;
                    },
                    _ => token.kind = TokenKind::Slash,
                },
                '"' => {
                    let mut len = 1;
                    while self.column + len < cur_line.len() {
                        let current_char = cur_line[self.column + len] as char;
                        let prev_char = cur_line.get(self.column + len - 1).map(|c| *c as char);
                        if current_char == '"' && prev_char != Some('\\') {
                            break;
                        } else if current_char == '\n' {
                            return Err(Report::new(LexerError::UnterminatedString).attach(Span {
                                start_row: self.line,
                                start_col: self.column,
                                end_row: self.line,
                                end_col: self.column + len,
                            }));
                        }
                        len += 1;
                    }
                    token.span.end_col += len;
                    token.kind = TokenKind::String(
                        std::str::from_utf8(&cur_line[self.column + 1..self.column + len])
                            .map_err(|e| Report::new(e).change_context(LexerError::InvalidUtf8))?
                            .into(),
                    );
                    self.column += len;
                },
                'A'..='Z' | 'a'..='z' => {
                    let mut len = 1;
                    while self.column + len < cur_line.len()
                        && matches!(cur_line[self.column+len] as char, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_')
                    {
                        len += 1;
                    }
                    token.span.end_col += len - 1;
                    match std::str::from_utf8(&cur_line[self.column..self.column + len])
                        .map_err(|e| Report::new(e).change_context(LexerError::InvalidUtf8))?
                    {
                        "let" => {
                            token.kind = TokenKind::Let;
                        },
                        "fn" => {
                            token.kind = TokenKind::Func;
                        },
                        "true" => {
                            token.kind = TokenKind::True;
                        },
                        "false" => {
                            token.kind = TokenKind::False;
                        },
                        "if" => {
                            token.kind = TokenKind::If;
                        },
                        "else" => {
                            token.kind = TokenKind::Else;
                        },
                        "return" => {
                            token.kind = TokenKind::Return;
                        },
                        "for" => {
                            token.kind = TokenKind::For;
                        },
                        "in" => {
                            token.kind = TokenKind::In;
                        },
                        "mut" => {
                            token.kind = TokenKind::Mut;
                        },
                        "break" => {
                            token.kind = TokenKind::Break;
                        },
                        "continue" => {
                            token.kind = TokenKind::Continue;
                        },
                        "loop" => {
                            token.kind = TokenKind::Loop;
                        },
                        "while" => {
                            token.kind = TokenKind::While;
                        },
                        ident => {
                            token.kind = TokenKind::Ident(ident.into());
                        },
                    };
                    self.column += len - 1;
                },
                _ => {
                    token.kind = TokenKind::Illegal;
                },
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
            },
        }
    }
}
