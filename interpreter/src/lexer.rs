use error_stack::{Context, IntoReport, Report, Result, ResultExt};
use std::{fmt::Display, mem::discriminant};

use crate::parser::structs::Suggestion;

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

#[derive(PartialEq, PartialOrd)]
pub(crate) enum Precedence {
    Lowest = 0,
    Equals = 1,
    LessGreat = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
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

    // Operators
    Assign,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Bang,

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
}
impl Token {
    /// Checks if the token type matches without checking the internal data
    pub(crate) fn token_matches(&self, other: &Self) -> bool {
        discriminant(self) == discriminant(other)
    }
    pub(crate) fn precedence(&self) -> Precedence {
        use Precedence::*;
        use Token::*;
        match self {
            LParen => Call,
            Plus => Sum,
            Minus => Sum,
            Slash => Product,
            Asterisk => Product,
            LT => LessGreat,
            GT => LessGreat,
            Eq => Equals,
            Ne => Equals,
            _ => Lowest,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct LocTok {
    line: u32,
    column: usize,
    abs_pos: usize,
    len: usize,
    pub(crate) token: Token,
}

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'a> {
    input: &'a [u8],
    len: usize,
    line: u32,
    column: usize,
    pos: usize,
}
impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
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
                            .report()
                            .change_context(LexerError::InvalidUtf8)?
                            .parse::<i64>()
                            .report()
                            .change_context(LexerError::IntegerOverflow)
                            .attach_printable("Found a number that doesn't fit into an i64")
                            .attach(Suggestion(
                                "The largest value that can fit into a 64 int is ...",
                            ))?,
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
        self.next_token().unwrap()
    }
}

#[cfg(test)]
mod test;
