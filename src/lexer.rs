use error_stack::{Context, IntoReport, Report, Result, ResultExt};
use std::fmt::Display;

#[derive(Debug)]
pub enum LexerError {
    InvalidUtf8,
    UnknownChar,
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl Context for LexerError {}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    // Extra stuff
    Illegal,
    Eof,

    // keywords
    Let,
    Func,
    True,
    False,
    If,
    Else,
    Return,
    For,
    In,

    // Ident
    Ident(String),

    // Literals
    Int(i32),

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

#[derive(Debug, Clone)]
pub(crate) struct LocTok {
    line: u32,
    column: usize,
    abs_pos: usize,
    len: usize,
    pub(crate) token: Token,
}

#[derive(Debug)]
pub(crate) struct Lexer<'a> {
    input: &'a [u8],
    len: usize,
    line: u32,
    column: usize,
    pos: usize,
}
impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a [u8], len: usize) -> Self {
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
            self.pos += 1;
            self.column += 1;
            if self.input[self.pos] as char == '\n' {
                self.line += 1;
                self.column = 0;
            }
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
                            .parse::<i32>()
                            .expect("All character 0-9 starting optionally with `-` should be able to parse into an i32"),
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
                        Ok(ident) => {
                            token.len = ident.len();
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
mod test {
    use super::*;
    use pretty_assertions::assert_eq;
    // use pretty_assertions::assert_ne;
    use Token::*;

    #[test]
    fn single_expr() {
        let code: &'static str = r#"5 + 5"#;
        let correct = vec![Int(5), Plus, Int(5)];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_let() {
        let code: &'static str = r#"let"#;
        let correct = vec![Let];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_assign() {
        let code: &'static str = r#"let five = 5;"#;
        let correct = vec![Let, Ident("five".into()), Assign, Int(5), Semicolon];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn double_assign() {
        let code: &'static str = r#"let five = 5;let ten = 10;"#;
        #[rustfmt::skip]
        let correct = vec![
            Let, Ident("five".into()), Assign, Int(5), Semicolon, Let, Ident("ten".into()), Assign, Int(10), Semicolon,
        ];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_func() {
        let code: &'static str = r#"let add = fn(x, y) {
                x - y
            };"#;
        #[rustfmt::skip]
        let correct = vec![
            Let,
            Ident("add".into()), Assign, Func, LParen, Ident("x".into()), Comma, Ident("y".into()), RParen, LBrace, Ident("x".into()),
            Minus, Ident("y".into()), RBrace, Semicolon,
        ];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn comprehensive() {
        let code: &'static str = r#"
                let five = 5;
                let ten = 10;
                   let add = fn(x, y) {
                     x - y;
                };
                   let result = add(five, ten);
                   !-/*5;
                   5 < 10 > 5;
                   if (5 < 10) {
                       return true;
                   } else {
                       return false;
                }
                10 == 10; 10 != 9;"#;
        #[rustfmt::skip]
            let correct = vec![
                Let, Ident("five".into()), Assign, Int(5), Semicolon,
                Let, Ident("ten".into()), Assign, Int(10), Semicolon,
                Let, Ident("add".into()), Assign, Func, LParen, Ident("x".into()), Comma, Ident("y".into()), RParen, LBrace,
                Ident("x".into()), Minus, Ident("y".into()), Semicolon, 
                RBrace, Semicolon,
                Let, Ident("result".into()), Assign, Ident("add".into()), LParen, Ident("five".into()), Comma, Ident("ten".into()), RParen, Semicolon,
                Bang, Minus, Slash, Asterisk, Int(5), Semicolon,
                Int(5), LT, Int(10), GT, Int(5), Semicolon,
                If, LParen, Int(5), LT, Int(10), RParen, LBrace,
                Return, True, Semicolon,
                RBrace, Else, LBrace, 
                Return, False, Semicolon,
                RBrace,
                Int(10), Eq, Int(10), Semicolon, Int(10), Ne, Int(9), Semicolon
            ];
        let lexer = Lexer::new(code.as_bytes(), code.len());
        assert_eq!(
            correct,
            lexer
                .into_iter()
                .map(|lok_tok| lok_tok.token)
                .collect::<Vec<_>>()
        );
    }
}
