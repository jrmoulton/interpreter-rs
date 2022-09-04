#![allow(dead_code)]

mod lexer {
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

    #[derive(Debug, PartialEq)]
    pub(crate) enum Token {
        Illegal,
        Eof,
        Let,
        Func,
        Ident(&'static str),
        Int(i32),
        Assign,
        Plus,
        Comma,
        Semicolon,
        LParen,
        RParen,
        LBrace,
        RBrace,
    }

    #[derive(Debug)]
    pub(crate) struct LocTok {
        line: u32,
        column: u32,
        abs_pos: usize,
        len: usize,
        pub(crate) token: Token,
    }

    pub(crate) struct Lexer {
        input: &'static [u8],
        len: usize,
        line: u32,
        column: u32,
        pos: usize,
    }
    impl Lexer {
        pub(crate) fn new(input: &'static [u8], len: usize) -> Self {
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
                    }
                    '=' => {
                        token.token = Token::Assign;
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
                    'A'..='Z' | 'a'..='z' => {
                        let mut len = 1;
                        while self.pos + len < self.len
                            && matches!(self.input[self.pos+len] as char, 'A'..='Z' | 'a'..='z' | '0'..='9' |'_')
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
                            Ok(ident) => {
                                token.len = ident.len();
                                token.token = Token::Ident(ident);
                            }
                            Err(e) => {
                                return Err(Report::new(e).change_context(LexerError::InvalidUtf8));
                            }
                        };
                        self.pos += len - 1;
                    }
                    _ => {
                        token.token = Token::Illegal;
                    }
                }; // End of main match statement matching on first chars
            } else {
                return Ok(None);
            }
            self.column += 1;
            self.pos += 1;
            Ok(Some(token))
        }
    }
    impl Iterator for Lexer {
        type Item = LocTok;
        fn next(&mut self) -> Option<Self::Item> {
            self.next_token().unwrap()
        }
    }
}

#[cfg(test)]
mod tests {
    mod lexer {
        use super::super::lexer::*;
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
        fn single_assign() {
            let code: &'static str = r#"let five = 5;"#;
            let correct = vec![Ident("five"), Assign, Int(5), Semicolon];
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
            let correct = vec![
                Let,
                Ident("five"),
                Assign,
                Int(5),
                Semicolon,
                Let,
                Ident("ten"),
                Assign,
                Int(10),
                Semicolon,
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
    // let code: &'static str = r#"let five = 5; let ten = 10;
    // let add = fn(x, y) { x + y;
    // };
    // let result = add(five, ten);"#;
    // let lexer = Lexer::new(code);
}
