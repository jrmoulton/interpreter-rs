use error_stack::{IntoReport, Report, Result, ResultExt};

use crate::{
    error::LexerError,
    token::{Span, Token, TokenKind},
};

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    pub(crate) input: Vec<String>,
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

    pub(crate) fn update(&mut self, input: String) {
        self.input
            .extend(input.split('\n').map(String::from).collect::<Vec<_>>());
    }

    // if at end of line move to next, eat white space, if at end of line move to
    // next, eat white space
    pub(crate) fn next_token(&mut self) -> Result<Option<Token>, LexerError> {
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
                    token.kind = TokenKind::Lt;
                },
                '>' => {
                    token.kind = TokenKind::Gt;
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
