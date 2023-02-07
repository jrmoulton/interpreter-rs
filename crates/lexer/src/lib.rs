mod error;
mod lexer;
mod tests;
mod token;

use std::fmt::Debug;

pub use lexer::Lexer;
pub use token::{Precedence, Span, Token, TokenKind};

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
where PeekLex: Iterator
{
    pub fn peek(&mut self) -> Option<&Token> {
        let iter = &mut self.iter;
        match self.peeked[0] {
            Some(ref p1) => p1.as_ref(),
            None => {
                self.peeked[0] = iter.next_token().ok();
                self.peeked[0].as_ref().unwrap().as_ref()
            },
        }
    }

    pub fn peek2(&mut self) -> Option<&Token> {
        let iter = &mut self.iter;
        match self.peeked[1] {
            Some(ref p2) => p2.as_ref(),
            None => match self.peeked[0] {
                Some(ref _p1) => {
                    self.peeked[1] = iter.next_token().ok();
                    self.peeked[1].as_ref().unwrap().as_ref()
                },
                None => {
                    self.peeked[0] = iter.next_token().ok();
                    self.peeked[1] = iter.next_token().ok();
                    self.peeked[1].as_ref().unwrap().as_ref()
                },
            },
        }
    }
}
