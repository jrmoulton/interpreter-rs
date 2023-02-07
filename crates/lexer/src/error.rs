use std::fmt::Display;

#[derive(Debug)]
pub enum LexerError {
    InvalidUtf8,
    IntegerOverflow,
    UnterminatedString,
}
impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{self:?}"))
    }
}
impl std::error::Error for LexerError {}
