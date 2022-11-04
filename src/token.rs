#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(char),
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(isize),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
}
