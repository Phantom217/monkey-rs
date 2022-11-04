use std::fmt;

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
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

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
    True,
    False,
    If,
    Else,
    Return,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Illegal(c) => f.write_str(&c.to_string()),
            Self::Eof => f.write_str("\0"),
            Self::Ident(ident) => f.write_str(ident),
            Self::Int(int) => f.write_str(&int.to_string()),
            Self::Assign => f.write_str("="),
            Self::Plus => f.write_str("+"),
            Self::Minus => f.write_str("-"),
            Self::Bang => f.write_str("!"),
            Self::Asterisk => f.write_str("*"),
            Self::Slash => f.write_str("/"),
            Self::Lt => f.write_str("<"),
            Self::Gt => f.write_str(">"),
            Self::Eq => f.write_str("=="),
            Self::NotEq => f.write_str("!="),
            Self::Comma => f.write_str(","),
            Self::Semicolon => f.write_str(";"),
            Self::LParen => f.write_str("("),
            Self::RParen => f.write_str(")"),
            Self::LBrace => f.write_str("{"),
            Self::RBrace => f.write_str("}"),
            Self::Function => f.write_str("fn"),
            Self::Let => f.write_str("let"),
            Self::True => f.write_str("true"),
            Self::False => f.write_str("false"),
            Self::If => f.write_str("if"),
            Self::Else => f.write_str("else"),
            Self::Return => f.write_str("return"),
        }
    }
}
