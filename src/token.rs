use std::fmt;

#[derive(PartialEq, Clone)]
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

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Illegal(_) => f.write_fmt(format_args!("Type: Illegal\tLiteral: {self}")),
            Self::Eof => f.write_fmt(format_args!("Type: Eof\tLiteral: {self}")),
            Self::Ident(_) => f.write_fmt(format_args!("Type: Ident\tLiteral: {self}")),
            Self::Int(_) => f.write_fmt(format_args!("Type: Int\tLiteral: {self}")),
            Self::Assign => f.write_fmt(format_args!("Type: Assign\tLiteral: {self}")),
            Self::Plus => f.write_fmt(format_args!("Type: Plus\tLiteral: {self}")),
            Self::Minus => f.write_fmt(format_args!("Type: Minus\tLiteral: {self}")),
            Self::Bang => f.write_fmt(format_args!("Type: Bang\tLiteral: {self}")),
            Self::Asterisk => f.write_fmt(format_args!("Type: Asterisk\tLiteral: {self}")),
            Self::Slash => f.write_fmt(format_args!("Type: Slash\tLiteral: {self}")),
            Self::Lt => f.write_fmt(format_args!("Type: Lt\tLiteral: {self}")),
            Self::Gt => f.write_fmt(format_args!("Type: Gt\tLiteral: {self}")),
            Self::Eq => f.write_fmt(format_args!("Type: Eq\tLiteral: {self}")),
            Self::NotEq => f.write_fmt(format_args!("Type: NotEq\tLiteral: {self}")),
            Self::Comma => f.write_fmt(format_args!("Type: Comma\tLiteral: {self}")),
            Self::Semicolon => f.write_fmt(format_args!("Type: Semicolon\tLiteral: {self}")),
            Self::LParen => f.write_fmt(format_args!("Type: LParen\tLiteral: {self}")),
            Self::RParen => f.write_fmt(format_args!("Type: RParen\tLiteral: {self}")),
            Self::LBrace => f.write_fmt(format_args!("Type: LBrace\tLiteral: {self}")),
            Self::RBrace => f.write_fmt(format_args!("Type: RBrace\tLiteral: {self}")),
            Self::Function => f.write_fmt(format_args!("Type: Function\tLiteral: {self}")),
            Self::Let => f.write_fmt(format_args!("Type: Let\tLiteral: {self}")),
            Self::True => f.write_fmt(format_args!("Type: True\tLiteral: {self}")),
            Self::False => f.write_fmt(format_args!("Type: False\tLiteral: {self}")),
            Self::If => f.write_fmt(format_args!("Type: If\tLiteral: {self}")),
            Self::Else => f.write_fmt(format_args!("Type: Else\tLiteral: {self}")),
            Self::Return => f.write_fmt(format_args!("Type: Return\tLiteral: {self}")),
        }
    }
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
