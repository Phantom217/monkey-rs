use std::fmt;

use crate::token::Token;

#[derive(Debug)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, Expr),                      // Let(Identifier, Value)
    Return(std::marker::PhantomData<Expr>), // Return(Value)
    Expression(Expr),                       // Expression(Value)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(s, e) => write!(f, "let {s} = {e};"),
            Self::Return(e) => write!(f, "return {e:?};"), // TODO: remove debug print
            Self::Expression(e) => write!(f, "{e}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Boolean(bool),
    Function(Vec<Expr>, BlockStatement), // parameters, body
    Identifier(String),
    If(Box<Expr>, BlockStatement, Option<BlockStatement>), // condition, consequence, alternative
    Infix(Box<Expr>, Token, Box<Expr>), // left expression, operator, right expression
    Integer(i64),
    Prefix(Token, Box<Expr>),
    Str(&'static str),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Function(parameters, body) => {
                write!(f, "fn({}) {body}", vec_to_str(&parameters))
            }
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::If(condition, consequence, alternative) => match alternative {
                Some(alt) => write!(f, "if {condition} {consequence} else {alt}"),
                None => write!(f, "if {condition} {consequence}"),
            },
            Self::Infix(left, op, right) => write!(f, "({left} {op} {right})"),
            Self::Integer(int) => write!(f, "{int}"),
            Self::Prefix(token, expr) => write!(f, "({token}{expr})"),
            Self::Str(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub(crate) statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new() -> Self {
        Self { statements: vec![] }
    }
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}

fn vec_to_str<T: fmt::Display>(slice: &[T]) -> String {
    slice
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmt() {
        let program = Program {
            statements: vec![Statement::Let("myVar".to_string(), Expr::Str("anotherVar"))],
        };

        let expected = "let myVar = anotherVar;";
        let actual = format!("{}", program.statements.get(0).unwrap());

        assert_eq!(actual, expected);
    }
}
