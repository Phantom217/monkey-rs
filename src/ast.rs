use std::fmt;

#[derive(Debug)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, std::marker::PhantomData<Expr>), // Let(Identifier, Value)
    Return(std::marker::PhantomData<Expr>),      // Return(Value)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(s, e) => write!(f, "let {s} = {e:?}"),
            Self::Return(e) => write!(f, "return {e:?};"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Str(&'static str),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(s) => write!(f, "{s}"),
        }
    }
}
