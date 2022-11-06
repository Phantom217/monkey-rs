use std::fmt;

#[derive(Debug)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
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
            Self::Return(e) => write!(f, "return {e:?};"),
            Self::Expression(e) => write!(f, "{e:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Str(&'static str),
    Identifier(String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(ident) => write!(f, "{ident}"),
            Self::Str(s) => write!(f, "{s}"),
        }
    }
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
