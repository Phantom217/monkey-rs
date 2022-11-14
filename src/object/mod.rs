pub mod environment;

use std::fmt;

use crate::ast::{vec_to_str, BlockStatement, Expr};

use self::environment::MutEnv;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Boolean(bool),
    Function(Vec<Expr>, BlockStatement, MutEnv),
    Integer(i64),
    Null,
    Return(Box<Object>),
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

impl Object {
    pub fn error_display(&self) -> String {
        match self {
            Self::Boolean(_) => format!("BOOLEAN"),
            Self::Function(_, _, _) => format!("FUNCTION"),
            Self::Integer(_) => format!("INTEGER"),
            Self::Null => format!("NULL"),
            Self::Return(_) => format!("RETURN"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Function(params, body, _) => write!(
                f,
                "fn({params}) {{\n {body}\n}}",
                params = vec_to_str(params)
            ),
            Self::Integer(int) => write!(f, "{int}"),
            Self::Null => write!(f, "null"),
            Self::Return(object) => write!(f, "return {object}"),
        }
    }
}
