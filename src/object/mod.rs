pub mod builtin;
pub mod environment;

use std::fmt;

use crate::{
    ast::{vec_to_str, BlockStatement, Expr},
    evaluator,
};

use self::environment::MutEnv;

type BuiltinFunction = fn(Vec<Object>) -> evaluator::Result<Object>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Boolean(bool),
    Builtin(String, BuiltinFunction),
    Function(Vec<Expr>, BlockStatement, MutEnv),
    Integer(i64),
    Null,
    Return(Box<Object>),
    String(String),
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

impl Object {
    pub fn error_display(&self) -> String {
        match self {
            Self::Boolean(_) => "BOOLEAN",
            Self::Builtin(..) => "BUILTIN",
            Self::Function(_, _, _) => "FUNCTION",
            Self::Integer(_) => "INTEGER",
            Self::Null => "NULL",
            Self::Return(_) => "RETURN",
            Self::String(_) => "STRING",
        }
        .to_string()
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Builtin(func, _) => write!(f, "{func}"),
            Self::Function(params, body, _) => write!(
                f,
                "fn({params}) {{\n {body}\n}}",
                params = vec_to_str(params)
            ),
            Self::Integer(int) => write!(f, "{int}"),
            Self::Null => write!(f, "null"),
            Self::Return(object) => write!(f, "return {object}"),
            Self::String(string) => write!(f, "{string}"),
        }
    }
}
