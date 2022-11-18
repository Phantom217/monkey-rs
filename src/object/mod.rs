pub mod builtin;
pub mod environment;

use std::{
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
};

use crate::{
    ast::{vec_to_str, BlockStatement, Expr},
    evaluator,
};

use self::environment::MutEnv;

type BuiltinFunction = fn(Vec<Object>) -> evaluator::Result<Object>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Array(Vec<Object>),
    Boolean(bool),
    Builtin(String, BuiltinFunction),
    Function(Vec<Expr>, BlockStatement, MutEnv),
    Hash(HashMap<Object, Object>),
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
            Self::Array(..) => "ARRAY",
            Self::Boolean(..) => "BOOLEAN",
            Self::Builtin(..) => "BUILTIN",
            Self::Function(..) => "FUNCTION",
            Self::Hash(..) => "HASH",
            Self::Integer(..) => "INTEGER",
            Self::Null => "NULL",
            Self::Return(..) => "RETURN",
            Self::String(..) => "STRING",
        }
        .to_string()
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Array(xs) => write!(f, "[{}]", vec_to_str(xs)),
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Builtin(func, _) => write!(f, "{func}"),
            Self::Function(params, body, _) => write!(
                f,
                "fn({params}) {{\n {body}\n}}",
                params = vec_to_str(params)
            ),
            Self::Hash(map) => {
                let pairs: Vec<String> = map.iter().map(|(k, v)| format!("{k}: {v}")).collect();
                let pairs = vec_to_str(&pairs);
                write!(f, "{{{pairs}}}")
            }
            Self::Integer(int) => write!(f, "{int}"),
            Self::Null => write!(f, "null"),
            Self::Return(object) => write!(f, "return {object}"),
            Self::String(string) => write!(f, "{string}"),
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::String(s) => s.hash(state),
            _ => 0.hash(state),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn calculate_hash<T: Hash>(t: &T) -> u64 {
        let mut s = std::collections::hash_map::DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
    #[test]
    fn test_string_hash_key() {
        let hello1 = Object::String("Hello World".to_string());
        let hello2 = Object::String("Hello World".to_string());
        let diff1 = Object::String("Hello Steve".to_string());
        let diff2 = Object::String("Hello Steve".to_string());

        assert_eq!(calculate_hash(&hello1), calculate_hash(&hello2));
        assert_eq!(calculate_hash(&diff1), calculate_hash(&diff2));
        assert_ne!(calculate_hash(&hello1), calculate_hash(&diff1));
    }
}
