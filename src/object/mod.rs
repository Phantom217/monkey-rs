use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
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
            Self::Integer(int) => write!(f, "{int}"),
            Self::Null => write!(f, "null"),
            Self::Return(object) => write!(f, "return {object}"),
        }
    }
}
