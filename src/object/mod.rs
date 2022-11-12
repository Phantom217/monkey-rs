use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Null,
}

pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(bool) => write!(f, "{bool}"),
            Self::Integer(int) => write!(f, "{int}"),
            Self::Null => write!(f, "null"),
        }
    }
}
