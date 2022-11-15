use crate::evaluator::{EvalError, Result};

use super::Object;

pub fn get(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::Builtin(ident.to_string(), len)),
        _ => None,
    }
}

fn len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::WrongNumberOfArguments(args.len()));
    };

    let Some(Object::String(s)) = args.get(0) else {
        return Err(EvalError::UnsupportedArgument("len", args.get(0).unwrap().error_display()));
    };

    Ok(Object::Integer(s.len() as i64))
}
