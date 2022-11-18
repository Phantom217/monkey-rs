use crate::{
    evaluator::{EvalError, Result},
    object,
};

use super::Object;

/// Runner for builtin functions
pub fn get(ident: &str) -> Option<Object> {
    match ident {
        "len" => Some(Object::Builtin(ident.to_string(), len)),
        "first" => Some(Object::Builtin(ident.to_string(), first)),
        "last" => Some(Object::Builtin(ident.to_string(), last)),
        "rest" => Some(Object::Builtin(ident.to_string(), rest)),
        "push" => Some(Object::Builtin(ident.to_string(), push)),
        "puts" => Some(Object::Builtin(ident.to_string(), puts)),
        _ => None,
    }
}

macro_rules! assert_num_args {
    ( $name:expr, $args:ident, $expected:expr ) => {
        if $args.len() != $expected {
            return Err(EvalError::WrongNumberOfArguments(
                $name,
                $expected,
                $args.len(),
            ));
        }
    };
}

/// Get the length of an array or string
fn len(args: Vec<Object>) -> Result<Object> {
    const NAME: &str = "len";
    assert_num_args!(NAME, args, 1);

    match args.get(0).unwrap() {
        Object::Array(xs) => Ok(Object::Integer(xs.len() as i64)),
        Object::String(s) => Ok(Object::Integer(s.len() as i64)),
        err => Err(EvalError::UnsupportedArgument(NAME, err.error_display())),
    }
}

/// Get the first element of an array
fn first(args: Vec<Object>) -> Result<Object> {
    const NAME: &str = "first";
    assert_num_args!(NAME, args, 1);

    let Some(Object::Array(xs)) = args.get(0) else {
        return Err(EvalError::UnsupportedArgument(NAME, args.get(0).unwrap().error_display()));
    };

    if xs.is_empty() {
        return Ok(object::NULL);
    }

    Ok(xs.get(0).unwrap().clone())
}

/// Get the last element of an array
fn last(args: Vec<Object>) -> Result<Object> {
    const NAME: &str = "last";
    assert_num_args!(NAME, args, 1);

    let Some(Object::Array(xs)) = args.get(0) else {
        return Err(EvalError::UnsupportedArgument(NAME, args.get(0).unwrap().error_display()));
    };

    if xs.is_empty() {
        return Ok(object::NULL);
    }

    Ok(xs.last().unwrap().clone())
}

/// Get the contents of an array minus the first element
fn rest(args: Vec<Object>) -> Result<Object> {
    const NAME: &str = "rest";
    assert_num_args!(NAME, args, 1);

    let Some(Object::Array(xs)) = args.get(0) else {
        return Err(EvalError::UnsupportedArgument(NAME, args.get(0).unwrap().error_display()));
    };

    if xs.is_empty() {
        return Ok(object::NULL);
    }

    Ok(Object::Array(xs.clone().drain(1..).collect()))
}

/// Adds a new element to the end of an array, allocating a new array and returning the result
fn push(args: Vec<Object>) -> Result<Object> {
    const NAME: &str = "push";
    assert_num_args!(NAME, args, 2);

    let Some(Object::Array(xs)) = args.get(0) else {
        return Err(EvalError::UnsupportedArgument(NAME, args.get(0).unwrap().error_display()));
    };

    let ys = {
        let mut ys = xs.clone();
        let elem = args.get(1).unwrap().clone();
        ys.push(elem);
        ys
    };

    Ok(Object::Array(ys))
}

/// Prints args to stdout
fn puts(args: Vec<Object>) -> Result<Object> {
    for arg in &args {
        println!("{arg}");
    }

    Ok(object::NULL)
}
