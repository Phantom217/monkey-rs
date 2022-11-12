use crate::{
    ast::{Expr, Program, Statement},
    object::Object,
};

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq)]
pub enum EvalError {}

pub fn eval(program: Program) -> Result<Object> {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval_statement(&statement)?;
    }

    Ok(result)
}

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Let(ident, expr) => todo!(),
        Statement::Return(expr) => todo!(),
        Statement::Expression(expr) => eval_expression(expr),
    }
}

fn eval_expression(expr: &Expr) -> Result<Object> {
    match expr {
        Expr::Integer(int) => Ok(Object::Integer(*int)),
        Expr::Boolean(b) => Ok(Object::Boolean(*b)),
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![("5", Object::Integer(5)), ("10", Object::Integer(10))];

        for (input, expected) in tests {
            let program = Program::new(input);
            let actual = eval(program);

            assert_eq!(actual.unwrap(), expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
        ];

        for (input, expected) in tests {
            let program = Program::new(input);
            let actual = eval(program);

            assert_eq!(actual.unwrap(), expected);
        }
    }
}
