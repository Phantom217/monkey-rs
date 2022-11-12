use crate::{
    ast::{Expr, Program, Statement},
    object::{self, Object},
    token::Token,
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
        Expr::Boolean(b) => {
            if *b {
                Ok(object::TRUE)
            } else {
                Ok(object::FALSE)
            }
        }
        Expr::Prefix(operator, expr) => {
            let right = eval_expression(expr)?;
            eval_prefix_expression(operator, right)
        }
        _ => unimplemented!(),
    }
}

fn eval_prefix_expression(operator: &Token, right: Object) -> Result<Object> {
    match operator {
        Token::Bang => match right {
            object::TRUE => Ok(object::FALSE),
            object::FALSE => Ok(object::TRUE),
            object::NULL => Ok(object::TRUE),
            _ => Ok(object::FALSE),
        },
        Token::Minus => match right {
            Object::Integer(int) => Ok(Object::Integer(-int)),
            _ => Ok(object::NULL),
        },
        _ => Ok(object::NULL),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! run_tests {
        ( $tests:ident ) => {
            for (input, expected) in $tests {
                let program = Program::new(input);
                let actual = eval(program);

                assert_eq!(actual.unwrap(), expected);
            }
        };
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
        ];

        run_tests!(tests);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![("true", object::TRUE), ("false", object::FALSE)];

        run_tests!(tests);
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", object::FALSE),
            ("!false", object::TRUE),
            ("!5", object::FALSE),
            ("!!true", object::TRUE),
            ("!!false", object::FALSE),
            ("!!5", object::TRUE),
        ];

        run_tests!(tests);
    }
}
