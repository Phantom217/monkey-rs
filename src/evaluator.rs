use std::{fmt, rc::Rc};

use crate::{
    ast::{BlockStatement, Expr, Program, Statement},
    object::{
        self,
        environment::{Environment, MutEnv},
        Object,
    },
    token::Token,
};

pub type Result<T> = std::result::Result<T, EvalError>;

// TODO: possibly add function location
#[derive(Debug, PartialEq)]
pub enum EvalError {
    IdentifierNotFound(String),
    TypeMismatch(String),
    UnknownOperator(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IdentifierNotFound(s) => write!(f, "identifier not found: {s}"),
            Self::TypeMismatch(s) => write!(f, "type mismatch: {s}"),
            Self::UnknownOperator(s) => write!(f, "unknown operator: {s}"),
        }
    }
}

macro_rules! eval_boolean {
    ( $expr:expr ) => {
        if $expr {
            Ok(object::TRUE)
        } else {
            Ok(object::FALSE)
        }
    };
}

pub fn eval(program: Program, env: MutEnv) -> Result<Object> {
    let mut result = object::NULL;

    for statement in program.statements.iter() {
        result = eval_statement(&statement, Rc::clone(&env))?;

        if let Object::Return(expr) = result {
            result = *expr;
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: MutEnv) -> Result<Object> {
    let mut result = object::NULL;

    for statement in block.statements.iter() {
        result = eval_statement(&statement, Rc::clone(&env))?;

        if let Object::Return(_) = result {
            break;
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement, env: MutEnv) -> Result<Object> {
    match statement {
        Statement::Return(expr) => Ok(Object::Return(Box::new(eval_expression(expr, env)?))),
        Statement::Expression(expr) => eval_expression(expr, env),
        Statement::Let(ident, expr) => {
            let object = eval_expression(expr, Rc::clone(&env))?;
            env.borrow_mut().set(ident, object);
            Ok(object::NULL)
        }
    }
}

fn eval_expression(expr: &Expr, env: MutEnv) -> Result<Object> {
    match expr {
        Expr::Integer(int) => Ok(Object::Integer(*int)),
        Expr::Boolean(b) => eval_boolean!(*b),
        Expr::Prefix(operator, expr) => {
            let right = eval_expression(expr, env)?;
            eval_prefix_expression(operator, right)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(left, Rc::clone(&env))?;
            let right = eval_expression(right, Rc::clone(&env))?;
            eval_infix_expression(operator, left, right)
        }
        Expr::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expr::Identifier(ident) => eval_identifier(ident, env),
        _ => todo!(),
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
            _ => Err(EvalError::UnknownOperator(format!(
                "{operator}{}",
                right.error_display()
            ))),
        },
        _ => Err(EvalError::UnknownOperator(format!(
            "{operator}{}",
            right.error_display()
        ))),
    }
}

fn eval_infix_expression(operator: &Token, left: Object, right: Object) -> Result<Object> {
    use Object::*;

    match (&left, &right) {
        (Integer(l), Integer(r)) => eval_integer_infix_expression(&operator, *l, *r),
        (Boolean(l), Boolean(r)) => eval_boolean_infix_expression(&operator, *l, *r),
        _ => Err(EvalError::TypeMismatch(format!(
            "{} {} {}",
            left.error_display(),
            operator,
            right.error_display()
        ))),
    }
}

fn eval_integer_infix_expression(operator: &Token, left: i64, right: i64) -> Result<Object> {
    match operator {
        Token::Plus => Ok(Object::Integer(left + right)),
        Token::Minus => Ok(Object::Integer(left - right)),
        Token::Asterisk => Ok(Object::Integer(left * right)),
        Token::Slash => Ok(Object::Integer(left / right)),
        Token::Lt => eval_boolean!(left < right),
        Token::Gt => eval_boolean!(left > right),
        Token::Eq => eval_boolean!(left == right),
        Token::NotEq => eval_boolean!(left != right),
        _ => Err(EvalError::UnknownOperator(format!("{operator}"))),
    }
}

fn eval_boolean_infix_expression(operator: &Token, left: bool, right: bool) -> Result<Object> {
    match operator {
        Token::Eq => eval_boolean!(left == right),
        Token::NotEq => eval_boolean!(left != right),
        _ => Err(EvalError::UnknownOperator(format!(
            "{b} {operator} {b}",
            b = object::TRUE.error_display()
        ))),
    }
}

fn eval_if_expression(
    condition: &Expr,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
    env: MutEnv,
) -> Result<Object> {
    let condition = eval_expression(condition, Rc::clone(&env))?;
    if is_truthy(condition) {
        eval_block_statement(consequence, Rc::clone(&env))
    } else {
        match alternative {
            Some(alt) => eval_block_statement(alt, env),
            None => Ok(object::NULL),
        }
    }
}

fn eval_identifier(ident: &str, env: MutEnv) -> Result<Object> {
    match env.borrow().get(ident) {
        Some(val) => Ok(val.clone()),
        None => Err(EvalError::IdentifierNotFound(ident.to_string())),
    }
}

fn is_truthy(condition: Object) -> bool {
    match condition {
        object::NULL => false,
        object::TRUE => true,
        object::FALSE => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! run_tests {
        ( $tests:ident => $unwrapper:ident) => {
            for (input, expected) in $tests {
                let program = Program::new(input);
                let env = Environment::new();
                let actual = eval(program, env).$unwrapper();

                assert_eq!(actual, expected);
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
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", object::TRUE),
            ("false", object::FALSE),
            ("1 < 2", object::TRUE),
            ("1 > 2", object::FALSE),
            ("1 < 1", object::FALSE),
            ("1 > 1", object::FALSE),
            ("1 == 1", object::TRUE),
            ("1 != 1", object::FALSE),
            ("1 == 2", object::FALSE),
            ("1 != 2", object::TRUE),
            ("true == true", object::TRUE),
            ("false == false", object::TRUE),
            ("true == false", object::FALSE),
            ("true != false", object::TRUE),
            ("false != true", object::TRUE),
            ("(1 < 2) == true", object::TRUE),
            ("(1 < 2) == false", object::FALSE),
            ("(1 > 2) == true", object::FALSE),
            ("(1 > 2) == false", object::TRUE),
        ];

        run_tests!(tests => unwrap);
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

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", object::NULL),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", object::NULL),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_return_statement() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            ("if (10 > 1) { return 10; }", Object::Integer(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; }  return 1; } ",
                Object::Integer(10),
            ),
            // ("let f = fn(x) { return x; x + 10; }; f(10);", Object::Integer(10)),
            // ("let f = fn(x) { let result = x + 10; return result; return 10; }; f(10);", Object::Integer(20)),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                EvalError::TypeMismatch("INTEGER + BOOLEAN".to_string()),
            ),
            (
                "5 + true; 5;",
                EvalError::TypeMismatch("INTEGER + BOOLEAN".to_string()),
            ),
            ("-true", EvalError::UnknownOperator("-BOOLEAN".to_string())),
            (
                "true + false;",
                EvalError::UnknownOperator("BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "true + false + true + false;",
                EvalError::UnknownOperator("BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "5; true + false; 5",
                EvalError::UnknownOperator("BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "if (10 > 1) { true + false; }",
                EvalError::UnknownOperator("BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return true + false;
  }

  return 1;
}
",
                EvalError::UnknownOperator("BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "foobar",
                EvalError::IdentifierNotFound("foobar".to_string()),
            ),
        ];

        run_tests!(tests => unwrap_err);
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        run_tests!(tests => unwrap);
    }
}
