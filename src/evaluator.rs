use std::{fmt, rc::Rc};

use crate::{
    ast::{BlockStatement, Expr, Program, Statement},
    object::{
        self, builtin,
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
    IndexOutOfBounds(i64),
    TypeMismatch(String),
    UnknownIndexOperator(String),
    UnknownOperator(String),
    UnsupportedArgument(&'static str, String),
    WrongNumberOfArguments(&'static str, usize, usize),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IdentifierNotFound(s) => write!(f, "identifier not found: {s}"),
            Self::IndexOutOfBounds(i) => write!(f, "index out of bounds: {i}"),
            Self::TypeMismatch(s) => write!(f, "type mismatch: {s}"),
            Self::UnknownIndexOperator(s) => write!(f, "index operator not supported: {s}"),
            Self::UnknownOperator(s) => write!(f, "unknown operator: {s}"),
            Self::UnsupportedArgument(func, arg) => {
                write!(f, "argument to {func} not supported, got {arg}",)
            }
            Self::WrongNumberOfArguments(name, expected, got) => {
                write!(
                    f,
                    "wrong number of arguments to {name}: expected {expected} argument, got {got}"
                )
            }
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

pub fn eval(program: &Program, env: &MutEnv) -> Result<Object> {
    let mut result = object::NULL;

    for statement in &program.statements {
        result = eval_statement(statement, Rc::clone(env))?;

        if let Object::Return(expr) = result {
            result = *expr;
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement, env: &MutEnv) -> Result<Object> {
    let mut result = object::NULL;

    for statement in &block.statements {
        result = eval_statement(statement, Rc::clone(env))?;

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
        Expr::String(string) => Ok(Object::String(string.to_string())),
        Expr::Array(xs) => {
            let xs = eval_expressions(xs, &Rc::clone(&env))?;
            Ok(Object::Array(xs))
        }
        Expr::Index(left, idx) => eval_index_expression(left, idx, &Rc::clone(&env)),
        Expr::Prefix(operator, expr) => {
            let right = eval_expression(expr, env)?;
            eval_prefix_expression(operator, &right)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(left, Rc::clone(&env))?;
            let right = eval_expression(right, Rc::clone(&env))?;
            eval_infix_expression(operator, &left, &right)
        }
        Expr::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, &env)
        }
        Expr::Identifier(ident) => eval_identifier(ident, &env),
        Expr::Function(params, body) => Ok(Object::Function(
            params.clone(),
            body.clone(),
            Rc::clone(&env),
        )),
        Expr::Call(func, args) => {
            let func = eval_expression(func, Rc::clone(&env))?;
            let args = eval_expressions(args, &Rc::clone(&env))?;
            apply_function(func, args)
        }
    }
}

fn eval_prefix_expression(operator: &Token, right: &Object) -> Result<Object> {
    match operator {
        Token::Bang => match *right {
            object::FALSE | object::NULL => Ok(object::TRUE),
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

fn eval_infix_expression(operator: &Token, left: &Object, right: &Object) -> Result<Object> {
    use Object::{Boolean, Integer, String};

    match (&left, &right) {
        (Integer(l), Integer(r)) => eval_integer_infix_expression(operator, *l, *r),
        (Boolean(l), Boolean(r)) => eval_boolean_infix_expression(operator, *l, *r),
        (String(l), String(r)) => eval_string_infix_expression(operator, l, r),
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

fn eval_string_infix_expression(operator: &Token, left: &str, right: &str) -> Result<Object> {
    match operator {
        Token::Plus => {
            let s = {
                let mut s = String::with_capacity(left.len() + right.len());
                s.push_str(left);
                s.push_str(right);
                s
            };

            Ok(Object::String(s))
        }
        Token::Eq => eval_boolean!(left == right),
        Token::NotEq => eval_boolean!(left != right),
        _ => Err(EvalError::UnknownOperator(format!(
            "STRING {operator} STRING"
        ))),
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
    env: &MutEnv,
) -> Result<Object> {
    let condition = eval_expression(condition, Rc::clone(env))?;
    if is_truthy(&condition) {
        eval_block_statement(consequence, &Rc::clone(env))
    } else {
        match alternative {
            Some(alt) => eval_block_statement(alt, env),
            None => Ok(object::NULL),
        }
    }
}

fn eval_identifier(ident: &str, env: &MutEnv) -> Result<Object> {
    if let Some(val) = env.borrow().get(ident) {
        return Ok(val);
    }

    if let Some(builtin) = builtin::get(ident) {
        return Ok(builtin);
    }

    return Err(EvalError::IdentifierNotFound(ident.to_string()));
}

fn eval_expressions(exprs: &[Expr], env: &MutEnv) -> Result<Vec<Object>> {
    let mut result = vec![];

    for expr in exprs.iter() {
        result.push(eval_expression(expr, Rc::clone(env))?);
    }

    Ok(result)
}

fn eval_index_expression(left: &Expr, idx: &Expr, env: &MutEnv) -> Result<Object> {
    let left = eval_expression(left, Rc::clone(&env))?;
    let idx = eval_expression(idx, Rc::clone(&env))?;

    let (Object::Array(xs), &Object::Integer(idx)) = (left, &idx) else {
        return Err(EvalError::UnknownIndexOperator(idx.error_display()));
    };

    let Some(object) = xs.get(idx as usize) else {
        return Ok(object::NULL);
    };

    Ok(object.clone())
}

fn apply_function(function: Object, args: Vec<Object>) -> Result<Object> {
    match function {
        Object::Function(params, body, env) => {
            let extended_env = extend_function_environment(&params, args, env)?;
            match eval_block_statement(&body, &extended_env)? {
                Object::Return(result) => Ok(*result),
                object => Ok(object),
            }
        }
        Object::Builtin(_, func) => func(args),
        _ => Err(EvalError::TypeMismatch(format!(
            "{} is not a function",
            function.error_display()
        ))),
    }
}

fn extend_function_environment(params: &[Expr], args: Vec<Object>, env: MutEnv) -> Result<MutEnv> {
    let extended_env = Environment::new_enclosed(env);

    for (param, arg) in params.iter().zip(args.into_iter()) {
        let Expr::Identifier(ident) = param else {
            return Err(EvalError::IdentifierNotFound(param.to_string()))
        };
        extended_env.borrow_mut().set(ident, arg);
    }

    Ok(extended_env)
}

fn is_truthy(condition: &Object) -> bool {
    !matches!(*condition, object::NULL | object::FALSE)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! run_tests {
        ( $tests:ident => $unwrapper:ident) => {
            for (input, expected) in $tests {
                let env = object::environment::Environment::new();
                let program = Program::new(input);
                let actual = eval(&program, &env).$unwrapper();

                assert_eq!(actual, expected, "{input}");
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
            (
                "let f = fn(x) { return x; x + 10; }; f(10);",
                Object::Integer(10),
            ),
            (
                "let f = fn(x) { let result = x + 10; return result; return 10; }; f(10);",
                Object::Integer(20),
            ),
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
            (
                r#""Hello" - "World""#,
                EvalError::UnknownOperator("STRING - STRING".to_string()),
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

    #[test]
    fn test_function_object() {
        let tests = vec![(
            "fn(x) { x + 2; };",
            Object::Function(
                vec![Expr::Identifier("x".to_string())],
                BlockStatement {
                    statements: vec![Statement::Expression(Expr::Infix(
                        Box::new(Expr::Identifier("x".to_string())),
                        Token::Plus,
                        Box::new(Expr::Integer(2)),
                    ))],
                },
                object::environment::Environment::new(),
            ),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_enclosed_environment() {
        let tests = vec![(
            "
let first = 10;
let second = 10;
let third = 10;

let ourFunction = fn(first) {
  let second = 20;

  first + second + third;
};

ourFunction(20) + first + second;
",
            Object::Integer(70),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_closures() {
        let tests = vec![(
            "
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
",
            Object::Integer(4),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_string_literal() {
        let tests = vec![(
            "\"Hello World!\"",
            Object::String("Hello World!".to_string()),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_string_concatenation() {
        let tests = vec![(
            r#""Hello" + " " + "World!""#,
            Object::String("Hello World!".to_string()),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_string_comparison() {
        let tests = vec![
            (r#""Hello" == "Hello""#, object::TRUE),
            (r#""Hello" == "World""#, object::FALSE),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_builtin_passing() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            ("len([1, 2, 3])", Object::Integer(3)),
            ("len([])", Object::Integer(0)),
            // (r#"puts("hello", "world!")"#, object::NULL),
            ("first([1, 2, 3])", Object::Integer(1)),
            ("first([])", object::NULL),
            ("last([1, 2, 3])", Object::Integer(3)),
            ("last([])", object::NULL),
            (
                "rest([1, 2, 3])",
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            ("rest([])", object::NULL),
            // ("push([], 1)", vec![Object::Integer(1)]),
        ];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_builtin_errors() {
        let tests = vec![
            (
                "len(0)",
                EvalError::UnsupportedArgument("len", "INTEGER".to_string()),
            ),
            (
                r#"len("one", "two")"#,
                EvalError::WrongNumberOfArguments("len", 1, 2),
            ),
            (
                "first(1)",
                EvalError::UnsupportedArgument("first", "INTEGER".to_string()),
            ),
            (
                "last(1)",
                EvalError::UnsupportedArgument("last", "INTEGER".to_string()),
            ),
            // ( "push(1)", EvalError::UnsupportedArgument("push", "INTEGER".to_string()), ),
        ];

        run_tests!(tests => unwrap_err);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![(
            "[1, 2 * 2, 3 + 3]",
            Object::Array(vec![
                Object::Integer(1),
                Object::Integer(4),
                Object::Integer(6),
            ]),
        )];

        run_tests!(tests => unwrap);
    }

    #[test]
    fn test_array_index_expressions() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", object::NULL),
            ("[1, 2, 3][-1]", object::NULL),
        ];

        run_tests!(tests => unwrap);
    }
}
