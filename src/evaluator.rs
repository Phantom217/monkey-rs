use crate::{
    ast::{BlockStatement, Expr, Program, Statement},
    object::{self, Object},
    token::Token,
};

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, PartialEq)]
pub enum EvalError {}

macro_rules! eval_boolean {
    ( $expr:expr ) => {
        if $expr {
            Ok(object::TRUE)
        } else {
            Ok(object::FALSE)
        }
    };
}

pub fn eval(program: Program) -> Result<Object> {
    let mut result = object::NULL;

    for statement in program.statements.iter() {
        result = eval_statement(&statement)?;

        if let Object::Return(expr) = result {
            result = *expr;
            break;
        }
    }

    Ok(result)
}

fn eval_block_statement(block: &BlockStatement) -> Result<Object> {
    let mut result = object::NULL;

    for statement in block.statements.iter() {
        result = eval_statement(&statement)?;

        if let Object::Return(_) = result {
            break;
        }
    }

    Ok(result)
}

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Let(ident, expr) => todo!(),
        Statement::Return(expr) => Ok(Object::Return(Box::new(eval_expression(expr)?))),
        Statement::Expression(expr) => eval_expression(expr),
    }
}

fn eval_expression(expr: &Expr) -> Result<Object> {
    match expr {
        Expr::Integer(int) => Ok(Object::Integer(*int)),
        Expr::Boolean(b) => eval_boolean!(*b),
        Expr::Prefix(operator, expr) => {
            let right = eval_expression(expr)?;
            eval_prefix_expression(operator, right)
        }
        Expr::Infix(left, operator, right) => {
            let left = eval_expression(left)?;
            let right = eval_expression(right)?;
            eval_infix_expression(operator, left, right)
        }
        Expr::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative)
        }
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
            _ => Ok(object::NULL),
        },
        _ => Ok(object::NULL),
    }
}

fn eval_infix_expression(operator: &Token, left: Object, right: Object) -> Result<Object> {
    use Object::*;

    match (left, right) {
        (Integer(l), Integer(r)) => eval_integer_infix_expression(&operator, l, r),
        (Boolean(l), Boolean(r)) => eval_boolean_infix_expression(&operator, l, r),
        _ => panic!(),
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
        _ => Ok(object::NULL),
    }
}

fn eval_boolean_infix_expression(operator: &Token, left: bool, right: bool) -> Result<Object> {
    match operator {
        Token::Eq => eval_boolean!(left == right),
        Token::NotEq => eval_boolean!(left != right),
        _ => Ok(object::NULL),
    }
}

fn eval_if_expression(
    condition: &Expr,
    consequence: &BlockStatement,
    alternative: &Option<BlockStatement>,
) -> Result<Object> {
    let condition = eval_expression(condition)?;
    if is_truthy(condition) {
        eval_block_statement(consequence)
    } else {
        match alternative {
            Some(alt) => eval_block_statement(alt),
            None => Ok(object::NULL),
        }
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

        run_tests!(tests);
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

        run_tests!(tests);
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

        run_tests!(tests);
    }
}
