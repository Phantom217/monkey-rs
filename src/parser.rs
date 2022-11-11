use std::fmt;

use crate::{
    ast::{BlockStatement, Expr, Program, Statement},
    lexer::Lexer,
    token::Token,
};

type Result<T> = std::result::Result<T, ParserError>;

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) enum ParserError {
    ExpectedAssign(Token),
    ExpectedIdent(Token),
    ExpectedLBrace(Token),
    ExpectedLParen(Token),
    ExpectedPrefixToken(Token),
    ExpectedRParen(Token),
    ExpectedToken { expected: Token, got: Token },
    Unimplemented(Token),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ParserError::*;

        let (expected, got) = match self {
            ExpectedAssign(token) => ("=".to_string(), format!("{token}")),
            ExpectedIdent(token) => ("ident".to_string(), format!("{token}")),
            ExpectedLBrace(token) => ("{".to_string(), format!("{token}")),
            ExpectedLParen(token) => ("(".to_string(), format!("{token}")),
            ExpectedPrefixToken(token) => ("a prefix".to_string(), format!("{token}")),
            ExpectedRParen(token) => (")".to_string(), format!("{token}")),
            ExpectedToken { expected, got } => (format!("{expected}"), format!("{got}")),
            Unimplemented(token) => panic!("Unimplemented Token: {token}"),
        };

        write!(f, "expected next token to be {expected}, got {got} instead")
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // >, <
    Sum,         // +
    Product,     // *
    Prefix,      // -X, !X
    Call,        // my_function(X)
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::LParen => Self::Call,
            Token::Asterisk | Token::Slash => Self::Product,
            Token::Plus | Token::Minus => Self::Sum,
            Token::Lt | Token::Gt => Self::LessGreater,
            Token::Eq | Token::NotEq => Self::Equals,
            _ => Self::Lowest,
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub(crate) errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        // read two tokens so both cur_token and peek_token are set
        parser.next_token();
        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.cur_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    fn expect_peek<F>(&mut self, token: Token, parser_error: F) -> Result<()>
    where
        F: Fn(Token) -> ParserError,
    {
        if self.peek_token == token {
            self.next_token();
            Ok(())
        } else {
            Err(parser_error(self.peek_token.clone()))
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match &self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        // cur_token: Let, peek_token: Ident
        let identifier: String;
        if let Token::Ident(ident) = &self.peek_token {
            identifier = ident.clone();
            self.next_token();
        } else {
            return Err(ParserError::ExpectedIdent(self.peek_token.clone()));
        };

        // cur_token: Ident, peek_token: Assign
        self.expect_peek(Token::Assign, ParserError::ExpectedAssign)?;

        // cur_token: Assign, peek_token: beginning of expression
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(identifier, value))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStatement> {
        let mut block = BlockStatement::new();
        self.next_token();

        while self.cur_token != Token::RBrace && self.cur_token != Token::Eof {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Ok(block)
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return(return_value))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let stmt = Statement::Expression(self.parse_expression(Precedence::Lowest)?);

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr> {
        let mut left_expr = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && precedence < Precedence::from(&self.peek_token)
        {
            left_expr = match Precedence::from(&self.peek_token) {
                Precedence::Equals
                | Precedence::LessGreater
                | Precedence::Sum
                | Precedence::Product => {
                    self.next_token();
                    self.parse_infix_expression(Box::new(left_expr))?
                }
                Precedence::Call => {
                    self.next_token();
                    self.parse_call_expression(Box::new(left_expr))?
                }
                _ => todo!(),
            }
        }

        Ok(left_expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(Expr::Identifier(ident.clone())),
            Token::Int(int) => Ok(Expr::Integer(*int)),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True => Ok(Expr::Boolean(true)),
            Token::False => Ok(Expr::Boolean(false)),
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            token => Err(ParserError::ExpectedPrefixToken(token.clone())),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expr> {
        let cur_token = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Precedence::Prefix)?;

        Ok(Expr::Prefix(cur_token, Box::new(right)))
    }

    fn parse_infix_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        let token = self.cur_token.clone();
        let right = {
            let precedence = Precedence::from(&token);
            self.next_token();
            let expr = self.parse_expression(precedence)?;
            Box::new(expr)
        };

        Ok(Expr::Infix(left, token, right))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expr> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        // cur_token: If, peek_token: LParen
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;
        self.next_token(); // consume LParen to get expression

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;
        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expr::If(condition, consequence, alternative))
    }

    fn parse_function_literal(&mut self) -> Result<Expr> {
        self.expect_peek(Token::LParen, ParserError::ExpectedLParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(Token::LBrace, ParserError::ExpectedLBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expr::Function(parameters, body))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expr>> {
        let mut parameters = vec![];

        self.next_token();
        if self.cur_token == Token::RParen {
            return Ok(parameters);
        }

        let ident = match &self.cur_token {
            Token::Ident(i) => Expr::Identifier(i.clone()),
            t => return Err(ParserError::ExpectedIdent(t.clone())),
        };
        parameters.push(ident);

        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            let ident = match &self.cur_token {
                Token::Ident(i) => Expr::Identifier(i.clone()),
                t => return Err(ParserError::ExpectedIdent(t.clone())),
            };
            parameters.push(ident);
        }

        self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;

        Ok(parameters)
    }

    fn parse_call_expression(&mut self, function: Box<Expr>) -> Result<Expr> {
        let args = self.parse_call_arguments()?;
        Ok(Expr::Call(function, args))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expr>> {
        let mut args = vec![];

        self.next_token();
        if self.cur_token != Token::RParen {
            args.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
                args.push(self.parse_expression(Precedence::Lowest)?);
            }

            self.expect_peek(Token::RParen, ParserError::ExpectedRParen)?;
        }

        Ok(args)
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            eprintln!("\t{i}. {error:?}")
        }
    }
}

#[cfg(test)]
mod test_precedence {
    use super::*;

    #[test]
    fn test_ord() {
        assert!(Precedence::Lowest < Precedence::Equals);
        assert!(Precedence::Equals > Precedence::Lowest);
    }
}

#[cfg(test)]
mod test_statements {
    use super::*;

    fn init_test(input: &str) -> (Parser, Program) {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        (parser, program)
    }

    #[test]
    fn test_let_statements() {
        let input = r"
let x = 5;
let y = true;
let foobar = y;
";
        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Let("x".to_string(), Expr::Integer(5)),
            Statement::Let("y".to_string(), Expr::Boolean(true)),
            Statement::Let("foobar".to_string(), Expr::Identifier("y".to_string())),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_return_statements() {
        let input = r"
return 5;
return true;
return foobar;
";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Return(Expr::Integer(5)),
            Statement::Return(Expr::Boolean(true)),
            Statement::Return(Expr::Identifier("foobar".to_string())),
        ];

        assert_eq!(program.statements, expected);
    }
}

#[cfg(test)]
mod test_expressions {
    use super::*;

    fn init_test(input: &str) -> (Parser, Program) {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        (parser, program)
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::Identifier(
            "foobar".to_string(),
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::Integer(5))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_parse_prefix_expression() {
        //         let input = r"
        // !5;
        // -15;
        // !foobar;
        // -foobar;
        // !true;
        // !false;
        // ";
        let input = r"
!5;
-15;
!true;
!false;
";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Integer(5)))),
            Statement::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Integer(15)))),
            // Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Str("foobar")))),
            // Statement::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Str("foobar")))),
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(true)))),
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(false)))),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_infix_expression() {
        let input = r"
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;
true == true;
true != false;
";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Plus,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Minus,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Asterisk,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Slash,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Gt,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Lt,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::Eq,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Integer(5)),
                Token::NotEq,
                Box::new(Expr::Integer(5)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::Eq,
                Box::new(Expr::Boolean(true)),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Boolean(true)),
                Token::NotEq,
                Box::new(Expr::Boolean(false)),
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in tests {
            let (parser, program) = init_test(input);
            parser.check_parser_errors();

            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_boolean() {
        let input = r"
true;
false;
";
        //         let input = r"
        // true;
        // false;
        // let foobar = true;
        // ";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Expression(Expr::Boolean(true)),
            Statement::Expression(Expr::Boolean(false)),
            // Statement::Let("foobar".to_string(), Expr::Boolean(true)),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStatement {
                statements: vec![Statement::Expression(Expr::Identifier("x".to_string()))],
            },
            None,
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::If(
            Box::new(Expr::Infix(
                Box::new(Expr::Identifier("x".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("y".to_string())),
            )),
            BlockStatement {
                statements: vec![Statement::Expression(Expr::Identifier("x".to_string()))],
            },
            Some(BlockStatement {
                statements: vec![Statement::Expression(Expr::Identifier("y".to_string()))],
            }),
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::Function(
            vec![
                Expr::Identifier("x".to_string()),
                Expr::Identifier("y".to_string()),
            ],
            BlockStatement {
                statements: vec![Statement::Expression(Expr::Infix(
                    Box::new(Expr::Identifier("x".to_string())),
                    Token::Plus,
                    Box::new(Expr::Identifier("y".to_string())),
                ))],
            },
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let input = r"
fn() {};
fn(x) {};
fn(x, y, z) {};
";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Expression(Expr::Function(
                vec![],
                BlockStatement { statements: vec![] },
            )),
            Statement::Expression(Expr::Function(
                vec![Expr::Identifier("x".to_string())],
                BlockStatement { statements: vec![] },
            )),
            Statement::Expression(Expr::Function(
                vec![
                    Expr::Identifier("x".to_string()),
                    Expr::Identifier("y".to_string()),
                    Expr::Identifier("z".to_string()),
                ],
                BlockStatement { statements: vec![] },
            )),
        ];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![Statement::Expression(Expr::Call(
            Box::new(Expr::Identifier("add".to_string())),
            vec![
                Expr::Integer(1),
                Expr::Infix(
                    Box::new(Expr::Integer(2)),
                    Token::Asterisk,
                    Box::new(Expr::Integer(3)),
                ),
                Expr::Infix(
                    Box::new(Expr::Integer(4)),
                    Token::Plus,
                    Box::new(Expr::Integer(5)),
                ),
            ],
        ))];

        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        let input = r"
add();
add(1);
add(1, 2 * 3, 4 + 5);
";

        let (parser, program) = init_test(input);
        parser.check_parser_errors();

        let expected = vec![
            Statement::Expression(Expr::Call(
                Box::new(Expr::Identifier("add".to_string())),
                vec![],
            )),
            Statement::Expression(Expr::Call(
                Box::new(Expr::Identifier("add".to_string())),
                vec![Expr::Integer(1)],
            )),
            Statement::Expression(Expr::Call(
                Box::new(Expr::Identifier("add".to_string())),
                vec![
                    Expr::Integer(1),
                    Expr::Infix(
                        Box::new(Expr::Integer(2)),
                        Token::Asterisk,
                        Box::new(Expr::Integer(3)),
                    ),
                    Expr::Infix(
                        Box::new(Expr::Integer(4)),
                        Token::Plus,
                        Box::new(Expr::Integer(5)),
                    ),
                ],
            )),
        ];

        assert_eq!(program.statements, expected);
    }
}
