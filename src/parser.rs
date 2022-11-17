use std::{collections::BTreeMap, fmt};

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
    ExpectedColon(Token),
    ExpectedComma(Token),
    ExpectedIdent(Token),
    ExpectedLBrace(Token),
    ExpectedLParen(Token),
    ExpectedPrefixToken(Token),
    ExpectedRBrace(Token),
    ExpectedRBracket(Token),
    ExpectedRParen(Token),
    ExpectedToken { expected: Token, got: Token },
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (expected, got) = match self {
            Self::ExpectedAssign(token) => ("=".to_string(), format!("{token}")),
            Self::ExpectedColon(token) => (":".to_string(), format!("{token}")),
            Self::ExpectedComma(token) => (",".to_string(), format!("{token}")),
            Self::ExpectedIdent(token) => ("ident".to_string(), format!("{token}")),
            Self::ExpectedLBrace(token) => ("{".to_string(), format!("{token}")),
            Self::ExpectedLParen(token) => ("(".to_string(), format!("{token}")),
            Self::ExpectedPrefixToken(token) => ("a prefix".to_string(), format!("{token}")),
            Self::ExpectedRBrace(token) => ("}".to_string(), format!("{token}")),
            Self::ExpectedRBracket(token) => ("]".to_string(), format!("{token}")),
            Self::ExpectedRParen(token) => (")".to_string(), format!("{token}")),
            Self::ExpectedToken { expected, got } => (format!("{expected}"), format!("{got}")),
        };

        write!(f, "expected next token to be {expected}, got {got} instead")
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // >, <
    Sum,         // +
    Product,     // *
    Prefix,      // -X, !X
    Call,        // my_function(X)
    Index,       // array[index]
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::LBracket => Self::Index,
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

    fn expect_peek<F>(&mut self, token: &Token, parser_error: F) -> Result<()>
    where
        F: Fn(Token) -> ParserError,
    {
        if self.peek_token == *token {
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
        let Token::Ident(ident) = &self.peek_token else {
            return Err(ParserError::ExpectedIdent(self.peek_token.clone()));
        };
        let identifier = ident.clone();
        self.next_token();

        // cur_token: Ident, peek_token: Assign
        self.expect_peek(&Token::Assign, ParserError::ExpectedAssign)?;

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
                Precedence::Index => {
                    self.next_token();
                    self.parse_index_expression(Box::new(left_expr))?
                }
                Precedence::Prefix | Precedence::Lowest => break,
            }
        }

        Ok(left_expr)
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        match &self.cur_token {
            Token::Ident(ident) => Ok(Expr::Identifier(ident.clone())),
            Token::Int(int) => Ok(Expr::Integer(*int)),
            Token::String(string) => Ok(Expr::String(string.clone())),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True => Ok(Expr::Boolean(true)),
            Token::False => Ok(Expr::Boolean(false)),
            Token::LBracket => self.parse_array_literal(),
            Token::LParen => self.parse_grouped_expression(),
            Token::LBrace => self.parse_hash_literal(),
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

        self.expect_peek(&Token::RParen, ParserError::ExpectedRParen)?;

        Ok(exp)
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        // cur_token: If, peek_token: LParen
        self.expect_peek(&Token::LParen, ParserError::ExpectedLParen)?;
        self.next_token(); // consume LParen to get expression

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(&Token::RParen, ParserError::ExpectedRParen)?;
        self.expect_peek(&Token::LBrace, ParserError::ExpectedLBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            self.expect_peek(&Token::LBrace, ParserError::ExpectedLBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expr::If(condition, consequence, alternative))
    }

    fn parse_function_literal(&mut self) -> Result<Expr> {
        self.expect_peek(&Token::LParen, ParserError::ExpectedLParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(&Token::LBrace, ParserError::ExpectedLBrace)?;

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

        self.expect_peek(&Token::RParen, ParserError::ExpectedRParen)?;

        Ok(parameters)
    }

    fn parse_index_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        self.next_token();
        let idx = self.parse_expression(Precedence::Lowest)?;

        self.expect_peek(&Token::RBracket, ParserError::ExpectedRBracket)?;

        Ok(Expr::Index(left, Box::new(idx)))
    }

    fn parse_array_literal(&mut self) -> Result<Expr> {
        let xs = self.parse_expression_list(Token::RBracket)?;
        Ok(Expr::Array(xs))
    }

    fn parse_hash_literal(&mut self) -> Result<Expr> {
        let mut hash = BTreeMap::new();

        while self.peek_token != Token::RBrace {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;

            self.expect_peek(&Token::Colon, ParserError::ExpectedColon)?;

            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;

            hash.insert(key, value);

            if self.peek_token == Token::RBrace {
                break;
            }

            self.expect_peek(&Token::Comma, ParserError::ExpectedComma)?;
        }

        self.expect_peek(&Token::RBrace, ParserError::ExpectedRBrace)?;

        Ok(Expr::Hash(hash))
    }

    fn parse_call_expression(&mut self, function: Box<Expr>) -> Result<Expr> {
        let args = self.parse_expression_list(Token::RParen)?;
        Ok(Expr::Call(function, args))
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<Expr>> {
        let mut list = vec![];

        self.next_token();
        if self.cur_token != end {
            list.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
                list.push(self.parse_expression(Precedence::Lowest)?);
            }

            self.expect_peek(&end, |got| ParserError::ExpectedToken {
                expected: end.clone(),
                got,
            })?;
        }

        Ok(list)
    }

    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }

        eprintln!("Parser has {} errors:", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            eprintln!("\t{i}. {error:?}");
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

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
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
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in &tests {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();

            assert_eq!(program.to_string(), expected.to_string());
        }
    }
}

#[cfg(test)]
mod test_statements {
    use super::*;

    macro_rules! run_tests {
        ( $input:expr => $expected:expr) => {
            let lexer = Lexer::new($input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();

            assert_eq!(program.statements, $expected);
        };
    }

    #[test]
    fn test_let_statements() {
        let input = r"
let x = 5;
let y = true;
let foobar = y;
";
        let expected = vec![
            Statement::Let("x".to_string(), Expr::Integer(5)),
            Statement::Let("y".to_string(), Expr::Boolean(true)),
            Statement::Let("foobar".to_string(), Expr::Identifier("y".to_string())),
        ];

        run_tests!(input => expected);
    }

    #[test]
    fn test_return_statements() {
        let input = r"
return 5;
return true;
return foobar;
";

        let expected = vec![
            Statement::Return(Expr::Integer(5)),
            Statement::Return(Expr::Boolean(true)),
            Statement::Return(Expr::Identifier("foobar".to_string())),
        ];

        run_tests!(input => expected);
    }
}

#[cfg(test)]
mod test_expressions {
    use super::*;

    macro_rules! run_tests {
        ( $input:expr => $expected:expr) => {
            let lexer = Lexer::new($input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();

            assert_eq!(program.statements, $expected);
        };
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let expected = vec![Statement::Expression(Expr::Identifier(
            "foobar".to_string(),
        ))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let expected = vec![Statement::Expression(Expr::Integer(5))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_prefix_expression() {
        let input = r"
!5;
-15;
!foobar;
-foobar;
!true;
!false;
";

        let expected = vec![
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Integer(5)))),
            Statement::Expression(Expr::Prefix(Token::Minus, Box::new(Expr::Integer(15)))),
            Statement::Expression(Expr::Prefix(
                Token::Bang,
                Box::new(Expr::Identifier("foobar".to_string())),
            )),
            Statement::Expression(Expr::Prefix(
                Token::Minus,
                Box::new(Expr::Identifier("foobar".to_string())),
            )),
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(true)))),
            Statement::Expression(Expr::Prefix(Token::Bang, Box::new(Expr::Boolean(false)))),
        ];

        run_tests!(input => expected);
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
foobar + barfoo;
foobar - barfoo;
foobar * barfoo;
foobar / barfoo;
foobar > barfoo;
foobar < barfoo;
foobar == barfoo;
foobar != barfoo;
true == true;
true != false;
false == false;
";

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
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Plus,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Minus,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Asterisk,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Slash,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Gt,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Lt,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::Eq,
                Box::new(Expr::Identifier("barfoo".to_string())),
            )),
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Identifier("foobar".to_string())),
                Token::NotEq,
                Box::new(Expr::Identifier("barfoo".to_string())),
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
            Statement::Expression(Expr::Infix(
                Box::new(Expr::Boolean(false)),
                Token::Eq,
                Box::new(Expr::Boolean(false)),
            )),
        ];

        run_tests!(input => expected);
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
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();

            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_boolean() {
        let input = r"
true;
false;
let foobar = true;
";

        let expected = vec![
            Statement::Expression(Expr::Boolean(true)),
            Statement::Expression(Expr::Boolean(false)),
            Statement::Let("foobar".to_string(), Expr::Boolean(true)),
        ];

        run_tests!(input => expected);
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let input = r"
fn() {};
fn(x) {};
fn(x, y, z) {};
";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        let input = r"
add();
add(1);
add(1, 2 * 3, 4 + 5);
";

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

        run_tests!(input => expected);
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world";"#;

        let expected = vec![Statement::Expression(Expr::String(
            "hello world".to_string(),
        ))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_empty_array_literals() {
        let input = "[]";

        let expected = vec![Statement::Expression(Expr::Array(vec![]))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3];";

        let expected = vec![Statement::Expression(Expr::Array(vec![
            Expr::Integer(1),
            Expr::Infix(
                Box::new(Expr::Integer(2)),
                Token::Asterisk,
                Box::new(Expr::Integer(2)),
            ),
            Expr::Infix(
                Box::new(Expr::Integer(3)),
                Token::Plus,
                Box::new(Expr::Integer(3)),
            ),
        ]))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_index_expressions() {
        let input = "myArray[1 + 1]";

        let expected = vec![Statement::Expression(Expr::Index(
            Box::new(Expr::Identifier("myArray".to_string())),
            Box::new(Expr::Infix(
                Box::new(Expr::Integer(1)),
                Token::Plus,
                Box::new(Expr::Integer(1)),
            )),
        ))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_empty_hash_literal() {
        let input = "{}";

        let expected = vec![Statement::Expression(Expr::Hash(BTreeMap::new()))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_hash_literal_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let mut map = BTreeMap::new();
        map.insert(Expr::String("one".to_string()), Expr::Integer(1));
        map.insert(Expr::String("two".to_string()), Expr::Integer(2));
        map.insert(Expr::String("three".to_string()), Expr::Integer(3));

        let expected = vec![Statement::Expression(Expr::Hash(map))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_hash_literal_boolean_keys() {
        let input = "{true: 1, false: 2}";

        let mut map = BTreeMap::new();
        map.insert(Expr::Boolean(true), Expr::Integer(1));
        map.insert(Expr::Boolean(false), Expr::Integer(2));

        let expected = vec![Statement::Expression(Expr::Hash(map))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_hash_literal_integer_keys() {
        let input = "{1: 1, 2: 2, 3: 3}";

        let mut map = BTreeMap::new();
        map.insert(Expr::Integer(1), Expr::Integer(1));
        map.insert(Expr::Integer(2), Expr::Integer(2));
        map.insert(Expr::Integer(3), Expr::Integer(3));

        let expected = vec![Statement::Expression(Expr::Hash(map))];

        run_tests!(input => expected);
    }

    #[test]
    fn test_parse_hash_literal_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let mut map = BTreeMap::new();
        map.insert(
            Expr::String("one".to_string()),
            Expr::Infix(
                Box::new(Expr::Integer(0)),
                Token::Plus,
                Box::new(Expr::Integer(1)),
            ),
        );
        map.insert(
            Expr::String("two".to_string()),
            Expr::Infix(
                Box::new(Expr::Integer(10)),
                Token::Minus,
                Box::new(Expr::Integer(8)),
            ),
        );
        map.insert(
            Expr::String("three".to_string()),
            Expr::Infix(
                Box::new(Expr::Integer(15)),
                Token::Slash,
                Box::new(Expr::Integer(5)),
            ),
        );

        let expected = vec![Statement::Expression(Expr::Hash(map))];

        run_tests!(input => expected);
    }
}
