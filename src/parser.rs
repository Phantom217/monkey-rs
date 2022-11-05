use crate::{
    ast::{Program, Statement},
    lexer::Lexer,
    token::Token,
};

type Result<T> = std::result::Result<T, ParserError>;

#[derive(Debug)]
enum ParserError {
    ExpectedIdent(Token),
    ExpectedAssign(Token),
    ExpectedToken { expected: Token, got: Token },
    Unimplemented(Token),
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
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
            // TODO: remove when finished implementing Parser
            token => Err(ParserError::Unimplemented(token.clone())),
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

        // TODO: We're skipping the expressions until we encounter a semicolon
        // cur_token: Assign, peek_token: beginning of expression
        self.next_token();
        while self.peek_token != Token::Semicolon {
            self.next_token();
        }

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let(identifier, ()))
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
mod tests {
    use crate::lexer::Lexer;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r"
let x = 5;
let y = 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();

        let expected = vec![
            Statement::Let("x".to_string(), ()),
            Statement::Let("y".to_string(), ()),
            Statement::Let("foobar".to_string(), ()),
        ];

        assert_eq!(program.statements, expected);
    }
}
