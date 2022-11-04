use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
struct Lexer {
    input: Vec<char>,
    // current position in input (points to current char)
    position: usize,
    // current reading position in input (after current char)
    read_position: usize,
    // current char under examination
    ch: char,
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }
}

impl Lexer {
    #[allow(dead_code)]
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    // TODO: support unicode characters
    fn read_char(&mut self) {
        self.ch = match self.input.get(self.read_position) {
            Some(&char) => char,
            None => '\0',
        };
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => Token::Assign,
            '+' => Token::Plus,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '\0' => Token::Eof,
            c if c.is_letter() => {
                let identifier = self.read_identifier();
                return self.lookup_identifier(identifier);
            }
            c if c.is_digit(10) => {
                let int = self.read_number();
                return Token::Int(int);
            }
            c => Token::Illegal(c),
        };

        self.read_char();
        token
    }

    fn lookup_identifier(&self, s: String) -> Token {
        match s.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Ident(s),
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while self.ch.is_letter() {
            self.read_char();
        }
        self.input[start..self.position].iter().collect()
    }

    fn read_number(&mut self) -> isize {
        let start = self.position;
        while self.ch.is_digit(10) {
            self.read_char();
        }
        self.input[start..self.position]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.position <= self.input.len() {
            Some(self.next_token())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::token::Token;
    use super::*;

    macro_rules! input_produces_tokens {
        ( $input:ident => $tokens:ident ) => {
            let lexer = Lexer::new($input.to_string());

            for (tok, expected) in lexer.zip($tokens.iter()) {
                assert_eq!(*expected, tok);
            }
        };
    }

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;";
        let tokens = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }

    #[test]
    fn test_next_token_subset() {
        let input = r"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);";
        let tokens = vec![
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }
}
