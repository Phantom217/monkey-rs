use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer {
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
    pub fn new(input: &str) -> Self {
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

    fn peek_char(&self) -> Option<&char> {
        self.input.get(self.read_position)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::Eq
                }
                _ => Token::Assign,
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Token::NotEq
                }
                _ => Token::Bang,
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '"' => {
                let string = self.read_string();
                Token::String(string)
            }
            '\0' => Token::Eof,
            c if c.is_letter() => {
                let ident = self.read_identifier();
                return lookup_identifier(ident);
            }
            c if c.is_ascii_digit() => {
                let int = self.read_number();
                return Token::Int(int);
            }
            c => Token::Illegal(c),
        };

        self.read_char();
        token
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while self.ch.is_letter() {
            self.read_char();
        }
        self.input[start..self.position].iter().collect()
    }

    fn read_number(&mut self) -> i64 {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[start..self.position]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }

    fn read_string(&mut self) -> String {
        let start = self.position + 1;
        loop {
            self.read_char();

            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        self.input[start..self.position].iter().collect()
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

fn lookup_identifier(ident: String) -> Token {
    match ident.as_str() {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(ident),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! input_produces_tokens {
        ( $input:ident => $tokens:ident ) => {
            let lexer = Lexer::new($input);

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

    #[test]
    fn test_token_lexer_extension() {
        let input = r"let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";
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
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }

    #[test]
    fn test_lexer_string_token() {
        let input = r#"
"foobar"
"foo bar"
"#;

        let tokens = vec![
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }

    #[ignore = "not yet implemented"]
    #[test]
    fn test_string_character_escaping() {
        let input = r#"
"hello \"world\""
"hello\n world"
"hello\t\t\tworld"
"#;
        let tokens = vec![
            Token::String("hello \"world\"".to_string()),
            Token::String("hello\n world".to_string()),
            Token::String("hello\t\t\tworld".to_string()),
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }

    #[test]
    fn test_lexer_array() {
        let input = "[1, 2];";

        let tokens = vec![
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::Semicolon,
        ];

        input_produces_tokens!(input => tokens);
    }

    #[test]
    fn test_hash() {
        let input = r#"{"foo": "bar"}"#;

        let tokens = vec![
            Token::LBrace,
            Token::String("foo".to_string()),
            Token::Colon,
            Token::String("bar".to_string()),
            Token::RBrace,
            Token::Eof,
        ];

        input_produces_tokens!(input => tokens);
    }
}
