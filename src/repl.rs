use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let mut lexer = Lexer::new(input);

        loop {
            let token = lexer.next_token();
            if token == Token::Eof {
                break;
            }

            println!("{token:?}");
        }
    }
}
