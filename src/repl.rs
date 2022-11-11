use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start() -> io::Result<()> {
    loop {
        print!("{PROMPT}");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.errors.len() != 0 {
            print_parser_errors(&parser);
            continue;
        }

        println!("{program}");
    }
}

fn print_parser_errors(parser: &Parser) {
    println!("{MONKEY_FACE}");
    println!("Whoops! We ran into some monkey business here!");
    println!(" parser errors:");

    for error in parser.errors.iter() {
        println!("\t{error}");
    }
}
