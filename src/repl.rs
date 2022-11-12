use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{
    evaluator,
    lexer::Lexer,
    object::{self, environment},
    parser::Parser,
};

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
    let env = environment::Environment::new();
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

        let evaluated = evaluator::eval(program, Rc::clone(&env));
        match evaluated {
            Ok(object) if object == object::NULL => continue,
            Ok(object) => println!("{object}"),
            Err(_) => todo!(),
        }
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
