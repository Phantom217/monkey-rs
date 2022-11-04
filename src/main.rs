use std::error::Error;

use monkey_rs::repl;

fn main() -> Result<(), Box<dyn Error>> {
    let user = std::env::var("USER")?;

    println!("Hello {user}! This is the Monkey programming language!");
    println!("Feel free to type in commands");

    repl::start()?;

    Ok(())
}
