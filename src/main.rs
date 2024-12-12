use std::io::{self, Write};

use diamond_core::lexer::Lexer;

fn main() {
    println!("Welcome to the Diamond REPL");

    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let input = input.trim();

        if input.eq_ignore_ascii_case("exit") {
            println!("Bye");
            break;
        }

        for token in Lexer::new(input.to_string()).token_iter() {
            println!("{:?}", token);
        }
    }
}
