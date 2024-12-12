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

        let lexer = Lexer::new(input.to_string());
        let mut parser = diamond_core::parser::Parser::new(lexer);
        let program = parser.parse_program();

        if parser.errors().is_empty() {
            println!("{}", program.to_string());
        } else {
            println!("parser has {} errors:", parser.errors().len());
            for msg in parser.errors().into_iter() {
                println!("\tparser error: {}", msg);
            }
        }
    }
}
