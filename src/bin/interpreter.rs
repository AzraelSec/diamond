use std::{env, fs};

use diamond_core::{
    evaluator::{self, environment::Environment, object::Object},
    lexer::Lexer,
    parser::{node::Node, Parser},
};

fn main() {
    let args = env::args().collect::<Vec<String>>();
    if args.len() < 2 {
        eprintln!("Usage: diamond [file]");
        return;
    }

    let executable_file_name = args.get(1).unwrap();
    let executable_code = fs::read_to_string(executable_file_name);

    let executable_code = match executable_code {
        Ok(code) => code,
        Err(_) => {
            eprintln!("Could not read file {}", executable_file_name);
            return;
        }
    };

    let mut env = Environment::new();

    let lexer = Lexer::new(executable_code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors().is_empty() {
        eprintln!("parser has {} errors:", parser.errors().len());
        for msg in parser.errors().into_iter() {
            eprintln!("\tparser error: {}", msg);
        }
        return;
    }

    let evaluated = evaluator::eval(Node::Program(program), &mut env);
    if let Object::Error(err) = evaluated {
        eprintln!("evaluator error: {}", err);
    }
}
