pub mod ast;
pub mod lexer;
pub mod parser;

use std::io::{Write, stdin, stdout};

fn main() {
    let mut input = String::new();
    println!("Hello this is the Monkey programming language!");

    loop {
        //todo handle unwrap
        stdout().write(">>".as_bytes()).unwrap();
        stdout().flush().unwrap();

        let readline = stdin().read_line(&mut input);
        if let Ok(_) = readline {
            let l = lexer::Lexer::new(input.clone());
            let mut p = parser::new(l);

            if p.errors.len() != 0 {
                println!("{:?}", p.errors);
                continue;
            }

            if let Some(program) = p.parse_program() {
                println!("{:?}", program.string());
            }
        }
        input.clear();
    }
}
