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
            let mut l = lexer::Lexer::new(input.clone());
            while let Some(tok) = l.next_token() {
                println!("[Type:{:?} Literal:{}]", tok, tok.literal());
            }
        }
        input.clear();
    }
}
