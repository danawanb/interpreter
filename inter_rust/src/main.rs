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

            if let Some(program) = p.parse_program() {
                if p.errors.len() != 0 {
                    print_parse_error(p.errors);
                    continue;
                }
                println!("{:?}", program.string());
            }
        }
        input.clear();
    }
}

const MONKEY: &str = r#"
               ',
            .-`-,\__
              ."`   `,
            .'_.  ._  `;.
        __ / `      `  `.\ .--.
       /--,| 0)   0)     )`_.-,)
      |    ;.-----.__ _-');   /
       '--./         `.`/  `"`
          :   '`      |.
          | \     /  //
           \ '---'  /'
            `------' \
          _/       `--...
                  __
"#;

fn print_parse_error(errors: Vec<String>) {
    println!("{}", MONKEY);
    println!("{}", "Woops! We ran into some monkey business here!");
    println!("{}", "parser errors:");
    for err in errors {
        println!("{:?}", err);
    }
}
