use crate::ast::{self, Statement};
use crate::lexer::{self, Token};
use std::mem::discriminant;

pub struct Parser {
    l: lexer::Lexer,
    cur_token: lexer::Token,
    peek_token: lexer::Token,
}

impl Parser {
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        //self.peek_token = self.l.next_token().unwrap();
        if let Some(next) = self.l.next_token() {
            self.peek_token = next;
        } else {
            println!("ga gada next token");
        }
    }

    fn parse_program(&mut self) -> Option<Box<ast::Program>> {
        let mut program = ast::Program {
            statements: Vec::new(),
        };

        while self.cur_token != lexer::Token::EOF {
            println!("cur token {:?}", self.cur_token);
            if let Some(stmt) = self.parse_statement() {
                println!("push ke statemtn");
                program.statements.push(stmt);
            }
            self.next_token();
        }

        return Some(Box::new(program));
    }

    fn parse_statement(&mut self) -> Option<Box<Statement>> {
        match self.cur_token {
            lexer::Token::LET => {
                println!("tedeteksi parse let statement");
                return self.parse_let_statement();
            }
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<Statement>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::IDENT(String::new())) {
            println!("expect peek bukan ident");
            return None;
        }

        let ident = ast::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal(),
        };

        if !self.expect_peek(Token::ASSIGN) {
            println!("expect peek bukan ident");
            return None;
        }

        while !self.cur_token_is(lexer::Token::SEMICOLON) {
            self.next_token();
        }

        let expr = ast::Expression {};

        let stmt = ast::Statement::Let {
            token: token,
            name: Box::new(ident),
            value: Box::new(expr),
        };

        Some(Box::new(stmt))
    }

    fn cur_token_is(&self, t: Token) -> bool {
        discriminant(&self.cur_token) == discriminant(&t)
    }

    fn peek_token_is(&self, t: Token) -> bool {
        discriminant(&self.peek_token) == discriminant(&t)
    }

    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            return true;
        } else {
            return false;
        }
    }
}

fn new(lex: lexer::Lexer) -> Parser {
    let mut p = Parser {
        l: lex,
        cur_token: lexer::Token::ILLEGAL,
        peek_token: lexer::Token::ILLEGAL,
    };

    p.next_token();
    p.next_token();

    return p;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
            "#
        .to_string();

        let l = lexer::Lexer::new(input);
        let mut p = new(l);

        if let Some(program) = p.parse_program() {
            let dprogram = *program;
            assert_eq!(3, dprogram.statements.len());

            let cases = vec!["x", "y", "foobar"];

            for (i, input) in cases.iter().enumerate() {
                if let Some(stmt) = dprogram.statements.get(i) {
                    let dstmt: &ast::Statement = &*stmt;
                    let val = test_let_statement(dstmt, input.to_string());
                    if !val {
                        panic!("ss");
                    }
                }
            }
        } else {
            panic!("failed");
        }
    }

    fn test_let_statement(s: &ast::Statement, namex: String) -> bool {
        match s {
            Statement::Let { token, name, value } => {
                let dname: &ast::Identifier = &*name;
                assert_eq!(dname.value.to_string(), namex);
                assert_eq!(dname.token.literal(), namex);
            }
            _ => {
                println!("s not ast::LetStatement");
                return false;
            }
        }
        true
    }
}
