use crate::ast::{self, Statement};
use crate::lexer::{self, Token};
use std::collections::HashMap;
use std::mem::discriminant;

type PrefixParseFn = fn(&mut Parser) -> ast::Expression;
type InfixParseFn = fn(ast::Expression) -> ast::Expression;

pub enum Precendence {
    Lo,
    Eq,
    Lg,
    Sum,
    Product,
    Prefix,
    Call,
}
pub struct Parser {
    l: lexer::Lexer,
    cur_token: lexer::Token,
    peek_token: lexer::Token,

    prefix_parse_fns: HashMap<Token, PrefixParseFn>,
    infix_parse_fns: HashMap<Token, InfixParseFn>,
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
                return self.parse_let_statement();
            }
            lexer::Token::RETURN => {
                return self.parse_return_statement();
            }
            _ => return self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Box<ast::Statement>> {
        let token = self.cur_token.clone();

        if let Some(expr) = self.parse_expression(Precendence::Lo as i8) {
            let mut stmt = ast::Statement::ExpressionStatement {
                token: token,
                value: Box::new(expr),
            };

            if self.peek_token_is(Token::SEMICOLON) {
                self.next_token();
            }

            return Some(Box::new(stmt));
        } else {
            return None;
        }
    }

    fn parse_expression(&mut self, precedence: i8) -> Option<ast::Expression> {
        if let Some(prefix) = self.prefix_parse_fns.get(&self.cur_token) {
            let left_exp = prefix(self);
            return Some(left_exp);
        } else {
            return None;
        }
    }

    fn parse_let_statement(&mut self) -> Option<Box<Statement>> {
        let token = self.cur_token.clone();

        if !self.expect_peek(Token::IDENT(String::new())) {
            println!("expect peek bukan ident");
            return None;
        }

        let ident = ast::Expression::Identifier {
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

        let expr = ast::Expression::Integer(0);

        let stmt = ast::Statement::Let {
            token: token,
            name: Box::new(ident),
            value: Box::new(expr),
        };

        Some(Box::new(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Box<Statement>> {
        let token = self.cur_token.clone();
        let expr = ast::Expression::Integer(0);

        self.next_token();

        while !self.cur_token_is(lexer::Token::SEMICOLON) {
            self.next_token();
        }

        let stmt = ast::Statement::Return {
            token: token,
            value: Box::new(expr),
        };

        Some(Box::new(stmt))
    }

    fn parse_indentifier(&mut self) -> ast::Expression {
        ast::Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal(),
        }
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

    fn register_prefix(&mut self, t: Token, fun: PrefixParseFn) {
        //unwrap
        self.prefix_parse_fns.insert(t, fun);
    }

    fn register_infix(&mut self, t: Token, fun: InfixParseFn) {
        //unwrap
        self.infix_parse_fns.insert(t, fun);
    }
}

fn new(lex: lexer::Lexer) -> Parser {
    let mut p = Parser {
        l: lex,
        cur_token: lexer::Token::ILLEGAL,
        peek_token: lexer::Token::ILLEGAL,
        prefix_parse_fns: HashMap::new(),
        infix_parse_fns: HashMap::new(),
    };

    p.register_prefix(Token::IDENT("".to_string()), Parser::parse_indentifier);

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
                let dname: &ast::Expression = &*name;
                match dname {
                    ast::Expression::Identifier { token, value } => {
                        assert_eq!(value.to_string(), namex);
                        assert_eq!(token.literal(), namex);
                    }
                    _ => {
                        panic!("not an ast Expression Identifier");
                    }
                }
            }
            _ => {
                println!("s not ast::LetStatement");
                return false;
            }
        }
        true
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
            "#
        .to_string();

        let l = lexer::Lexer::new(input);
        let mut p = new(l);

        if let Some(program) = p.parse_program() {
            let dprogram = *program;
            assert_eq!(3, dprogram.statements.len());

            for i in 0..3 {
                if let Some(stmt) = dprogram.statements.get(i) {
                    let dstmt: &ast::Statement = &*stmt;
                    match dstmt {
                        Statement::Return { token, value } => {
                            assert_eq!(*token, Token::RETURN);
                        }
                        _ => {
                            return panic!("not return");
                        }
                    }
                }
            }
        } else {
            panic!("failed");
        }
    }

    #[test]
    fn test_indentifier_expression() {
        let input = r#"
        foobar;
        "#
        .to_string();

        let l = lexer::Lexer::new(input);
        let mut p = new(l);

        if let Some(program) = p.parse_program() {
            let dprogram = *program;
            assert_eq!(1, dprogram.statements.len());

            if let Some(stmt) = dprogram.statements.get(0) {
                let dstmt: &ast::Statement = &*stmt;
                match dstmt {
                    Statement::ExpressionStatement { token, value } => {
                        let val: &ast::Expression = value;
                        match val {
                            ast::Expression::Identifier { token, value } => {
                                assert_eq!(value.to_string(), "foobar".to_string());
                            }
                            _ => {
                                panic!("not an ast Identifier");
                            }
                        }
                    }
                    _ => {
                        return panic!("not return");
                    }
                }
            }
        } else {
            panic!("failed");
        }
    }
}
