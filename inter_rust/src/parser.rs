use crate::ast::{self, Expression, Statement};
use crate::lexer::{self, Token};
use std::any::Any;
use std::clone;
use std::collections::HashMap;
use std::mem::discriminant;

type PrefixParseFn = fn(&mut Parser) -> ast::Expression;
type InfixParseFn = fn(&mut Parser, ast::Expression) -> ast::Expression;

pub enum Precendence {
    Lo,
    Eq,
    Lg,
    Sum,
    Product,
    Prefix,
    Call,
}

fn get_precendence(t: Token) -> Option<Precendence> {
    match t {
        Token::EQ => Some(Precendence::Eq),
        Token::NOT_EQ => Some(Precendence::Eq),
        Token::LT => Some(Precendence::Lg),
        Token::GT => Some(Precendence::Lg),
        Token::PLUS => Some(Precendence::Sum),
        Token::MINUS => Some(Precendence::Sum),
        Token::SLASH => Some(Precendence::Product),
        Token::ASTERISK => Some(Precendence::Product),
        Token::LPAREN => Some(Precendence::Call),
        _ => None,
    }
}

#[derive(Clone)]
pub struct Parser {
    l: lexer::Lexer,
    cur_token: lexer::Token,
    peek_token: lexer::Token,
    errors: Vec<String>,
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
    fn parse_block_statement(&mut self) -> Option<Box<ast::Statement>> {
        let cur_token = self.cur_token.clone();
        self.next_token();

        let mut stmts: Vec<Box<ast::Statement>> = Vec::new();

        while !self.cur_token_is(Token::RBRACE) && !self.cur_token_is(Token::EOF) {
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            }
            self.next_token();
        }

        let block = ast::Statement::BlockStatement {
            token: cur_token,
            statements: stmts,
        };
        Some(Box::new(block))
    }

    fn parse_expression(&mut self, precedence: i8) -> Option<ast::Expression> {
        if let Some(prefix) = self.prefix_parse_fns.get(&self.cur_token) {
            let mut left_exp = prefix(self);

            while !self.peek_token_is(Token::SEMICOLON) && precedence < self.peek_precendence() {
                if let Some(infix) = self.infix_parse_fns.get(&self.peek_token).cloned() {
                    self.next_token();
                    left_exp = infix(self, left_exp);
                } else {
                    return Some(left_exp);
                }
            }
            return Some(left_exp);
        } else {
            self.no_prefix_parse_fn_error(self.cur_token.clone());
            return None;
        }
    }

    fn parse_prefix_expression(&mut self) -> ast::Expression {
        let curtoken = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Precendence::Prefix as i8);

        ast::Expression::Prefix {
            token: curtoken.clone(),
            operator: curtoken.literal(),
            right: Box::new(right.unwrap()),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> ast::Expression {
        let curtoken = self.cur_token.clone();
        let precend = self.cur_precendence();

        self.next_token();

        let right = self.parse_expression(precend);

        ast::Expression::Infix {
            token: curtoken.clone(),
            operator: curtoken.literal(),
            left: Box::new(left),
            right: Box::new(right.unwrap()),
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

        let expr = ast::Expression::IntegerLiteral {
            token: Token::INT(0),
            value: 0,
        };
        let stmt = ast::Statement::Let {
            token: token,
            name: Box::new(ident),
            value: Box::new(expr),
        };

        Some(Box::new(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Box<Statement>> {
        let token = self.cur_token.clone();
        let expr = ast::Expression::IntegerLiteral {
            token: Token::INT(0),
            value: 0,
        };

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

    fn parse_boolean(&mut self) -> ast::Expression {
        ast::Expression::Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(Token::TRUE),
        }
    }

    fn parse_grouped_expression(&mut self) -> ast::Expression {
        self.next_token();
        if let Some(exp) = self.parse_expression(Precendence::Lo as i8) {
            if !self.expect_peek(Token::RPAREN) {
                return ast::Expression::Nil;
            }
            return exp;
        } else {
            ast::Expression::Nil
        }
    }

    fn parse_integer_literal(&mut self) -> ast::Expression {
        if let Token::INT(val) = self.cur_token {
            return ast::Expression::IntegerLiteral {
                token: self.cur_token.clone(),
                value: val,
            };
        } else {
            return ast::Expression::IntegerLiteral {
                token: self.cur_token.clone(),
                value: 0,
            };
        }
    }
    fn parse_if_expression(&mut self) -> ast::Expression {
        let curtoken = self.cur_token.clone();

        if !self.expect_peek(Token::LPAREN) {
            return ast::Expression::Nil;
        }

        self.next_token();

        let cond = self.parse_expression(Precendence::Lo as i8);

        let mut alternative: Option<Box<Statement>> = None;
        let mut conseq: Box<Statement> = Box::new(ast::Statement::Nil);
        let mut condx = ast::Expression::Nil;

        if let Some(val_cond) = cond {
            condx = val_cond;
            if !self.expect_peek(Token::RPAREN) {
                return ast::Expression::Nil;
            }

            if !self.expect_peek(Token::LBRACE) {
                return ast::Expression::Nil;
            }

            if let Some(consequence) = self.parse_block_statement() {
                conseq = consequence;

                if self.peek_token_is(Token::ELSE) {
                    self.next_token();

                    if !self.expect_peek(Token::LBRACE) {
                        return ast::Expression::Nil;
                    }

                    if let Some(alt) = self.parse_block_statement() {
                        alternative = Some(alt);
                    }
                }
            }
        }

        return ast::Expression::IfExpression {
            token: curtoken,
            condition: Box::new(condx),
            consequence: conseq,
            alternative: alternative,
        };
    }

    fn parse_function_literal(&mut self) -> ast::Expression {
        let curtoken = self.cur_token.clone();

        if !self.expect_peek(Token::LPAREN) {
            return ast::Expression::Nil;
        }

        if let Some(param) = self.parse_function_parameters() {
            if !self.expect_peek(Token::LBRACE) {
                return ast::Expression::Nil;
            }

            if let Some(body) = self.parse_block_statement() {
                return ast::Expression::FunctionLiteral {
                    token: curtoken,
                    parameters: param,
                    body: body,
                };
            }
        }

        ast::Expression::Nil
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Box<Expression>>> {
        let mut identifiers: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();

        let ident = ast::Expression::Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal(),
        };
        identifiers.push(Box::new(ident));

        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();

            let ident = ast::Expression::Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal(),
            };
            identifiers.push(Box::new(ident));
        }

        if !self.expect_peek(Token::RPAREN) {
            return None;
        }

        Some(identifiers)
    }

    fn parse_call_expression(&mut self, func: ast::Expression) -> ast::Expression {
        let curtoken = self.cur_token.clone();
        if let Some(args) = self.parse_call_arguments() {
            return ast::Expression::CallExpression {
                token: curtoken,
                function: Box::new(func),
                arguments: args,
            };
        }

        ast::Expression::Nil
    }

    fn parse_call_arguments(&mut self) -> Option<Vec<Box<Expression>>> {
        let mut args: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(Token::RPAREN) {
            self.next_token();
            return Some(args);
        }

        self.next_token();
        if let Some(val_a) = self.parse_expression(Precendence::Lo as i8) {
            args.push(Box::new(val_a));
        }
        while self.peek_token_is(Token::COMMA) {
            self.next_token();
            self.next_token();

            if let Some(val_a) = self.parse_expression(Precendence::Lo as i8) {
                args.push(Box::new(val_a));
            }
        }

        if !self.expect_peek(Token::RPAREN) {
            return None;
        }

        Some(args)
    }

    fn no_prefix_parse_fn_error(&mut self, t: Token) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(msg);
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

    fn peek_precendence(&self) -> i8 {
        if let Some(val) = get_precendence(self.peek_token.clone()) {
            val as i8
        } else {
            Precendence::Lo as i8
        }
    }

    fn cur_precendence(&self) -> i8 {
        if let Some(val) = get_precendence(self.cur_token.clone()) {
            val as i8
        } else {
            Precendence::Lo as i8
        }
    }
}

fn new(lex: lexer::Lexer) -> Parser {
    let mut p = Parser {
        l: lex,
        cur_token: lexer::Token::ILLEGAL,
        peek_token: lexer::Token::ILLEGAL,
        errors: Vec::new(),
        prefix_parse_fns: HashMap::new(),
        infix_parse_fns: HashMap::new(),
    };

    //fishy
    p.register_prefix(Token::IDENT("".to_string()), Parser::parse_indentifier);
    p.register_prefix(Token::INT(0), Parser::parse_integer_literal);
    p.register_prefix(Token::BANG, Parser::parse_prefix_expression);
    p.register_prefix(Token::MINUS, Parser::parse_prefix_expression);
    p.register_prefix(Token::TRUE, Parser::parse_boolean);
    p.register_prefix(Token::FALSE, Parser::parse_boolean);
    p.register_prefix(Token::LPAREN, Parser::parse_grouped_expression);
    p.register_prefix(Token::IF, Parser::parse_if_expression);
    p.register_prefix(Token::FUNCTION, Parser::parse_function_literal);

    p.register_infix(Token::PLUS, Parser::parse_infix_expression);
    p.register_infix(Token::MINUS, Parser::parse_infix_expression);
    p.register_infix(Token::SLASH, Parser::parse_infix_expression);
    p.register_infix(Token::ASTERISK, Parser::parse_infix_expression);
    p.register_infix(Token::EQ, Parser::parse_infix_expression);
    p.register_infix(Token::NOT_EQ, Parser::parse_infix_expression);
    p.register_infix(Token::LT, Parser::parse_infix_expression);
    p.register_infix(Token::GT, Parser::parse_infix_expression);
    p.register_infix(Token::LPAREN, Parser::parse_call_expression);

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

    #[test]
    fn test_integer_literals() {
        let input = r#"
        5;
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
                            ast::Expression::IntegerLiteral { token, value } => {
                                assert_eq!(value.clone(), 5);
                            }
                            _ => {
                                panic!("not an ast Integer");
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

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = [("!5;", "!", 5), ("-15;", "-", 15)];

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
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
                                ast::Expression::Prefix {
                                    token,
                                    operator,
                                    right,
                                } => {
                                    assert_eq!(operator.to_string(), tt.1.to_string());
                                    assert_eq!(test_integer_literal(&right, tt.2 as i64), true);
                                }
                                _ => {
                                    panic!("not an ast Prefix");
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
        let prefix_tests_bool = [("!true;", "!", true), ("!false;", "!", false)];

        for tt in prefix_tests_bool {
            let l = lexer::Lexer::new(tt.0.to_string());
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
                                ast::Expression::Prefix {
                                    token,
                                    operator,
                                    right,
                                } => {
                                    assert_eq!(operator.to_string(), tt.1.to_string());
                                    assert_eq!(test_literal_expression(&right, &tt.2), true);
                                }
                                _ => {
                                    panic!("not an ast Prefix");
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

    fn test_integer_literal(il: &ast::Expression, valuex: i64) -> bool {
        match il {
            ast::Expression::IntegerLiteral { token, value } => {
                if *value != valuex {
                    return false;
                }

                let strval = format!("{}", value);
                if token.literal() != strval {
                    return false;
                }

                true
            }
            _ => false,
        }
    }

    fn test_identifier(exp: &ast::Expression, valuex: String) -> bool {
        match exp {
            ast::Expression::Identifier { token, value } => {
                if value.to_string() != valuex {
                    return false;
                }

                if token.literal() != valuex {
                    return false;
                }
                return true;
            }
            _ => return false,
        }
    }

    fn test_boolean_literal(exp: &ast::Expression, valuex: bool) -> bool {
        match exp {
            ast::Expression::Boolean { token, value } => {
                if value.to_owned() != valuex {
                    return false;
                }

                if token.literal() != valuex.to_string() {
                    return false;
                }
                return true;
            }
            _ => return false,
        }
    }

    fn test_literal_expression(exp: &ast::Expression, expected: &dyn Any) -> bool {
        if let Some(v) = expected.downcast_ref::<i64>() {
            return test_integer_literal(exp, v.to_owned());
        } else if let Some(v) = expected.downcast_ref::<i32>() {
            return test_integer_literal(exp, v.to_owned() as i64);
        } else if let Some(v) = expected.downcast_ref::<String>() {
            return test_identifier(exp, v.to_owned());
        } else if let Some(v) = expected.downcast_ref::<&str>() {
            return test_identifier(exp, v.to_string());
        } else if let Some(v) = expected.downcast_ref::<bool>() {
            return test_boolean_literal(exp, v.to_owned());
        }

        false
    }
    fn test_infix_expression(exp: &ast::Expression, l: &dyn Any, opr: String, r: &dyn Any) -> bool {
        match exp {
            ast::Expression::Infix {
                token,
                left,
                operator,
                right,
            } => {
                if !test_literal_expression(left, l) {
                    return false;
                }
                if operator.to_string() != opr {
                    return false;
                }
                if !test_literal_expression(right, r) {
                    return false;
                }
                return true;
            }

            _ => return false,
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let prefix_tests = [
            ("5 + 5", 5, "+", 5),
            ("5 - 5", 5, "-", 5),
            ("5 * 5", 5, "*", 5),
            ("5 / 5", 5, "/", 5),
            ("5 > 5", 5, ">", 5),
            ("5 < 5", 5, "<", 5),
            ("5 == 5", 5, "==", 5),
            //("5 != 5", 5, "!=", 5),
        ];

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
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
                                ast::Expression::Infix {
                                    token,
                                    left,
                                    operator,
                                    right,
                                } => {
                                    assert_eq!(true, test_integer_literal(&left, tt.1 as i64));
                                    assert_eq!(operator.to_string(), tt.2.to_string());
                                    assert_eq!(true, test_integer_literal(&right, tt.3 as i64));
                                }
                                _ => {
                                    panic!("not an ast Infix");
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
        let prefix_tests_boolean = [
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
            ("false == false", false, "==", false),
            //("5 != 5", 5, "!=", 5),
        ];

        for tt in prefix_tests_boolean {
            let l = lexer::Lexer::new(tt.0.to_string());
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
                                ast::Expression::Infix {
                                    token,
                                    left,
                                    operator,
                                    right,
                                } => {
                                    assert_eq!(true, test_literal_expression(&left, &tt.1));
                                    assert_eq!(operator.to_string(), tt.2.to_string());
                                    assert_eq!(true, test_literal_expression(&right, &tt.3));
                                }
                                _ => {
                                    panic!("not an ast Infix");
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
    #[test]
    fn test_operator_precendece_parsing() {
        let prefix_tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
            let mut p = new(l);

            if let Some(program) = p.parse_program() {
                let dprogram = *program;
                assert_eq!(dprogram.string(), tt.1);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = r#"
        if (x < y) { x }
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
                            ast::Expression::IfExpression {
                                token,
                                condition,
                                consequence,
                                alternative,
                            } => {
                                let left = String::from("x");
                                let right = String::from("y");
                                assert_eq!(
                                    test_infix_expression(
                                        condition,
                                        &left,
                                        "<".to_string(),
                                        &right
                                    ),
                                    true
                                );
                                let conse = &**consequence;
                                match conse {
                                    Statement::BlockStatement { token, statements } => {
                                        assert_eq!(statements.len(), 1);

                                        if let Some(consequenstmt) = statements.get(0) {
                                            let val = &**consequenstmt;
                                            match val {
                                                Statement::ExpressionStatement { token, value } => {
                                                    assert_eq!(
                                                        test_identifier(&*value, "x".to_string()),
                                                        true
                                                    );
                                                }
                                                _ => panic!("consequence not an expr stmt"),
                                            }
                                        } else {
                                            panic!("can't get consequence");
                                        }
                                    }
                                    _ => {}
                                }

                                if let Some(_) = alternative {
                                    panic!("alternative must be none");
                                }
                            }
                            _ => {
                                panic!("not an ast Integer");
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

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) {x + y;}".to_string();

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
                            ast::Expression::FunctionLiteral {
                                token,
                                parameters,
                                body,
                            } => {
                                assert_eq!(parameters.len(), 2);
                                assert_eq!(
                                    test_literal_expression(
                                        &parameters.get(0).unwrap(),
                                        &String::from("x")
                                    ),
                                    true
                                );
                                assert_eq!(
                                    test_literal_expression(
                                        &parameters.get(1).unwrap(),
                                        &String::from("y")
                                    ),
                                    true
                                );

                                let bodyx = &**body;

                                match bodyx {
                                    Statement::BlockStatement { token, statements } => {
                                        assert_eq!(statements.len(), 1);
                                        let first_stmt = &**statements.get(0).unwrap();

                                        match first_stmt {
                                            Statement::ExpressionStatement { token, value } => {}
                                            _ => panic!("not an expression statement"),
                                        }
                                    }
                                    _ => panic!("not a block stmt"),
                                }
                            }
                            _ => {
                                panic!("not an ast Expr Function Literal");
                            }
                        }
                    }
                    _ => {
                        panic!("not an ast Expr");
                    }
                }
            }
        } else {
            panic!("failed");
        }
    }

    #[test]
    fn test_functon_parameter_parsing() {
        let mut prefix_tests = Vec::new();
        prefix_tests.push(("fn() {};", vec![]));
        prefix_tests.push(("fn(x) {};", vec!["x"]));
        prefix_tests.push(("fn(x, y, z) {};", vec!["x", "y", "z"]));

        for tt in prefix_tests {
            let l = lexer::Lexer::new(tt.0.to_string());
            let mut p = new(l);

            if let Some(program) = p.parse_program() {
                let dprogram = *program;
                if let Some(stmt) = dprogram.statements.get(0) {
                    let dstmt: &ast::Statement = &*stmt;
                    match dstmt {
                        Statement::ExpressionStatement { token, value } => {
                            let val = &**value;
                            match val {
                                Expression::FunctionLiteral {
                                    token,
                                    parameters,
                                    body,
                                } => {
                                    assert_eq!(parameters.len(), tt.1.len());
                                    //todo compare parameters identfier value
                                }

                                _ => panic!("not a function literal"),
                            }
                        }
                        _ => panic!("failed not an expression statement"),
                    }
                } else {
                    panic!("failed")
                }
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);".to_string();

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
                            ast::Expression::CallExpression {
                                token,
                                arguments,
                                function,
                            } => {
                                assert_eq!(arguments.len(), 3);
                                assert_eq!(
                                    test_literal_expression(&arguments.get(0).unwrap(), &1),
                                    true
                                );
                                assert_eq!(
                                    test_infix_expression(
                                        &arguments.get(1).unwrap(),
                                        &2,
                                        "*".to_string(),
                                        &3
                                    ),
                                    true
                                );
                                assert_eq!(
                                    test_infix_expression(
                                        &arguments.get(2).unwrap(),
                                        &4,
                                        "+".to_string(),
                                        &5
                                    ),
                                    true
                                );

                                assert_eq!(test_identifier(&**function, "add".to_string()), true);
                            }
                            _ => {
                                panic!("not an ast Expr Call Expr");
                            }
                        }
                    }
                    _ => {
                        panic!("not an ast Expr");
                    }
                }
            }
        } else {
            panic!("failed");
        }
    }
}
