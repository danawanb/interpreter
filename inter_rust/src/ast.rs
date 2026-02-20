use std::fmt::format;

use crate::lexer;

pub enum Statement {
    Let {
        token: lexer::Token,
        name: Box<Expression>,
        value: Box<Expression>,
    },
    Return {
        token: lexer::Token,
        value: Box<Expression>,
    },
    ExpressionStatement {
        token: lexer::Token,
        value: Box<Expression>,
    },
    Nil,
}

impl Statement {
    fn literal(&self) -> String {
        //match self {
        //Statement::LetStatement(x) => x.literal(),
        // Self::ReturnStatement => String::from(""),
        //}
        //
        String::from("ss")
    }

    fn string(&self) -> String {
        match self {
            Statement::Let { token, name, value } => {
                let mut out = String::new();
                out.push_str(token.literal().as_str());
                out.push_str(" ");
                out.push_str(&name.value());
                out.push_str(" = ");
                out.push_str(&value.value());
                out.push_str(";");
                out
            }
            Statement::Return { token, value } => {
                let mut out = String::new();
                out.push_str(token.literal().as_str());
                out.push_str(" ");
                out.push_str(&value.value());

                out.push_str(";");
                out
            }
            Statement::ExpressionStatement { token, value } => {
                let mut out = String::new();
                out.push_str(&value.value());
                out
            }
            Self::Nil => "Nil".to_string(),
        }
    }
}

pub enum Expression {
    Integer(i64),
    Identifier { token: lexer::Token, value: String },
}

impl Expression {
    fn value(&self) -> String {
        match self {
            Expression::Integer(val) => {
                return format!("{}", val);
            }
            Expression::Identifier { token, value } => {
                return value.clone();
            }
        }
    }
}

//trait Expression {}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Program {
    fn literal(&self) -> String {
        if self.statements.len() > 0 {
            if let Some(val) = self.statements.get(0) {
                val.literal()
            } else {
                String::from("")
            }
        } else {
            String::from("")
        }
    }
}
