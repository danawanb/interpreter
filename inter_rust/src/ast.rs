use std::fmt::format;

use crate::lexer::{self, Lexer};

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
    IntegerLiteral {
        token: lexer::Token,
        value: i64,
    },
    Identifier {
        token: lexer::Token,
        value: String,
    },
    Prefix {
        token: lexer::Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: lexer::Token,
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean {
        token: lexer::Token,
        value: bool,
    },
    Nil,
}

impl Expression {
    fn value(&self) -> String {
        match self {
            Expression::IntegerLiteral { token, value } => {
                return format!("{}", value);
            }
            Expression::Boolean { token, value } => {
                return format!("{}", value);
            }
            Expression::Identifier { token, value } => {
                return value.clone();
            }
            Expression::Prefix {
                token,
                operator,
                right,
            } => {
                let mut out = String::new();
                out.push_str("(");
                out.push_str(operator);
                out.push_str(&right.value());
                out.push_str(")");

                out
            }
            Expression::Infix {
                token,
                left,
                operator,
                right,
            } => {
                let mut out = String::new();
                out.push_str("(");
                out.push_str(&left.value());
                out.push_str(" ");
                out.push_str(operator);
                out.push_str(" ");
                out.push_str(&right.value());
                out.push_str(")");

                out
            }
            Expression::Nil => {
                let mut out = String::new();
                out.push_str("NULL");
                out
            }
        }
    }
}

//trait Expression {}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn string(&self) -> String {
        if self.statements.len() > 0 {
            if let Some(_) = self.statements.get(0) {
                self.statements
                    .iter()
                    .map(|s| s.string())
                    .collect::<Vec<_>>()
                    .join("")
            } else {
                String::from("")
            }
        } else {
            String::from("")
        }
    }
}
