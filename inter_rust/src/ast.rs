use std::{fmt::format, process::Output};

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
    BlockStatement {
        token: lexer::Token,
        statements: Vec<Box<Statement>>,
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
            Statement::BlockStatement { token, statements } => {
                let mut out = String::new();
                for stmt in statements {
                    out.push_str(&*stmt.string());
                }
                out
            }
            Self::Nil => "Nil".to_string(),
        }
    }
}

pub enum Expression {
    FunctionLiteral {
        token: lexer::Token,
        parameters: Vec<Box<Expression>>,
        body: Box<Statement>,
    },
    CallExpression {
        token: lexer::Token,
        function: Box<Expression>,
        arguments: Vec<Box<Expression>>,
    },
    IfExpression {
        token: lexer::Token,
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },
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
            Expression::CallExpression {
                token,
                function,
                arguments,
            } => {
                let mut out = String::new();

                let mut args = Vec::new();
                for arg in arguments {
                    args.push(arg.value().clone());
                }

                out.push_str(*&function.value().as_str());
                out.push_str("(");

                let args_join = args
                    .iter()
                    .map(|x| x.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                out.push_str(&args_join);
                out.push_str(")");
                out
            }
            Expression::FunctionLiteral {
                token,
                parameters,
                body,
            } => {
                let mut out = String::new();

                let mut params = Vec::new();
                for param in parameters {
                    params.push(param.value().clone());
                }

                out.push_str(token.literal().as_str());
                out.push_str("(");

                let params_join = params
                    .iter()
                    .map(|x| x.as_str())
                    .collect::<Vec<_>>()
                    .join(", ");

                out.push_str(&params_join);
                out.push_str(")");
                out
            }
            Expression::IfExpression {
                token,
                condition,
                consequence,
                alternative,
            } => {
                let mut out = String::new();
                out.push_str("if");
                out.push_str(*&condition.value().as_str());
                out.push_str(" ");
                out.push_str(*&&consequence.string().as_str());

                if let Some(alt) = alternative {
                    out.push_str(&*alt.string());
                }
                out
            }
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
