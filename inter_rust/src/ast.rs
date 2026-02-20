use crate::lexer;

impl Statement {
    fn literal(&self) -> String {
        //match self {
        //Statement::LetStatement(x) => x.literal(),
        // Self::ReturnStatement => String::from(""),
        //}
        //
        String::from("ss")
    }
}

pub enum Node {
    Program(Program),
    Statement(Statement),
    Expression,
    Identifier(lexer::Token, String),
}

pub enum Statement {
    Let {
        token: lexer::Token,
        name: Box<Identifier>,
        value: Box<Expression>,
    },
    ReturnStatement,
    Nil,
}

pub struct Identifier {
    pub token: lexer::Token,
    pub value: String,
}

pub struct Expression {}

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
