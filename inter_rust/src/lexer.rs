#[derive(Hash, Debug, Clone, PartialEq)]
enum Token {
    ILLEGAL,
    EOF,
    IDENT(String),
    INT(Integer),

    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    IF,
    TRUE,
    FALSE,
    ELSE,
    RETURN,

    //operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    NOT_EQ,
    EQ,
}

impl Token {
    fn literal(&self) -> String {
        match &self {
            &Self::ILLEGAL => "ILLEGAL".to_string(),
            &Self::LPAREN => "(".to_string(),
            &Self::RPAREN => ")".to_string(),
            &Self::LBRACE => "{".to_string(),
            &Self::RBRACE => "}".to_string(),
            &Self::EOF => "EOF".to_string(),
            &Self::IDENT(val) => val.to_string(),
            &Self::ASSIGN => "=".to_string(),
            &Self::PLUS => "+".to_string(),
            &Self::COMMA => ",".to_string(),
            &Self::SEMICOLON => ";".to_string(),
            &Self::FUNCTION => "FUNCTION".to_string(),
            &Self::LET => "LET".to_string(),
            &Self::INT(val) => val.get_string_value(),
            &Self::IF => String::from("if"),
            &Self::MINUS => "-".to_string(),
            &Self::BANG => "!".to_string(),
            &Self::ASTERISK => "*".to_string(),
            &Self::SLASH => "/".to_string(),
            &Self::LT => "<".to_string(),
            &Self::GT => ">".to_string(),
            &Self::TRUE => "true".to_string(),
            &Self::FALSE => "false".to_string(),
            &Self::ELSE => "else".to_string(),
            &Self::NOT_EQ => "!=".to_string(),
            &Self::EQ => "!=".to_string(),
            _ => "".to_string(),
        }
    }
}

#[derive(Hash, Clone, PartialEq, Debug)]
enum Integer {
    Integer32(i32),
    Integer64(i64),
    //Integer128(i128),
}

impl Integer {
    fn get_string_value(&self) -> String {
        match self {
            &Integer::Integer32(val) => val.to_string(),
            &Integer::Integer64(val) => val.to_string(),
        }
    }
}
struct Lexer {
    input: String,
    position: i32,
    read_position: i32,
    ch: char,
}

impl Lexer {
    fn new(input: String) -> Box<Lexer> {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '0',
        };

        l.read_char();
        return Box::new(l);
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as i32 {
            self.ch = '\0';
        } else {
            self.ch = self.input.as_bytes()[self.read_position as usize] as char;
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Option<&str> {
        let pos = self.position as usize;

        //todo exercise
        while self.ch.is_alphabetic() {
            self.read_char();
        }

        if let Some(res) = self.input.get(pos..self.position as usize) {
            Some(res)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn read_digit(&mut self) -> Option<&str> {
        let pos = self.position as usize;

        while self.ch.is_digit(10) {
            self.read_char();
        }

        if let Some(res) = self.input.get(pos..self.position as usize) {
            Some(res)
        } else {
            None
        }
    }
    fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() as i32 {
            None
        } else {
            Some(self.input.as_bytes()[self.read_position as usize] as char)
        }
    }

    fn next_token(&mut self) -> Token {
        let mut tok: Token = Token::ILLEGAL;
        self.skip_whitespace();
        match self.ch {
            '=' => {
                if let Some(val) = self.peek_char() {
                    if val == '=' {
                        self.read_char();
                        self.read_char();
                        tok = Token::EQ;
                        return tok;
                    } else {
                        tok = Token::ASSIGN;
                    }
                }
            }
            '+' => tok = Token::PLUS,
            '-' => tok = Token::MINUS,
            '!' => {
                if let Some(val) = self.peek_char() {
                    if val == '=' {
                        self.read_char();
                        self.read_char();
                        tok = Token::NOT_EQ;
                        return tok;
                    } else {
                        tok = Token::BANG;
                    }
                }
            }
            '/' => tok = Token::SLASH,
            '*' => tok = Token::ASTERISK,
            '<' => tok = Token::LT,
            '>' => tok = Token::GT,
            ';' => tok = Token::SEMICOLON,
            '(' => tok = Token::LPAREN,
            ')' => tok = Token::RPAREN,
            ',' => tok = Token::COMMA,
            '{' => tok = Token::LBRACE,
            '}' => tok = Token::RBRACE,
            _ => {
                //let start = self.position;

                if self.ch.is_alphabetic() {
                    if let Some(lit) = self.read_identifier() {
                        tok = lookup_ident(lit);
                        return tok;
                    }
                } else if self.ch.is_digit(10) {
                    if let Some(lit_int) = self.read_digit() {
                        tok = Token::INT(Integer::Integer32(lit_int.parse().expect("must be int")));
                        return tok;
                    }
                } else {
                    tok = Token::ILLEGAL
                }
            }
        }

        self.read_char();

        return tok;
    }
}

static KEYWORDS: [(&str, Token); 7] = [
    ("let", Token::LET),
    ("fn", Token::FUNCTION),
    ("true", Token::TRUE),
    ("false", Token::FALSE),
    ("if", Token::IF),
    ("else", Token::ELSE),
    ("return", Token::RETURN),
];

fn lookup_ident(ident: &str) -> Token {
    for key in &KEYWORDS {
        if key.0 == ident {
            return key.1.clone();
        }
    }
    return Token::IDENT(String::from(ident));
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let input = r#"
            let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }
    
            10 == 10;
            10 != 9;
        "#;
        let tests: [(Token, &str); _] = [
            (Token::LET, "let"),
            (Token::IDENT("five".to_string()), "five"),
            (Token::ASSIGN, "="),
            (Token::INT(Integer::Integer32(5)), "5"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT("ten".to_string()), "ten"),
            (Token::ASSIGN, "="),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT("add".to_string()), "add"),
            (Token::ASSIGN, "="),
            (Token::FUNCTION, "fn"),
            (Token::LPAREN, "("),
            (Token::IDENT(String::from("x")), "x"),
            (Token::COMMA, ","),
            (Token::IDENT(String::from("y")), "y"),
            (Token::RPAREN, ")"),
            (Token::LBRACE, "{"),
            (Token::IDENT(String::from("x")), "x"),
            (Token::PLUS, "+"),
            (Token::IDENT(String::from("y")), "y"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::SEMICOLON, ";"),
            (Token::LET, "let"),
            (Token::IDENT(String::from("result")), "result"),
            (Token::ASSIGN, "="),
            (Token::IDENT(String::from("add")), "add"),
            (Token::LPAREN, "("),
            (Token::IDENT(String::from("five")), "five"),
            (Token::COMMA, ","),
            (Token::IDENT(String::from("ten")), "ten"),
            (Token::RPAREN, ")"),
            (Token::SEMICOLON, ";"),
            (Token::BANG, "!"),
            (Token::MINUS, "-"),
            (Token::SLASH, "/"),
            (Token::ASTERISK, "*"),
            (Token::INT(Integer::Integer32(5)), "5"),
            (Token::SEMICOLON, ";"),
            (Token::INT(Integer::Integer32(5)), "5"),
            (Token::LT, "<"),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::GT, ">"),
            (Token::INT(Integer::Integer32(5)), "5"),
            (Token::SEMICOLON, ";"),
            (Token::IF, "if"),
            (Token::LPAREN, "("),
            (Token::INT(Integer::Integer32(5)), "5"),
            (Token::LT, "<"),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::RPAREN, ")"),
            (Token::LBRACE, "{"),
            (Token::RETURN, "return"),
            (Token::TRUE, "true"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::ELSE, "else"),
            (Token::LBRACE, "{"),
            (Token::RETURN, "return"),
            (Token::FALSE, "false"),
            (Token::SEMICOLON, ";"),
            (Token::RBRACE, "}"),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::EQ, "=="),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::SEMICOLON, ";"),
            (Token::INT(Integer::Integer32(10)), "10"),
            (Token::NOT_EQ, "!="),
            (Token::INT(Integer::Integer32(9)), "9"),
            (Token::SEMICOLON, ";"),
        ];

        let mut l = Lexer::new(input.to_string());
        for test in tests {
            let tok = l.next_token();
            assert_eq!(tok, test.0);
            println!("{:?} {:?}", tok.literal(), test.0.literal());
        }
    }
}
