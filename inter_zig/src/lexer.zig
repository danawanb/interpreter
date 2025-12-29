const std = @import("std");
const token = @import("token.zig");

const expect = std.testing.expect;

pub fn new(input: []const u8) Lexer {
    var l = Lexer{
        .input = input,
        .position = 0,
        .readPosition = 0,
        .ch = undefined,
    };
    l.readChar();
    return l;
}

pub const Lexer = struct {
    input: []const u8,
    position: u64,
    readPosition: u64,
    ch: u8,

    pub fn readChar(self: *Lexer) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }

        self.position = self.readPosition;
        self.readPosition += 1;
    }

    //invalid
    pub fn nextToken2(self: *Lexer) token.Token {
        var tok: token.Token = undefined;

        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tok = token.Token{ .type = token.TokenTypes.EQ, .literal = "==" };
                } else {
                    tok = newToken(token.TokenTypes.ASSIGN, self.ch);
                }
            },
            ';' => {
                tok = newToken(token.TokenTypes.SEMICOLON, self.ch);
            },
            '(' => {
                tok = newToken(token.TokenTypes.LPAREN, self.ch);
            },
            ')' => {
                tok = newToken(token.TokenTypes.RPAREN, self.ch);
            },
            ',' => {
                tok = newToken(token.TokenTypes.COMMA, self.ch);
            },
            '+' => {
                tok = newToken(token.TokenTypes.PLUS, self.ch);
            },
            '{' => {
                tok = newToken(token.TokenTypes.LBRACE, self.ch);
            },
            '}' => {
                tok = newToken(token.TokenTypes.RBRACE, self.ch);
            },
            '-' => {
                tok = newToken(token.TokenTypes.MINUS, self.ch);
            },
            '!' => {
                if (self.peekChar() == '=') {
                    self.readChar();
                    tok = token.Token{ .type = token.TokenTypes.NOT_EQ, .literal = "!=" };
                } else {
                    tok = newToken(token.TokenTypes.BANG, self.ch);
                }
            },
            '/' => {
                tok = newToken(token.TokenTypes.SLASH, self.ch);
            },
            '*' => {
                tok = newToken(token.TokenTypes.ASTERISK, self.ch);
            },
            '<' => {
                tok = newToken(token.TokenTypes.LT, self.ch);
            },
            '>' => {
                tok = newToken(token.TokenTypes.GT, self.ch);
            },
            0 => {
                tok = newToken(token.TokenTypes.EOF, self.ch);
                tok.literal = "";
            },
            else => {
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.type = token.lookupIdent(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.type = token.TokenTypes.INT;
                    tok.literal = self.readNumber();
                    return tok;
                } else {
                    tok = newToken(token.TokenTypes.ILLEGAL, self.ch);
                }
            },
        }

        self.readChar();
        return tok;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        var tok: token.Token = undefined;

        self.skipWhitespace();

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar();
                    tok = token.Token{
                        .type = token.TokenTypes.EQ,
                        .literal = self.input[start .. self.position + 1],
                    };
                } else {
                    tok = token.Token{
                        .type = token.TokenTypes.ASSIGN,
                        .literal = self.input[self.position .. self.position + 1],
                    };
                }
            },
            ';' => {
                tok = token.Token{ .type = token.TokenTypes.SEMICOLON, .literal = self.input[self.position .. self.position + 1] };
            },
            '(' => {
                tok = token.Token{ .type = token.TokenTypes.LPAREN, .literal = self.input[self.position .. self.position + 1] };
            },
            ')' => {
                tok = token.Token{ .type = token.TokenTypes.RPAREN, .literal = self.input[self.position .. self.position + 1] };
            },
            ',' => {
                tok = token.Token{ .type = token.TokenTypes.COMMA, .literal = self.input[self.position .. self.position + 1] };
            },
            '+' => {
                tok = token.Token{ .type = token.TokenTypes.PLUS, .literal = self.input[self.position .. self.position + 1] };
            },
            '{' => {
                tok = token.Token{ .type = token.TokenTypes.LBRACE, .literal = self.input[self.position .. self.position + 1] };
            },
            '}' => {
                tok = token.Token{ .type = token.TokenTypes.RBRACE, .literal = self.input[self.position .. self.position + 1] };
            },
            '-' => {
                tok = token.Token{ .type = token.TokenTypes.MINUS, .literal = self.input[self.position .. self.position + 1] };
            },
            '!' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar();
                    tok = token.Token{ .type = token.TokenTypes.NOT_EQ, .literal = self.input[start .. self.position + 1] };
                } else {
                    tok = token.Token{ .type = token.TokenTypes.BANG, .literal = self.input[self.position .. self.position + 1] };
                }
            },
            '/' => {
                tok = token.Token{ .type = token.TokenTypes.SLASH, .literal = self.input[self.position .. self.position + 1] };
            },
            '*' => {
                tok = token.Token{ .type = token.TokenTypes.ASTERISK, .literal = self.input[self.position .. self.position + 1] };
            },
            '<' => {
                tok = token.Token{ .type = token.TokenTypes.LT, .literal = self.input[self.position .. self.position + 1] };
            },
            '>' => {
                tok = token.Token{ .type = token.TokenTypes.GT, .literal = self.input[self.position .. self.position + 1] };
            },
            '"' => {
                tok = token.Token{ .type = token.TokenTypes.STRING, .literal = self.readString() };
            },
            0 => {
                tok = token.Token{
                    .type = token.TokenTypes.EOF,
                    .literal = "",
                };
            },
            else => {
                if (isLetter(self.ch)) {
                    tok.literal = self.readIdentifier();
                    tok.type = token.lookupIdent(tok.literal);
                    return tok;
                } else if (isDigit(self.ch)) {
                    tok.type = token.TokenTypes.INT;
                    tok.literal = self.readNumber();
                    return tok;
                } else {
                    tok = token.Token{
                        .type = token.TokenTypes.ILLEGAL,
                        .literal = self.input[self.position .. self.position + 1],
                    };
                }
            },
        }

        self.readChar();
        return tok;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        while (isLetter(self.ch)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    //unused
    fn newToken(tokenType: token.TokenTypes, ch: u8) token.Token {
        return token.Token{
            .type = tokenType,
            .literal = &[_]u8{ch}, // salah di stack
        };
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;

        while (isDigit(self.ch)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }
    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            self.readChar();
        }
    }

    fn peekChar(self: *Lexer) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.readPosition];
        }
    }

    fn readString(self: *Lexer) []const u8 {
        const position = self.position + 1;

        while (true) {
            self.readChar();

            if (self.ch == '"' or self.ch == 0) {
                break;
            }
        }

        return self.input[position..self.position];
    }
};

fn isLetter(ch: u8) bool {
    //return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isDigit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}
const TestNext = struct {
    expectedType: token.TokenTypes,
    expectedLiteral: []const u8,
};

test "next token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\ x + y;
        \\};
        \\ let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\ if (5 < 10) {
        \\  return true;
        \\ } else {
        \\  return false;
        \\ }
        \\
        \\ 10 == 10;
        \\ 10 != 9;
        \\ "foobar"
        \\ "foo bar"
    ;

    const tests = [_]TestNext{
        .{ .expectedType = token.TokenTypes.LET, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = token.TokenTypes.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        //
        .{ .expectedType = token.TokenTypes.LET, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = token.TokenTypes.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        //
        .{ .expectedType = token.TokenTypes.LET, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = token.TokenTypes.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenTypes.FUNCTION, .expectedLiteral = "fn" },
        .{ .expectedType = token.TokenTypes.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = token.TokenTypes.COMMA, .expectedLiteral = "," },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = token.TokenTypes.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenTypes.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "x" },
        .{ .expectedType = token.TokenTypes.PLUS, .expectedLiteral = "+" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "y" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        //
        .{ .expectedType = token.TokenTypes.LET, .expectedLiteral = "let" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "result" },
        .{ .expectedType = token.TokenTypes.ASSIGN, .expectedLiteral = "=" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "add" },
        .{ .expectedType = token.TokenTypes.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "five" },
        .{ .expectedType = token.TokenTypes.COMMA, .expectedLiteral = "," },
        .{ .expectedType = token.TokenTypes.IDENT, .expectedLiteral = "ten" },
        .{ .expectedType = token.TokenTypes.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.BANG, .expectedLiteral = "!" },
        .{ .expectedType = token.TokenTypes.MINUS, .expectedLiteral = "-" },
        .{ .expectedType = token.TokenTypes.SLASH, .expectedLiteral = "/" },
        .{ .expectedType = token.TokenTypes.ASTERISK, .expectedLiteral = "*" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenTypes.LT, .expectedLiteral = "<" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.GT, .expectedLiteral = ">" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.IF, .expectedLiteral = "if" },
        .{ .expectedType = token.TokenTypes.LPAREN, .expectedLiteral = "(" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "5" },
        .{ .expectedType = token.TokenTypes.LT, .expectedLiteral = "<" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.RPAREN, .expectedLiteral = ")" },
        .{ .expectedType = token.TokenTypes.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenTypes.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = token.TokenTypes.TRUE, .expectedLiteral = "true" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenTypes.ELSE, .expectedLiteral = "else" },
        .{ .expectedType = token.TokenTypes.LBRACE, .expectedLiteral = "{" },
        .{ .expectedType = token.TokenTypes.RETURN, .expectedLiteral = "return" },
        .{ .expectedType = token.TokenTypes.FALSE, .expectedLiteral = "false" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.RBRACE, .expectedLiteral = "}" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.EQ, .expectedLiteral = "==" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "10" },
        .{ .expectedType = token.TokenTypes.NOT_EQ, .expectedLiteral = "!=" },
        .{ .expectedType = token.TokenTypes.INT, .expectedLiteral = "9" },
        .{ .expectedType = token.TokenTypes.SEMICOLON, .expectedLiteral = ";" },
        .{ .expectedType = token.TokenTypes.STRING, .expectedLiteral = "foobar" },
        .{ .expectedType = token.TokenTypes.STRING, .expectedLiteral = "foo bar" },
    };

    var l = new(input);

    for (tests) |tt| {
        const tok = l.nextToken();
        if (tok.type != tt.expectedType) {
            std.debug.print("tokentype wrong. expected={s}, got={s} \n", .{ @tagName(tt.expectedType), @tagName(tok.type) });
        }
        if (!std.mem.eql(u8, tok.literal, tt.expectedLiteral)) {
            std.debug.print("literal wrong. expected={s}, got={s} \n", .{ tt.expectedLiteral, tok.literal });
        }
    }
}
