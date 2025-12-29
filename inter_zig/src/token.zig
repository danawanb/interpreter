const std = @import("std");
const expect = std.testing.expect;
const mem = @import("std").mem;

//pub const TokenType: []const u8 = undefined;

pub const Token = struct {
    type: TokenTypes,
    literal: []const u8,
};

pub const TokenTypes = enum {
    ILLEGAL,
    EOF,

    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

    STRING,

    pub fn literalx(self: TokenTypes) []const u8 {
        return switch (self) {
            .ILLEGAL => "ILLEGAL",
            .EOF => "EOF",
            .IDENT => "IDENT",
            .INT => "INT",
            .ASSIGN => "=",
            .PLUS => "+",
            .COMMA => ",",
            .SEMICOLON => ";",
            .LPAREN => "(",
            .RPAREN => ")",
            .LBRACE => "{",
            .RBRACE => "}",
            .FUNCTION => "FUNCTION",
            .LET => "LET",
            .MINUS => "-",
            .BANG => "!",
            .ASTERISK => "*",
            .SLASH => "/",
            .LT => "<",
            .GT => ">",
            .TRUE => "TRUE",
            .FALSE => "FALSE",
            .IF => "IF",
            .ELSE => "ELSE",
            .RETURN => "RETURN",
            .EQ => "==",
            .NOT_EQ => "!=",
            .STRING => "STRING",
        };
    }
};

pub fn lookupIdent(ident: []const u8) TokenTypes {
    const keywords = .{
        .{ "fn", TokenTypes.FUNCTION },
        .{ "let", TokenTypes.LET },
        .{ "true", TokenTypes.TRUE },
        .{ "false", TokenTypes.FALSE },
        .{ "if", TokenTypes.IF },
        .{ "else", TokenTypes.ELSE },
        .{ "return", TokenTypes.RETURN },
    };

    inline for (keywords) |kw| {
        if (std.mem.eql(u8, ident, kw[0])) return kw[1];
    }

    return TokenTypes.IDENT;
}

test "test enum literal" {
    var valueLet = TokenTypes.LET;
    try expect(mem.eql(u8, valueLet.literalx(), "LET") == true);
}
