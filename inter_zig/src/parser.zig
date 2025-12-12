const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");

pub const ParseError = error{
    PrefixParseFnErr,
    InfixParseFnErr,
    UnexpectedToken,
} || std.mem.Allocator.Error;

pub const PrefixParseFn = *const fn (p: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression;
pub const InfixParseFn = *const fn (p: *Parser, left: ?ast.Expression, allocator: std.mem.Allocator) ParseError!?ast.Expression;

const Parser = struct {
    l: *lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    errors: std.ArrayList([]const u8),
    prefixParseFns: std.AutoHashMap(token.TokenTypes, PrefixParseFn),
    infixParseFns: std.AutoHashMap(token.TokenTypes, InfixParseFn),

    fn nextToken(self: *Parser) void {
        self.curToken = self.peekToken;
        self.peekToken = self.l.nextToken();
    }

    pub fn parseProgram(self: *Parser, allocator: std.mem.Allocator) !*ast.Program {
        var program = try allocator.create(ast.Program);

        program.* = ast.Program{
            .statements = std.ArrayList(*ast.Statement).init(allocator),
        };

        while (self.curToken.type != token.TokenTypes.EOF) {
            const stmt = try self.parseStatement(allocator);

            if (stmt) |stmtVal| {
                try program.statements.append(stmtVal);
            }

            self.nextToken();
        }

        return program;
    }

    fn parseStatement(self: *Parser, allocator: std.mem.Allocator) !?*ast.Statement {
        switch (self.curToken.type) {
            token.TokenTypes.LET => {
                return self.parseLetStatement(allocator);
            },
            token.TokenTypes.RETURN => {
                return self.parseReturnStatement(allocator);
            },
            else => {
                const expr_stmt = try self.parseExpressionStatement(allocator);
                if (expr_stmt == null) return null;

                const stmt = try allocator.create(ast.Statement);
                stmt.* = ast.Statement{ .ExpressionStatement = expr_stmt.? };
                return stmt;
            },
        }
    }

    fn parseLetStatement(self: *Parser, allocator: std.mem.Allocator) !?*ast.Statement {
        var stmt = try allocator.create(ast.LetStatement);
        stmt.* = ast.LetStatement{ .token = self.curToken, .value = null, .name = null };

        if (!try self.expectPeek(token.TokenTypes.IDENT, allocator)) {
            return null;
        }

        const ident = try allocator.create(ast.Identifier);

        ident.* = ast.Identifier{
            .token = self.curToken,
            .value = self.curToken.literal,
        };

        stmt.name = ident;

        if (!try self.expectPeek(token.TokenTypes.ASSIGN, allocator)) {
            return null;
        }

        self.nextToken();
        stmt.value = try self.parseExpression(Precedence.LOWEST, allocator);

        if (self.peekTokenIs(token.TokenTypes.SEMICOLON)) {
            self.nextToken();
        }

        const wrapper = try allocator.create(ast.Statement);
        wrapper.* = ast.Statement{ .LetStatement = stmt };

        return wrapper;
    }

    fn parseReturnStatement(self: *Parser, allocator: std.mem.Allocator) !?*ast.Statement {
        const stmt = try allocator.create(ast.ReturnStatement);
        stmt.* = ast.ReturnStatement{ .token = self.curToken, .returnValue = null };

        self.nextToken();

        stmt.returnValue = try self.parseExpression(Precedence.LOWEST, allocator);

        if (self.peekTokenIs(token.TokenTypes.SEMICOLON)) {
            self.nextToken();
        }

        const wrapper = try allocator.create(ast.Statement);
        wrapper.* = ast.Statement{ .ReturnStatement = stmt };

        return wrapper;
    }

    fn parseIntegerLiteral(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const lit = try allocator.create(ast.IntegerLiteral);
        lit.* = ast.IntegerLiteral{
            .token = self.curToken,
            .value = undefined,
        };

        const val = std.fmt.parseInt(i64, self.curToken.literal, 10) catch {
            return ParseError.UnexpectedToken;
        };

        lit.value = val;
        return ast.Expression{ .integerLiteral = lit };
    }

    //tracing in page 76 does not implemented
    fn parseExpressionStatement(self: *Parser, allocator: std.mem.Allocator) !?*ast.ExpressionStatement {
        const stmt = try allocator.create(ast.ExpressionStatement);
        stmt.* = ast.ExpressionStatement{ .token = self.curToken, .value = null };

        stmt.value = try self.parseExpression(Precedence.LOWEST, allocator);

        if (self.peekTokenIs(token.TokenTypes.SEMICOLON)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseExpression(self: *Parser, precendence: Precedence, allocator: std.mem.Allocator) !?ast.Expression {
        //std.debug.print("DEBUG parseExpression: curToken.type = {s}, literal = '{s}'\n", .{ @tagName(self.curToken.type), self.curToken.literal });
        const prefixFn = self.prefixParseFns.get(self.curToken.type) orelse {
            try self.noPrefixParseFnError(self.curToken.type, allocator);
            return null;
        };

        var leftExp = try prefixFn(self, allocator);

        while (!self.peekTokenIs(token.TokenTypes.SEMICOLON) and @intFromEnum(precendence) < self.peekPrecendence()) {
            const infixFn = self.infixParseFns.get(self.peekToken.type) orelse {
                return leftExp;
            };

            self.nextToken();

            leftExp = try infixFn(self, leftExp, allocator);
        }

        return leftExp;
    }

    fn curTokenIs(self: *Parser, t: token.TokenTypes) bool {
        return self.curToken.type == t;
    }

    fn peekTokenIs(self: *Parser, t: token.TokenTypes) bool {
        return self.peekToken.type == t;
    }

    fn expectPeek(self: *Parser, t: token.TokenTypes, allocator: std.mem.Allocator) !bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        } else {
            try self.peekError(t, allocator);
            return false;
        }
    }

    fn Errors(self: *Parser) std.ArrayList([]const u8) {
        return self.errors;
    }

    fn peekError(self: *Parser, t: token.TokenTypes, allocator: std.mem.Allocator) !void {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice("expected next token to be ");
        try msg.appendSlice(@tagName(t));
        try msg.appendSlice(" got ");
        try msg.appendSlice(@tagName(self.peekToken.type));
        try msg.appendSlice(" instead");
        try msg.appendSlice("\n");

        const saved = try allocator.alloc(u8, msg.items.len);
        @memcpy(saved, msg.items);

        try self.errors.append(saved);
    }

    pub fn registerPrefix(self: *Parser, tokenType: token.TokenTypes, func: PrefixParseFn) !void {
        try self.prefixParseFns.put(tokenType, func);
    }

    pub fn registerInfix(self: *Parser, tokenType: token.TokenTypes, func: InfixParseFn) !void {
        try self.infixParseFns.put(tokenType, func);
    }

    pub fn parseIdentifier(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const ident = try allocator.create(ast.Identifier);
        ident.* = ast.Identifier{
            .token = self.curToken,
            .value = self.curToken.literal,
        };
        const expr = ast.Expression{ .identifier = ident };

        return expr;
    }

    pub fn noPrefixParseFnError(self: *Parser, tokenType: token.TokenTypes, allocator: std.mem.Allocator) !void {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice("no prefix parse function for ");
        try msg.appendSlice(@tagName(tokenType));
        try msg.appendSlice(" found");
        try msg.appendSlice("\n");

        const saved = try allocator.alloc(u8, msg.items.len);
        @memcpy(saved, msg.items);

        try self.errors.append(saved);
    }
    pub fn parsePrefixExpression(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const prefix = try allocator.create(ast.PrefixExpression);

        const operatorCp = try allocator.alloc(u8, self.curToken.literal.len);
        @memcpy(operatorCp, self.curToken.literal);
        //std.debug.print("DEBUG: operatorCp = '{s}'\n", .{operatorCp});
        prefix.* = ast.PrefixExpression{
            .token = self.curToken,
            .operator = operatorCp,
            .right = null,
        };

        self.nextToken();

        //std.debug.print("DEBUG: after nextToken, operator = '{s}'\n", .{prefix.operator});
        prefix.right = try self.parseExpression(Precedence.PREFIX, allocator);

        const expr = ast.Expression{ .prefixExpression = prefix };

        return expr;
    }

    pub fn parseInfixExpression(self: *Parser, left: ?ast.Expression, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const infix = try allocator.create(ast.InfixExpression);

        const operatorCp = try allocator.alloc(u8, self.curToken.literal.len);
        @memcpy(operatorCp, self.curToken.literal);

        infix.* = ast.InfixExpression{
            .token = self.curToken,
            .operator = operatorCp,
            .left = left,
            .right = null,
        };

        const prece = self.curPrecendence();

        self.nextToken();

        const rightExpr = try self.parseExpression(@enumFromInt(prece), allocator) orelse {
            return ParseError.InfixParseFnErr;
        };

        infix.right = rightExpr;

        const expr = ast.Expression{ .infixExpression = infix };

        return expr;
    }

    fn peekPrecendence(self: *Parser) u8 {
        return @intFromEnum(precedence(self.peekToken.type));
    }

    fn curPrecendence(self: *Parser) u8 {
        return @intFromEnum(precedence(self.curToken.type));
    }

    fn parseBoolean(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const boolean = try allocator.create(ast.Boolean);
        boolean.* = ast.Boolean{
            .token = self.curToken,
            .value = self.curTokenIs(token.TokenTypes.TRUE),
        };
        return ast.Expression{ .boolean = boolean };
    }

    fn parseGroupedExpression(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(Precedence.LOWEST, allocator);

        if (!try self.expectPeek(token.TokenTypes.RPAREN, allocator)) {
            return null;
        }

        return exp;
    }

    fn parseIfExpression(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const exp = try allocator.create(ast.IfExpression);
        exp.* = ast.IfExpression{
            .token = self.curToken,
            .alternative = null,
            .condition = null,
            .consequence = undefined,
        };

        if (!try self.expectPeek(token.TokenTypes.LPAREN, allocator)) {
            return null;
        }

        self.nextToken();

        const condExp = try self.parseExpression(Precedence.LOWEST, allocator) orelse {
            return ParseError.InfixParseFnErr;
        };

        exp.condition = condExp;

        if (!try self.expectPeek(token.TokenTypes.RPAREN, allocator)) {
            return null;
        }

        if (!try self.expectPeek(token.TokenTypes.LBRACE, allocator)) {
            return null;
        }
        const blockStmt = try self.parseBlockStatement(allocator) orelse {
            return ParseError.InfixParseFnErr;
        };

        exp.consequence = blockStmt;

        if (self.peekTokenIs(token.TokenTypes.ELSE)) {
            self.nextToken();

            if (!try self.expectPeek(token.TokenTypes.LBRACE, allocator)) {
                return null;
            }

            const expAlternative = try self.parseBlockStatement(allocator) orelse {
                return ParseError.InfixParseFnErr;
            };

            exp.alternative = expAlternative;
        }

        return ast.Expression{ .ifExpression = exp };
    }

    fn parseBlockStatement(self: *Parser, allocator: std.mem.Allocator) !?*ast.BlockStatement {
        var block = try allocator.create(ast.BlockStatement);

        block.* = ast.BlockStatement{ .statements = std.ArrayList(*ast.Statement).init(allocator), .token = self.curToken };

        self.nextToken();

        while (!self.curTokenIs(token.TokenTypes.RBRACE) and !self.curTokenIs(token.TokenTypes.EOF)) {
            const stmt = try self.parseStatement(allocator);

            if (stmt) |stmtVal| {
                try block.statements.append(stmtVal);
            }

            self.nextToken();
        }

        return block;
    }

    fn parseFunctionLiteral(self: *Parser, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const lit = try allocator.create(ast.FunctionLiteral);
        lit.* = ast.FunctionLiteral{
            .token = self.curToken,
            .parameters = std.ArrayList(*ast.Identifier).init(allocator),
            .body = undefined,
        };

        if (!try self.expectPeek(token.TokenTypes.LPAREN, allocator)) {
            return null;
        }

        const litParams = try self.parseFunctionParameters(allocator) orelse {
            return ParseError.PrefixParseFnErr;
        };

        lit.parameters = litParams;

        if (!try self.expectPeek(token.TokenTypes.LBRACE, allocator)) {
            return null;
        }

        const blockStmt = try self.parseBlockStatement(allocator) orelse {
            return ParseError.PrefixParseFnErr;
        };

        lit.body = blockStmt;

        return ast.Expression{ .functionLiteral = lit };
    }

    fn parseFunctionParameters(self: *Parser, allocator: std.mem.Allocator) ParseError!?std.ArrayList(*ast.Identifier) {
        var identifiers = std.ArrayList(*ast.Identifier).init(allocator);

        if (self.peekTokenIs(token.TokenTypes.RPAREN)) {
            self.nextToken();
            return identifiers;
        }

        self.nextToken();
        const ident = try allocator.create(ast.Identifier);
        ident.* = ast.Identifier{
            .token = self.curToken,
            .value = self.curToken.literal,
        };
        try identifiers.append(ident);

        while (self.peekTokenIs(token.TokenTypes.COMMA)) {
            self.nextToken();
            self.nextToken();

            const ident2 = try allocator.create(ast.Identifier);
            ident2.* = ast.Identifier{
                .token = self.curToken,
                .value = self.curToken.literal,
            };

            try identifiers.append(ident2);
        }
        if (!try self.expectPeek(token.TokenTypes.RPAREN, allocator)) {
            return null;
        }

        return identifiers;
    }
    fn parseCallExpression(self: *Parser, function: ?ast.Expression, allocator: std.mem.Allocator) ParseError!?ast.Expression {
        const exp = try allocator.create(ast.CallExpression);
        exp.* = ast.CallExpression{
            .token = self.curToken,
            .function = function,
            .arguments = std.ArrayList(*ast.Expression).init(allocator),
        };

        const listArgs = try self.parseCallArguments(allocator) orelse {
            return ParseError.PrefixParseFnErr;
        };

        exp.arguments = listArgs;

        return ast.Expression{ .callExpression = exp };
    }

    fn parseCallArguments(self: *Parser, allocator: std.mem.Allocator) ParseError!?std.ArrayList(*ast.Expression) {
        var args = std.ArrayList(*ast.Expression).init(allocator);

        if (self.peekTokenIs(token.TokenTypes.RPAREN)) {
            self.nextToken();
            return args;
        }

        self.nextToken();

        //on the heap
        const firstArg = try allocator.create(ast.Expression);
        firstArg.* = try self.parseExpression(Precedence.LOWEST, allocator) orelse {
            return ParseError.PrefixParseFnErr;
        };

        try args.append(firstArg);

        while (self.peekTokenIs(token.TokenTypes.COMMA)) {
            self.nextToken();
            self.nextToken();

            const nextArg = try allocator.create(ast.Expression);
            nextArg.* = try self.parseExpression(Precedence.LOWEST, allocator) orelse {
                return ParseError.PrefixParseFnErr;
            };
            try args.append(nextArg);
        }

        if (!try self.expectPeek(token.TokenTypes.RPAREN, allocator)) {
            return null;
        }

        return args;
    }
};

const Precedence = enum(u8) {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

pub fn precedence(t: token.TokenTypes) Precedence {
    return switch (t) {
        .EQ, .NOT_EQ => Precedence.EQUALS,
        .LT, .GT => Precedence.LESSGREATER,
        .PLUS, .MINUS => Precedence.SUM,
        .SLASH, .ASTERISK => Precedence.PRODUCT,
        .LPAREN => Precedence.CALL,
        else => Precedence.LOWEST,
    };
}

pub fn new(l: *lexer.Lexer, allocator: std.mem.Allocator) ParseError!*Parser {
    var p = try allocator.create(Parser);
    p.* = Parser{
        .l = l,
        .curToken = undefined,
        .peekToken = undefined,
        .errors = std.ArrayList([]const u8).init(allocator),
        .prefixParseFns = std.AutoHashMap(token.TokenTypes, PrefixParseFn).init(allocator),
        .infixParseFns = std.AutoHashMap(token.TokenTypes, InfixParseFn).init(allocator),
    };

    try p.registerPrefix(token.TokenTypes.IDENT, &Parser.parseIdentifier);
    try p.registerPrefix(token.TokenTypes.INT, &Parser.parseIntegerLiteral);
    try p.registerPrefix(token.TokenTypes.BANG, &Parser.parsePrefixExpression);
    try p.registerPrefix(token.TokenTypes.MINUS, &Parser.parsePrefixExpression);
    try p.registerPrefix(token.TokenTypes.TRUE, &Parser.parseBoolean);
    try p.registerPrefix(token.TokenTypes.FALSE, &Parser.parseBoolean);
    try p.registerPrefix(token.TokenTypes.LPAREN, &Parser.parseGroupedExpression);
    try p.registerPrefix(token.TokenTypes.IF, &Parser.parseIfExpression);
    try p.registerPrefix(token.TokenTypes.FUNCTION, &Parser.parseFunctionLiteral);

    try p.registerInfix(token.TokenTypes.LPAREN, &Parser.parseCallExpression);
    try p.registerInfix(token.TokenTypes.PLUS, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.MINUS, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.SLASH, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.ASTERISK, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.EQ, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.NOT_EQ, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.LT, &Parser.parseInfixExpression);
    try p.registerInfix(token.TokenTypes.GT, &Parser.parseInfixExpression);
    p.nextToken();
    p.nextToken();

    return p;
}

test "test let statements" {
    const realTests = .{
        .{ "let x = 5;", "x", 5 },
        .{ "let y = true;", "y", true },
        .{ "let foobar = y;", "foobar", "y" },
    };

    inline for (realTests) |tes| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var l = lexer.new(tes[0]);
        const p = try new(&l, allocator);

        const program = try p.parseProgram(allocator);

        try std.testing.expect(checkParserErrors(p) == false);

        if (program.statements.items.len != 1) {
            std.debug.print("program.statements does not contain 3 statements got {d} \n", .{program.statements.items.len});
        }

        switch (program.statements.items[0].*) {
            .LetStatement => |stmt| {
                if (!testLetStatement(program.statements.items[0].*, tes[1])) {
                    std.debug.print("not a let statement \n", .{});
                    return TestError.IncorrectStatement;
                }

                const val = stmt.value.?;

                if (!try testLiteralExpression(val, @TypeOf(tes[2]), tes[2])) {
                    return TestError.IncorrectStatement;
                }
            },
            else => |_| {
                return TestError.IncorrectStatement;
            },
        }
    }
}

fn testLetStatement(s: ast.Statement, name: []const u8) bool {
    if (!std.mem.eql(u8, s.LetStatement.tokenLiteral(), "let")) {
        std.debug.print("s token literal not 'let' got={s}", .{s.LetStatement.tokenLiteral()});
        return false;
    }

    switch (s) {
        .LetStatement => |_| {
            //std.debug.print("name {s} \n", .{name});
        },
        else => {
            std.debug.print("not s LetStatement \n", .{});
        },
    }
    if (!std.mem.eql(u8, s.LetStatement.name.?.value, name)) {
        std.debug.print("s.LetStaement.name.?.value not {s}. got {s} \n", .{ name, s.LetStatement.name.?.value });
        return false;
    }

    return true;
}

fn checkParserErrors(p: *Parser) bool {
    const err = p.Errors();

    if (err.items.len == 0) {
        return false;
    }

    for (err.items) |errVal| {
        std.debug.print("parser error: {s}", .{errVal});
    }

    return true;
}
const TestError = error{
    IncorrectStatement,
    Other,
};

test "test return statements" {
    const input =
        \\ return 5;
        \\ return 10;
        \\ return 993322;
        \\
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);

    if (program.statements.items.len != 3) {
        std.debug.print("program.statements does not contain 3 statements got {d} \n", .{program.statements.items.len});
    }

    for (program.statements.items) |stmt| {
        switch (stmt.*) {
            .LetStatement => |_| {
                std.debug.print("error got letStatement \n", .{});
                return TestError.IncorrectStatement;
            },
            .ReturnStatement => |returnStmt| {
                try std.testing.expect(std.mem.eql(u8, returnStmt.tokenLiteral(), "return") == true);
            },
            else => |_| {
                return TestError.IncorrectStatement;
            },
        }
    }
}

test "test indentifier expression" {
    const input = "foobar;";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);

    if (program.statements.items.len != 1) {
        std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
        return TestError.IncorrectStatement;
    }

    for (program.statements.items) |stmt| {
        switch (stmt.*) {
            .ExpressionStatement => |esStmt| {
                //const ident = esStmt.value.?.identifier.value;
                if (esStmt.value) |_| {
                    const valIdent = esStmt.value.?.identifier.value;

                    const valReal = "foobar";
                    if (!std.mem.eql(u8, valIdent, valReal)) {
                        std.debug.print("ident value not {s} got {s} \n", .{ valReal, valIdent });
                        return TestError.IncorrectStatement;
                    }
                } else {
                    return TestError.IncorrectStatement;
                }
            },
            else => |_| {
                std.debug.print("program.statements is not .ExpressionStatement got={s} \n", .{@tagName(stmt.*)});
                return TestError.IncorrectStatement;
            },
        }
    }
}

test "test integer literal expression" {
    const input = "5;";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);

    if (program.statements.items.len != 1) {
        std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
        return TestError.IncorrectStatement;
    }

    for (program.statements.items) |stmt| {
        switch (stmt.*) {
            .ExpressionStatement => |esStmt| {
                const valInt = esStmt.value.?.integerLiteral.value;
                if (valInt != 5) {
                    std.debug.print("ident value not {d} got {d} \n", .{ 5, valInt });
                    return TestError.IncorrectStatement;
                }
            },
            else => |_| {
                std.debug.print("program.statements is not .ExpressionStatement got={s} \n", .{@tagName(stmt.*)});
                return TestError.IncorrectStatement;
            },
        }
    }
}

test "parsing prefix expressions" {
    const prefixTests = .{
        .{ "!5;", "!", 5 },
        .{ "-15;", "-", 15 },
        .{ "!true;", "!", true },
        .{ "!false;", "!", false },
    };

    inline for (prefixTests) |prefixTest| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var l = lexer.new(prefixTest[0]);
        const p = try new(&l, allocator);

        const program = try p.parseProgram(allocator);

        try std.testing.expect(checkParserErrors(p) == false);

        if (program.statements.items.len != 1) {
            std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
            return TestError.IncorrectStatement;
        }

        switch (program.statements.items[0].*) {
            .ExpressionStatement => |esStmt| {
                const valPrefix = esStmt.value.?.prefixExpression;
                //if (!std.mem.eql(u8, valPrefix.operator, prefixTest[1])) {
                //   std.debug.print("exp Operator is not {s} got {s} \n", .{ prefixTest[1], valPrefix.operator });
                //  return TestError.IncorrectStatement;
                //}

                const exp = valPrefix.right.?;

                try std.testing.expect(try testLiteralExpression(exp, @TypeOf(prefixTest[2]), prefixTest[2]) == true);
            },
            else => |_| {
                return TestError.IncorrectStatement;
            },
        }
    }
}

fn testIntegerLiteral(il: ast.Expression, value: i64) !bool {
    switch (il) {
        .integerLiteral => |intLit| {
            if (intLit.value != value) {
                std.debug.print("intLit value not {d} got {d}", .{ value, intLit.value });
                return TestError.IncorrectStatement;
            }
            var buf: [256]u8 = undefined;
            const strVal = try std.fmt.bufPrint(&buf, "{}", .{value});
            if (!std.mem.eql(u8, intLit.tokenLiteral(), strVal)) {
                std.debug.print("intLit tokenliteral not {s} got {s}", .{ strVal, intLit.tokenLiteral() });
                return TestError.IncorrectStatement;
            }

            return true;
        },
        else => |_| {
            return TestError.IncorrectStatement;
        },
    }
}

// 5 != 5;
// 5 / 5;
// 5 == 5;
// <expression> <infix operator> <expression>
test "test parsing infix expressions" {
    const infixTests = .{
        .{ "5 + 5;", 5, "+", 5 },
        .{ "5 - 5;", 5, "-", 5 },
        .{ "5 * 5;", 5, "*", 5 },
        .{ "5 / 5;", 5, "/", 5 },
        .{ "5 > 5;", 5, ">", 5 },
        .{ "5 < 5;", 5, "<", 5 },
        .{ "5 == 5;", 5, "==", 5 },
        .{ "5 != 5;", 5, "!=", 5 },
        .{ "true == true", true, "==", true },
        .{ "true != false", true, "!=", false },
        .{ "false == false", false, "==", false },
    };

    inline for (infixTests) |infixTest| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var l = lexer.new(infixTest[0]);
        const p = try new(&l, allocator);

        const program = try p.parseProgram(allocator);

        try std.testing.expect(checkParserErrors(p) == false);

        if (program.statements.items.len != 1) {
            std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
            return TestError.IncorrectStatement;
        }

        switch (program.statements.items[0].*) {
            .ExpressionStatement => |es| {
                const infixEx = es.value.?.infixExpression;

                //left
                try std.testing.expect(try testLiteralExpression(infixEx.left.?, @TypeOf(infixTest[1]), infixTest[1]) == true);

                //operator
                //if (!std.mem.eql(u8, infixEx.operator, infixTest[2])) {
                //   std.debug.print("exp Operator is not {s} got {s} \n", .{ infixTest[2], infixEx.operator });
                //  return TestError.IncorrectStatement;
                //}
                //right
                try std.testing.expect(try testLiteralExpression(infixEx.right.?, @TypeOf(infixTest[3]), infixTest[3]) == true);
            },
            else => |_| {
                return TestError.IncorrectStatement;
            },
        }
    }
}
test "test operator precendence parsing" {
    const infixTests = .{
        .{ "-a * b", "((-a) * b)" },
        .{ "!-a", "(!(-a))" },
        .{ "a + b + c", "((a + b) + c)" },
        .{ "a + b - c", "((a + b) - c)" },
        .{ "a * b * c", "((a * b) * c)" },
        .{ "a * b / c", "((a * b) / c)" },
        .{ "a + b / c", "(a + (b / c))" },
        .{ "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)" },
        .{ "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)" },
        .{ "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))" },
        .{ "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))" },
        .{ "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ "true", "true" },
        .{ "false", "false" },
        .{ "3 > 5 == false", "((3 > 5) == false)" },
        .{ "3 < 5 == true", "((3 < 5) == true)" },
        .{ "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)" },
        .{ "(5 + 5) * 2", "((5 + 5) * 2)" },
        .{ "2 / (5 + 5)", "(2 / (5 + 5))" },
        .{ "-(5 + 5)", "(-(5 + 5))" },
        .{ "!(true == true)", "(!(true == true))" },
        .{ "a + add(b * c) + d", "((a + add((b * c))) + d)" },
        .{ "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ "add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))" },
    };

    inline for (infixTests) |infixTest| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var l = lexer.new(infixTest[0]);
        const p = try new(&l, allocator);

        const program = try p.parseProgram(allocator);

        try std.testing.expect(checkParserErrors(p) == false);
        const actual = try program.string(allocator);

        if (!std.mem.eql(u8, actual, infixTest[1])) {
            std.debug.print("failed operator precendence expected={s} got={s} \n", .{ infixTest[1], actual });
        }
        try std.testing.expect(std.mem.eql(u8, actual, infixTest[1]));
    }
}

fn testIdentifier(exp: ast.Expression, value: []const u8) bool {
    switch (exp) {
        .identifier => |ident| {
            if (!std.mem.eql(u8, ident.value, value)) {
                std.debug.print("ident.value not {s} got={s}\n", .{ value, ident.value });
                return false;
            }
            if (!std.mem.eql(u8, ident.tokenLiteral(), value)) {
                std.debug.print("ident.tokenLiteral() not {s} got={s}\n", .{ value, ident.tokenLiteral() });
                return false;
            }

            return true;
        },
        .integerLiteral => |inte| {
            if (testIntegerLiteral(exp, inte.value) catch false) {
                return true;
            }
            return false;
        },
        .boolean => |bl| {
            if (!testBooleanLiteral(exp, bl.value)) {
                return false;
            }
            return true;
        },
        else => |val| {
            std.debug.print("exp not ast.identifier got={s}\n", .{@tagName(val)});
            return false;
        },
    }
}

fn testInfixExpression(exp: ast.Expression, comptime L: type, valueL: L, operator: []const u8, comptime R: type, valueR: R) !bool {
    switch (exp) {
        .infixExpression => |infix| {
            if (!try testLiteralExpression(infix.left.?, L, valueL)) {
                std.debug.print("left literal expression failed\n", .{});
                return false;
            }
            if (!std.mem.eql(u8, infix.operator, operator)) {
                std.debug.print("operator is not eql\n", .{});
                return false;
            }
            if (!try testLiteralExpression(infix.right.?, R, valueR)) {
                std.debug.print("right literal expression failed\n", .{});
                return false;
            }
            return true;
        },
        else => {
            std.debug.print("exp not ast.infixExpression \n", .{});
            return false;
        },
    }
}
fn testLiteralExpression(exp: anytype, comptime E: type, value: E) !bool {
    const T = @TypeOf(exp);

    if (T == ast.Expression) {
        return testLiteralExpressionUnion(exp, E, value);
    }

    if (T == *ast.Identifier) {
        if (@TypeOf(value) != @TypeOf("string literal")) {
            const typeInfo = @typeInfo(@TypeOf(value));

            if (typeInfo != .pointer) {
                std.debug.print("Identifier can only be tests with string\n", .{});
                return false;
            }
        }
        return testLiteralIdentifier(exp, value);
    }

    std.debug.print("not an ast.Expression or ast.Identifier got ={} \n", .{T});
    return false;
}

fn testLiteralExpressionUnion(exp: ast.Expression, comptime E: type, value: E) !bool {
    switch (@typeInfo(E)) {
        .int => |val| {
            if (E == i64) {
                return testIntegerLiteral(exp, val.bits);
            } else {
                return testIntegerLiteral(exp, @as(i64, val.bits));
            }
        },
        .comptime_int => {
            return testIntegerLiteral(exp, @as(i64, value));
        },
        .bool => {
            return testBooleanLiteral(exp, value);
        },
        .pointer => |ptr| {
            if (ptr.size == .slice and ptr.child == u8 and ptr.is_const) {
                return testIdentifier(exp, value);
            }

            if (ptr.size == .one and @typeInfo(ptr.child) == .array and @typeInfo(ptr.child).array.child == u8 and ptr.is_const) {
                const slice: []const u8 = value[0..];
                return testIdentifier(exp, slice);
            }

            std.debug.print("not a []const u8 but a {s} \n", .{@typeName(E)});
            return false;
        },
        else => {
            std.debug.print("type of exp not handle got={s}\n", .{@typeName(E)});
            return false;
        },
    }
}

fn testLiteralIdentifier(ident: *ast.Identifier, value: []const u8) bool {
    if (!std.mem.eql(u8, ident.value, value)) {
        std.debug.print("ident.value not {s} got={s}\n", .{ value, ident.value });
        return false;
    }

    if (!std.mem.eql(u8, ident.tokenLiteral(), value)) {
        std.debug.print("ident.tokenLiteral() not {s} got={s}\n", .{ value, ident.tokenLiteral() });
        return false;
    }

    return true;
}

fn testBooleanLiteral(exp: ast.Expression, value: bool) bool {
    switch (exp) {
        .boolean => |bo| {
            if (bo.value != value) {
                std.debug.print("bo.value not {} got={s}\n", .{ value, bo.tokenLiteral() });
                return false;
            }

            return true;
        },
        else => |val| {
            std.debug.print("exp not ast.boolean got={s}\n", .{@tagName(val)});
            return false;
        },
    }
}

test "test literal express and infix" {
    var anotherIdent = ast.Identifier{
        .token = .{ .type = token.TokenTypes.IDENT, .literal = "anotherVar" },
        .value = "anotherVar",
    };

    const valueExpr = ast.Expression{
        .identifier = &anotherIdent,
    };

    const lit: []const u8 = "anotherVar";
    try std.testing.expect(try testLiteralExpression(valueExpr, @TypeOf(lit), lit) == true);
}

test "test if expression" {
    const input =
        \\if (x < y) { x } else { y }
        \\
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);

    if (program.statements.items.len != 1) {
        std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
        return TestError.IncorrectStatement;
    }
    switch (program.statements.items[0].*) {
        .ExpressionStatement => |esStmt| {
            switch (esStmt.value.?) {
                .ifExpression => |ifEx| {
                    const left: []const u8 = "x";
                    const operator: []const u8 = "<";
                    const right: []const u8 = "y";

                    if (!try testInfixExpression(ifEx.*.condition.?, @TypeOf(left), left, operator, @TypeOf(right), right)) {
                        return TestError.IncorrectStatement;
                    }

                    if (ifEx.consequence.statements.items.len != 1) {
                        std.debug.print("consequence  is not 1 statements\n", .{});
                        return TestError.IncorrectStatement;
                    }

                    switch (ifEx.consequence.statements.items[0].*) {
                        .ExpressionStatement => |es| {
                            if (!testIdentifier(es.value.?, left)) {
                                return TestError.IncorrectStatement;
                            }

                            if (ifEx.alternative) |_| {
                                //std.debug.print("ifEx alternative.statements was not null \n", .{});
                            } else {
                                std.debug.print("ifEx alternative.statements was not null \n", .{});
                            }
                        },
                        else => |_| {
                            std.debug.print("statements[0] is not ast.ExpressionStatement\n", .{});
                            return TestError.IncorrectStatement;
                        },
                    }
                },
                else => |_| {
                    std.debug.print("stmt.Expression is not ast.ifExpression\n", .{});
                    return TestError.IncorrectStatement;
                },
            }
        },
        else => |_| {
            std.debug.print("program.statements[0] is not ast.ExpressionStatement \n", .{});
            return TestError.IncorrectStatement;
        },
    }
}

test "test function literal parsing" {
    const input =
        \\fn(x, y) { x + y }
        \\
    ;
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);

    if (program.statements.items.len != 1) {
        std.debug.print("program.statements does not contain 1 statements got {d} \n", .{program.statements.items.len});
        return TestError.IncorrectStatement;
    }
    switch (program.statements.items[0].*) {
        .ExpressionStatement => |esStmt| {
            switch (esStmt.value.?) {
                .functionLiteral => |fl| {
                    if (fl.parameters.items.len != 2) {
                        std.debug.print("function literal paramteres wrong want 2, got {d} \n", .{fl.parameters.items.len});
                        return TestError.IncorrectStatement;
                    }
                    const firstParam: []const u8 = "x";
                    const secondParam: []const u8 = "y";

                    if (!try testLiteralExpression(fl.parameters.items[0], @TypeOf(firstParam), firstParam)) {
                        std.debug.print("test literalExpression param 1 incorrect\n", .{});
                        return TestError.IncorrectStatement;
                    }
                    if (!try testLiteralExpression(fl.parameters.items[1], @TypeOf(secondParam), secondParam)) {
                        std.debug.print("test literalExpression param 2 incorrect\n", .{});
                        return TestError.IncorrectStatement;
                    }

                    if (fl.body.statements.items.len != 1) {
                        std.debug.print("function body statements has not 1 statements got={d}\n", .{fl.body.statements.items.len});
                        return TestError.IncorrectStatement;
                    }

                    switch (fl.body.statements.items[0].*) {
                        .ExpressionStatement => |es| {
                            const opr: []const u8 = "+";
                            if (!try testInfixExpression(es.value.?, @TypeOf(firstParam), firstParam, opr, @TypeOf(secondParam), secondParam)) {
                                std.debug.print("testInfixExpression failed\n", .{});
                                return TestError.IncorrectStatement;
                            }
                        },
                        else => |_| {
                            std.debug.print("function body stmt is not ast.ExpressionStatement\n", .{});
                            return TestError.IncorrectStatement;
                        },
                    }
                },
                else => |_| {
                    std.debug.print("stmt.Expression is not ast.ifExpression\n", .{});
                    return TestError.IncorrectStatement;
                },
            }
        },
        else => |_| {
            std.debug.print("program.statements[0] is not ast.ExpressionStatement \n", .{});
            return TestError.IncorrectStatement;
        },
    }
}

test "test function parameter parsing" {
    const infixTests = .{
        .{ "fn() {};", [_][]const u8{} },
        .{ "fn(x) {};", [_][]const u8{"x"} },
        .{ "fn(x, y, z) {};", [_][]const u8{ "x", "y", "z" } },
    };

    inline for (infixTests) |infixTest| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const allocator = arena.allocator();

        var l = lexer.new(infixTest[0]);
        const p = try new(&l, allocator);

        const program = try p.parseProgram(allocator);

        try std.testing.expect(checkParserErrors(p) == false);

        switch (program.statements.items[0].*) {
            .ExpressionStatement => |est| {
                switch (est.value.?) {
                    .functionLiteral => |fl| {
                        if (fl.parameters.items.len != infixTest[1].len) {
                            std.debug.print("length parameters wrong want {d} got {d}\n", .{ fl.parameters.items.len, infixTest[1].len });
                            return TestError.IncorrectStatement;
                        }

                        inline for (infixTest[1], 0..) |ident, i| {
                            if (!try testLiteralExpression(fl.parameters.items[i], @TypeOf(ident), ident)) {
                                std.debug.print("test literal Expressison failed \n", .{});
                                return TestError.IncorrectStatement;
                            }
                        }
                    },
                    else => {
                        return TestError.IncorrectStatement;
                    },
                }
            },
            else => |_| {
                std.debug.print("program.statements[0] is not ast.ExpressionStatement \n", .{});
                return TestError.IncorrectStatement;
            },
        }
    }
}

test "test call expression parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var l = lexer.new(input);
    const p = try new(&l, allocator);

    const program = try p.parseProgram(allocator);

    try std.testing.expect(checkParserErrors(p) == false);
    if (program.statements.items.len != 1) {
        std.debug.print("program statements does not contain {d} statements. got={d}\n", .{ 1, program.statements.items.len });
        return TestError.IncorrectStatement;
    }

    switch (program.statements.items[0].*) {
        .ExpressionStatement => |es| {
            switch (es.value.?) {
                .callExpression => |ce| {
                    if (!testIdentifier(ce.function.?, "add")) {
                        std.debug.print("test identifier failed\n", .{});
                        return TestError.IncorrectStatement;
                    }

                    if (ce.arguments.items.len != 3) {
                        std.debug.print("wrong length of arguments. got={d}\n", .{ce.arguments.items.len});
                        return TestError.IncorrectStatement;
                    }
                    const lit = 1;
                    if (!try testLiteralExpression(ce.arguments.items[0].*, @TypeOf(lit), lit)) {
                        return TestError.IncorrectStatement;
                    }

                    const left = 2;
                    const operator: []const u8 = "*";
                    const right = 3;

                    if (!try testInfixExpression(ce.arguments.items[1].*, @TypeOf(left), left, operator, @TypeOf(right), right)) {
                        return TestError.IncorrectStatement;
                    }

                    const leftx = 4;
                    const operatorx: []const u8 = "+";
                    const rightx = 5;
                    if (!try testInfixExpression(ce.arguments.items[2].*, @TypeOf(leftx), leftx, operatorx, @TypeOf(rightx), rightx)) {
                        return TestError.IncorrectStatement;
                    }
                },
                else => |_| {
                    std.debug.print("stmt expression is not ast callExpression \n", .{});
                    return TestError.IncorrectStatement;
                },
            }
        },
        else => |_| {
            std.debug.print("stmt is not ast.ExpressionStatement \n", .{});
            return TestError.IncorrectStatement;
        },
    }
}
