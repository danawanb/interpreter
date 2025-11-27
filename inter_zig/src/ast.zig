const std = @import("std");
const token = @import("token.zig");

pub const StringError = error{OutOfMemory};

pub fn expressionString(expr: Expression, allocator: std.mem.Allocator) StringError![]const u8 {
    return switch (expr) {
        .identifier => |ident| ident.value,
        .integerLiteral => |intLit| intLit.token.literal,
        .boolean => |boolx| boolx.token.literal,
        .prefixExpression => |prefix| try prefix.string(allocator),
        .infixExpression => |infix| try infix.string(allocator),
    };
}

pub const Node = union(enum) {
    statement: *Statement,
    expression: *Expression,
};

// let num = 69;
// let <statement> = <identifier>;

pub const Statement = union(enum) {
    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
    ExpressionStatement: *ExpressionStatement,
};

pub const LetStatement = struct {
    token: token.Token,
    name: ?*Identifier,
    value: ?Expression,

    pub fn statementNode(_: *LetStatement) void {}

    pub fn tokenLiteral(self: *LetStatement) []const u8 {
        return self.token.literal;
    }

    fn string(self: *LetStatement, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice(self.tokenLiteral());
        try msg.appendSlice(" ");
        //todo
        if (self.name) |namex| {
            try msg.appendSlice(namex.value);
        }
        try msg.appendSlice(" = ");
        if (self.value) |val| {
            try msg.appendSlice(val.identifier.value);
        }
        try msg.appendSlice(";");
        const saved = try allocator.alloc(u8, msg.items.len);
        @memcpy(saved, msg.items);

        return saved;
    }
};

pub const Expression = union(enum) {
    identifier: *Identifier,
    integerLiteral: *IntegerLiteral,
    prefixExpression: *PrefixExpression,
    infixExpression: *InfixExpression,
    boolean: *Boolean,
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn expressionNode(_: *Identifier) void {}

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }

    fn string(self: *Identifier) []const u8 {
        return self.value;
    }
};

pub const Program = struct {
    statements: std.ArrayList(*Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .statements = std.ArrayList(*Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: *Program) []const u8 {
        if (self.statements.items.len == 0) {
            return "";
        }

        const stmt = self.statements.items[0];

        return switch (stmt) {
            .LetStatement => |letstmt| letstmt.tokenLiteral(),
            .ReturnStatement => |rs| rs.tokenLiteral(),
            .ExpressionStatement => |es| es.tokenLiteral(),
        };
    }
    pub fn string(self: *Program, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        for (self.statements.items) |s| {
            switch (s.*) {
                .LetStatement => |letstmt| {
                    try msg.appendSlice(try letstmt.string(allocator));
                },
                .ReturnStatement => |rs| try msg.appendSlice(try rs.string(allocator)),
                .ExpressionStatement => |es| {
                    const ss = try es.string(allocator);
                    try msg.appendSlice(ss);
                },
            }
        }

        const saved = try allocator.alloc(u8, msg.items.len);
        @memcpy(saved, msg.items);

        return saved;
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    returnValue: ?Expression,
    pub fn statementNode(_: *ReturnStatement) void {}

    pub fn tokenLiteral(self: *ReturnStatement) []const u8 {
        return self.token.literal;
    }

    fn string(self: *ReturnStatement, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice(self.tokenLiteral());
        try msg.appendSlice(" ");

        try msg.appendSlice(self.returnValue.?.identifier.string());

        try msg.appendSlice(";");
        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    value: ?Expression,

    pub fn statementNode(_: *ExpressionStatement) void {}

    pub fn tokenLiteral(self: *ExpressionStatement) []const u8 {
        return self.token.literal;
    }
    pub fn string(self: *ExpressionStatement, allocator: std.mem.Allocator) StringError![]const u8 {
        if (self.value) |val| {
            return switch (val) {
                .identifier => |ident| ident.value,
                .integerLiteral => |intLit| intLit.token.literal,
                .prefixExpression => |prefix| try prefix.string(allocator),
                .infixExpression => |infix| try infix.string(allocator),
                .boolean => |boolx| boolx.token.literal,
            };
        } else {
            return "";
        }
    }
};

pub const IntegerLiteral = struct {
    token: token.Token,
    value: i64,

    pub fn expressionNode(_: *IntegerLiteral) void {}

    pub fn tokenLiteral(self: *IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    fn string(self: *Identifier) []const u8 {
        return self.token.literal;
    }
};
pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    right: ?Expression,
    pub fn statementNode(_: *PrefixExpression) void {}

    pub fn tokenLiteral(self: *PrefixExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *PrefixExpression, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice("(");
        try msg.appendSlice(self.operator);
        //try msg.appendSlice(self.right.?.identifier.string());
        if (self.right) |right| {
            const rightStr = switch (right) {
                .identifier => |ident| ident.value,
                .integerLiteral => |intLit| intLit.token.literal,
                .boolean => |boolx| boolx.token.literal,
                .prefixExpression => |prefix| try prefix.string(allocator),
                .infixExpression => |infix| try infix.string(allocator),
            };
            try msg.appendSlice(rightStr);
        }
        try msg.appendSlice(")");

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

pub const InfixExpression = struct {
    token: token.Token,
    left: ?Expression,
    operator: []const u8,
    right: ?Expression,
    pub fn statementNode(_: *InfixExpression) void {}

    pub fn tokenLiteral(self: *InfixExpression) []const u8 {
        return self.token.literal;
    }

    fn string(self: *InfixExpression, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg = std.ArrayList(u8).init(allocator);
        defer msg.deinit();

        try msg.appendSlice("(");

        //try msg.appendSlice(self.left.?.identifier.string());
        if (self.left) |left| {
            const leftStr = switch (left) {
                .identifier => |ident| ident.value,
                .integerLiteral => |intLit| intLit.token.literal,
                .boolean => |boolx| boolx.token.literal,
                .prefixExpression => |prefix| try prefix.string(allocator),
                .infixExpression => |infix| try infix.string(allocator),
            };
            try msg.appendSlice(leftStr);
        }
        try msg.appendSlice(" ");
        try msg.appendSlice(self.operator);
        try msg.appendSlice(" ");

        //try msg.appendSlice(self.right.?.identifier.string());
        if (self.right) |right| {
            const rightStr = switch (right) {
                .identifier => |ident| ident.value,
                .integerLiteral => |intLit| intLit.token.literal,
                .boolean => |boolx| boolx.token.literal,
                .prefixExpression => |prefix| try prefix.string(allocator),
                .infixExpression => |infix| try infix.string(allocator),
            };
            try msg.appendSlice(rightStr);
        }
        try msg.appendSlice(")");

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

pub const Boolean = struct {
    token: token.Token,
    value: bool,

    pub fn expressionNode(_: *Boolean) void {}

    pub fn tokenLiteral(self: *Boolean) []const u8 {
        return self.token.literal;
    }

    fn string(self: *Boolean) []const u8 {
        return self.token.literal;
    }
};

test "test string" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var program = Program.init(allocator);

    var name = Identifier{
        .value = "myVar",
        .token = .{ .type = token.TokenTypes.IDENT, .literal = "myVar" },
    };

    var anotherIdent = Identifier{
        .token = .{ .type = token.TokenTypes.IDENT, .literal = "anotherVar" },
        .value = "anotherVar",
    };

    const valueExpr = Expression{
        .identifier = &anotherIdent,
    };

    var letStmt = LetStatement{
        .token = .{ .type = token.TokenTypes.LET, .literal = "let" },
        .name = &name,
        .value = valueExpr,
    };

    var stmtInsert = Statement{
        .LetStatement = &letStmt,
    };

    try program.statements.append(&stmtInsert);

    const out = try program.string(allocator);
    try std.testing.expect(std.mem.eql(u8, out, "let myVar = anotherVar;"));
}
