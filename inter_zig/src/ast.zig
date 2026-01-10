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
        .ifExpression => |ifE| try ifE.string(allocator),
        .functionLiteral => |fl| try fl.string(allocator),
        .callExpression => |ce| try ce.string(allocator),
        .stringLiteral => |sl| sl.string(),
        .arrayLiteral => |al| try al.string(allocator),
        .indexExpression => |ie| try ie.string(allocator),
        .hashLiteral => |hl| try hl.string(allocator),
    };
}

pub const Node = union(enum) {
    program: *Program,
    statement: *Statement,
    expression: *Expression,
};

// let num = 69;
// let <statement> = <identifier>;

pub const Statement = union(enum) {
    LetStatement: *LetStatement,
    ReturnStatement: *ReturnStatement,
    ExpressionStatement: *ExpressionStatement,
    BlockStatement: *BlockStatement,
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
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        try msg.appendSlice(allocator, self.tokenLiteral());
        try msg.appendSlice(allocator, " ");
        //todo
        if (self.name) |namex| {
            try msg.appendSlice(allocator, namex.value);
        }
        try msg.appendSlice(allocator, " = ");
        if (self.value) |val| {
            const valStr = try expressionString(val, allocator);
            try msg.appendSlice(allocator, valStr);
        }
        try msg.appendSlice(allocator, ";");
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
    ifExpression: *IfExpression,
    functionLiteral: *FunctionLiteral,
    callExpression: *CallExpression,
    stringLiteral: *StringLiteral,
    arrayLiteral: *ArrayLiteral,
    indexExpression: *IndexExpression,
    hashLiteral: *HashLiteral,
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,

    pub fn expressionNode(_: *Identifier) void {}

    pub fn tokenLiteral(self: *Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *Identifier) []const u8 {
        return self.value;
    }
};

pub const Program = struct {
    statements: std.ArrayList(*Statement),

    pub fn init() Program {
        return .{
            .statements = .{},
        };
    }

    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.statements.deinit(allocator);
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
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        for (self.statements.items) |s| {
            switch (s.*) {
                .LetStatement => |letstmt| {
                    try msg.appendSlice(allocator, try letstmt.string(allocator));
                },
                .ReturnStatement => |rs| try msg.appendSlice(allocator, try rs.string(allocator)),
                .ExpressionStatement => |es| {
                    const ss = try es.string(allocator);
                    try msg.appendSlice(allocator, ss);
                },
                .BlockStatement => |bs| {
                    const ss = try bs.string(allocator);
                    try msg.appendSlice(allocator, ss);
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
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        try msg.appendSlice(allocator, self.tokenLiteral());
        try msg.appendSlice(allocator, " ");

        if (self.returnValue) |val| {
            const valStr = try expressionString(val, allocator);
            try msg.appendSlice(allocator, valStr);
        }

        try msg.appendSlice(allocator, ";");
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
            const eStr = try expressionString(val, allocator);
            return eStr;
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

pub const BlockStatement = struct {
    token: token.Token,
    statements: std.ArrayList(*Statement),

    pub fn statementNode(_: *BlockStatement) void {}

    pub fn tokenLiteral(self: *BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *BlockStatement, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        for (self.statements.items) |s| {
            switch (s.*) {
                .LetStatement => |letstmt| {
                    try msg.appendSlice(allocator, try letstmt.string(allocator));
                },
                .ReturnStatement => |rs| try msg.appendSlice(allocator, try rs.string(allocator)),
                .ExpressionStatement => |es| {
                    const ss = try es.string(allocator);
                    try msg.appendSlice(allocator, ss);
                },
                .BlockStatement => |bs| {
                    const ss = try bs.string(allocator);
                    try msg.appendSlice(allocator, ss);
                },
            }
        }

        const saved = try allocator.alloc(u8, msg.items.len);
        @memcpy(saved, msg.items);

        return saved;
    }
};

pub const IfExpression = struct {
    token: token.Token,
    condition: ?Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn expressionNode(_: *IfExpression) void {}

    pub fn tokenLiteral(self: *IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *IfExpression, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        try msg.appendSlice(allocator, "if");

        //condition
        if (self.condition) |cond| {
            const condStr = try expressionString(cond, allocator);
            try msg.appendSlice(allocator, condStr);
        }

        try msg.appendSlice(allocator, " ");

        //consequence
        try msg.appendSlice(allocator, try self.consequence.string(allocator));

        //alternative
        if (self.alternative) |alt| {
            try msg.appendSlice(allocator, "else ");
            try msg.appendSlice(allocator, try alt.string(allocator));
        }

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
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
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        try msg.appendSlice(allocator, "(");
        try msg.appendSlice(allocator, self.operator);
        //try msg.appendSlice(self.right.?.identifier.string());
        if (self.right) |right| {
            const rightStr = try expressionString(right, allocator);
            try msg.appendSlice(allocator, rightStr);
        }
        try msg.appendSlice(allocator, ")");

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
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        try msg.appendSlice(allocator, "(");

        //try msg.appendSlice(self.left.?.identifier.string());
        if (self.left) |left| {
            const leftStr = try expressionString(left, allocator);
            try msg.appendSlice(allocator, leftStr);
        }
        try msg.appendSlice(allocator, " ");
        try msg.appendSlice(allocator, self.operator);
        try msg.appendSlice(allocator, " ");

        //try msg.appendSlice(self.right.?.identifier.string());
        if (self.right) |right| {
            const rightStr = try expressionString(right, allocator);
            try msg.appendSlice(allocator, rightStr);
        }
        try msg.appendSlice(allocator, ")");

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

//
//fn (x, y) {
//  return x + y;
//}
//fn <parameters> <block statement>
pub const FunctionLiteral = struct {
    token: token.Token,
    parameters: std.ArrayList(*Identifier),
    body: *BlockStatement,

    pub fn expressionNode(_: *FunctionLiteral) void {}

    pub fn tokenLiteral(self: *FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *FunctionLiteral, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        var params: std.ArrayList([]const u8) = .{};

        for (self.parameters.items) |item| {
            try params.append(allocator, item.string());
        }
        try msg.appendSlice(allocator, self.token.literal);
        try msg.appendSlice(allocator, "(");
        try msg.appendSlice(allocator, try stringsJoin(params, ", ", allocator));
        try msg.appendSlice(allocator, ") ");
        try msg.appendSlice(allocator, try self.body.string(allocator));

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

//<expression>(<comma separated exprressions>)
//add(2, 3);
pub const CallExpression = struct {
    token: token.Token, //The '(' token
    function: ?Expression, //Identifier or FunctionLiteral
    arguments: std.ArrayList(*Expression),

    pub fn expressionNode(_: *CallExpression) void {}

    pub fn tokenLiteral(self: *CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *CallExpression, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        var args: std.ArrayList([]const u8) = .{};

        for (self.arguments.items) |item| {
            const argStr = try expressionString(item.*, allocator);
            try args.append(allocator, argStr);
        }
        //try msg.appendSlice(try self.function.?.functionLiteral.string(allocator));
        if (self.function) |func| {
            const funcStr = try expressionString(func, allocator);
            try msg.appendSlice(allocator, funcStr);
        }
        try msg.appendSlice(allocator, "(");
        try msg.appendSlice(allocator, try stringsJoin(args, ", ", allocator));
        try msg.appendSlice(allocator, ")");

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

pub const StringLiteral = struct {
    token: token.Token,
    value: []const u8,

    pub fn expressionNode(_: *StringLiteral) void {}

    pub fn tokenLiteral(self: *StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *StringLiteral) []const u8 {
        return self.token.literal;
    }
};

pub const ArrayLiteral = struct {
    token: token.Token,
    elements: std.ArrayList(*Expression),

    pub fn expressionNode(_: *ArrayLiteral) void {}

    pub fn tokenLiteral(self: *ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *ArrayLiteral, allocator: std.mem.Allocator) StringError![]const u8 {
        var msg: std.ArrayList(u8) = .{};
        defer msg.deinit(allocator);

        var elems: std.ArrayList([]const u8) = .{};

        for (self.elements.items) |item| {
            const elemStr = try expressionString(item.*, allocator);
            try elems.append(allocator, elemStr);
        }

        try msg.appendSlice(allocator, "[");
        try msg.appendSlice(allocator, try stringsJoin(elems, ", ", allocator));
        try msg.appendSlice(allocator, "]");

        const saved = try allocator.dupe(u8, msg.items);
        return saved;
    }
};

pub const IndexExpression = struct {
    token: token.Token,
    left: ?Expression,
    index: ?Expression,

    pub fn expressionNode(_: *IndexExpression) void {}

    pub fn tokenLiteral(self: *IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *IndexExpression, allocator: std.mem.Allocator) StringError![]const u8 {
        var out: std.ArrayList(u8) = .{};
        defer out.deinit(allocator);

        try out.appendSlice(allocator, "(");
        try out.appendSlice(allocator, try expressionString(self.left.?, allocator));
        try out.appendSlice(allocator, "[");
        try out.appendSlice(allocator, try expressionString(self.index.?, allocator));
        try out.appendSlice(allocator, "])");

        const saved = try allocator.dupe(u8, out.items);
        return saved;
    }
};

pub const HashLiteral = struct {
    token: token.Token,
    pairs: std.AutoHashMap(*Expression, *Expression),

    pub fn expressionNode(_: *HashLiteral) void {}

    pub fn tokenLiteral(self: *HashLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *HashLiteral, allocator: std.mem.Allocator) StringError![]const u8 {
        var out: std.ArrayList(u8) = .{};
        defer out.deinit(allocator);

        var pairs: std.ArrayList([]const u8) = .{};

        var iterator = self.pairs.iterator();
        while (iterator.next()) |entry| {
            try pairs.append(allocator, try expressionString(entry.value_ptr.*.*, allocator));
            try pairs.append(allocator, ":");
            try pairs.append(allocator, try expressionString(entry.value_ptr.*.*, allocator));
        }

        try out.appendSlice(allocator, "{");
        try out.appendSlice(allocator, try stringsJoin(pairs, ", ", allocator));
        try out.appendSlice(allocator, "}");

        const saved = try allocator.dupe(u8, out.items);
        return saved;
    }
};

pub fn stringsJoin(arr: std.ArrayList([]const u8), conc: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    var res: std.ArrayList(u8) = .{};

    for (arr.items, 0..) |items, index| {
        try res.appendSlice(allocator, items);
        if (index < (arr.items.len - 1)) {
            try res.appendSlice(allocator, conc);
        }
    }

    const saved = try allocator.alloc(u8, res.items.len);
    @memcpy(saved, res.items);

    return saved;
}

test "strings join" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var res: std.ArrayList([]const u8) = .{};

    const part1: []const u8 = "haha";
    const part2: []const u8 = "hihi";

    try res.append(allocator, part1);
    try res.append(allocator, part2);

    const resJoin = try stringsJoin(res, ", ", allocator);
    const expectedRes: []const u8 = "haha, hihi";
    try std.testing.expect(std.mem.eql(u8, resJoin, expectedRes) == true);
}

test "test string" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var program = Program.init();

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

    try program.statements.append(allocator, &stmtInsert);

    const out = try program.string(allocator);
    try std.testing.expect(std.mem.eql(u8, out, "let myVar = anotherVar;"));
}
