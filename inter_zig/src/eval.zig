const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const lexer = @import("lexer.zig");

const parser = @import("parser.zig");

pub fn eval(node: ast.Node, allocator: std.mem.Allocator) !object.Object {
    return switch (node) {
        .program => |p| return try evalProgram(p, allocator),
        .statement => |_| return try createNull(allocator),
        .expression => |es| return try evalExpression(es),
    };
}

pub fn evalProgram(program: *ast.Program, allocator: std.mem.Allocator) !object.Object {
    var res: object.Object = undefined;
    if (program.statements.items.len == 0) {
        return try createNull(allocator);
    }
    for (program.statements.items) |stmt| {
        res = try evalStatement(stmt, allocator);
    }

    return res;
}
fn evalStatement(stmt: *ast.Statement, allocator: std.mem.Allocator) !object.Object {
    return switch (stmt.*) {
        .ExpressionStatement => |es| {
            return try evalExpression(es.value.?, allocator);
        },
        else => |_| {
            return try createNull(allocator);
        },
    };
}
fn evalExpression(expr: ast.Expression, allocator: std.mem.Allocator) !object.Object {
    switch (expr) {
        .integerLiteral => |intLit| {
            const intObj = try allocator.create(object.Integer);
            intObj.* = object.Integer{
                .value = intLit.value,
            };

            return object.Object{ .integer = intObj };
        },
        else => |_| {
            return try createNull(allocator);
        },
    }
}

fn createNull(allocator: std.mem.Allocator) !object.Object {
    const intObj = try allocator.create(object.Null);
    intObj.* = object.Null{ .value = {} };
    return object.Object{ .nullx = intObj };
}

test "test eval integer expression" {
    const tests = .{
        .{ "5", @as(i64, 5) },
        .{ "10", @as(i64, 10) },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        //std.debug.print("{}\n", .{tes[1]});
        const obj = try testEval(tes[0], allocator);
        try std.testing.expect(testIntegerObject(obj, tes[1]));
    }
}

fn testEval(input: []const u8, allocator: std.mem.Allocator) !object.Object {
    var l = lexer.new(input);
    const p = try parser.new(&l, allocator);

    const program = try p.parseProgram(allocator);
    try std.testing.expect(parser.checkParserErrors(p) == false);

    return evalProgram(program, allocator);
}

fn testIntegerObject(obj: object.Object, expected: i64) bool {
    switch (obj) {
        .integer => |val| {
            if (val.value != expected) {
                std.debug.print("object has wrong value got={d} want={d}\n", .{ val.value, expected });
                return false;
            }
            return true;
        },
        else => |_| {
            std.debug.print("object is not integer \n", .{});
            return false;
        },
    }
}
