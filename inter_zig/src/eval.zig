const std = @import("std");
const ast = @import("ast.zig");
const object = @import("object.zig");
const lexer = @import("lexer.zig");

const parser = @import("parser.zig");

var TRUE: *object.Boolean = undefined;
var FALSE: *object.Boolean = undefined;
var NULL: *object.Null = undefined;

pub fn initObjVal(allocator: std.mem.Allocator) !void {
    TRUE = try allocator.create(object.Boolean);
    FALSE = try allocator.create(object.Boolean);
    NULL = try allocator.create(object.Null);

    TRUE.* = .{ .value = true };
    FALSE.* = .{ .value = false };
    NULL.* = .{ .value = {} };
}

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
    try initObjVal(allocator);
    switch (expr) {
        .integerLiteral => |intLit| {
            const intObj = try allocator.create(object.Integer);
            intObj.* = object.Integer{
                .value = intLit.value,
            };

            return object.Object{ .integer = intObj };
        },
        .boolean => |boolLit| {
            const nativeBool = nativeBoolToBooleanObject(boolLit.value);
            return object.Object{ .boolean = nativeBool };
        },
        .prefixExpression => |pe| {
            const right = try evalExpression(pe.right.?, allocator);
            return try evalPrefixExpression(pe.operator, right, allocator);
        },
        .infixExpression => |ie| {
            const left = try evalExpression(ie.left.?, allocator);
            const right = try evalExpression(ie.right.?, allocator);
            return try evalInfixExpression(ie.operator, left, right, allocator);
        },
        else => |_| {
            return try createNull(allocator);
        },
    }
}

fn nativeBoolToBooleanObject(input: bool) *object.Boolean {
    if (input) {
        return TRUE;
    } else {
        return FALSE;
    }
}

fn evalPrefixExpression(operator: []const u8, right: object.Object, allocator: std.mem.Allocator) !object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return try evalMinusPrevixOperatorExpression(right, allocator);
    } else {
        return object.Object{ .nullx = NULL };
    }
}

fn evalInfixExpression(operator: []const u8, left: object.Object, right: object.Object, allocator: std.mem.Allocator) !object.Object {
    if (left.type_obj() == object.ObjectTypes.INTEGER_OBJ and right.type_obj() == object.ObjectTypes.INTEGER_OBJ) {
        return evalIntegerInfixExpression(operator, left, right, allocator);
    } else {
        return object.Object{ .nullx = NULL };
    }
}

fn evalMinusPrevixOperatorExpression(right: object.Object, allocator: std.mem.Allocator) !object.Object {
    switch (right) {
        .integer => |value| {
            const intObj = try allocator.create(object.Integer);
            intObj.* = object.Integer{
                .value = -value.value,
            };
            return object.Object{ .integer = intObj };
        },
        else => |_| {
            return object.Object{ .nullx = NULL };
        },
    }
}
fn evalBangOperatorExpression(right: object.Object) object.Object {
    switch (right) {
        .boolean => |bl| {
            if (bl.value) {
                return object.Object{ .boolean = nativeBoolToBooleanObject(false) };
            } else {
                return object.Object{ .boolean = nativeBoolToBooleanObject(true) };
            }
        },
        .nullx => |_| {
            return object.Object{ .boolean = nativeBoolToBooleanObject(true) };
        },
        else => |_| {
            return object.Object{ .boolean = nativeBoolToBooleanObject(false) };
        },
    }
}

fn evalIntegerInfixExpression(operator: []const u8, left: object.Object, right: object.Object, allocator: std.mem.Allocator) !object.Object {
    const leftVal = left.integer.value;
    const rightVal = right.integer.value;

    if (std.mem.eql(u8, operator, "+")) {
        const intObj = try allocator.create(object.Integer);
        intObj.* = object.Integer{
            .value = leftVal + rightVal,
        };
        return object.Object{ .integer = intObj };
    } else if (std.mem.eql(u8, operator, "-")) {
        const intObj = try allocator.create(object.Integer);
        intObj.* = object.Integer{
            .value = leftVal - rightVal,
        };
        return object.Object{ .integer = intObj };
    } else if (std.mem.eql(u8, operator, "*")) {
        const intObj = try allocator.create(object.Integer);
        intObj.* = object.Integer{
            .value = leftVal * rightVal,
        };
        return object.Object{ .integer = intObj };
    } else if (std.mem.eql(u8, operator, "/")) {
        const intObj = try allocator.create(object.Integer);
        intObj.* = object.Integer{
            .value = @divExact(leftVal, rightVal),
        };
        return object.Object{ .integer = intObj };
    } else {
        return object.Object{ .nullx = NULL };
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
        .{ "-5", @as(i64, -5) },
        .{ "-10", @as(i64, -10) },
        .{ "5 + 5 + 5 + 5 - 10", 10 },
        .{ "2 * 2 * 2 * 2 * 2", 32 },
        .{ "-50 + 100 + -50", 0 },
        .{ "5 * 2 + 10", 20 },
        .{ "5 + 2 * 10", 25 },
        .{ "20 + 2 * -10", 0 },
        .{ "50 / 2 * 2 + 10", 60 },
        .{ "2 * (5 + 10)", 30 },
        .{ "3 * 3 * 3 + 10", 37 },
        .{ "3 * (3 * 3) + 10", 37 },
        .{ "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50 },
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

fn testBooleanObject(obj: object.Object, expected: bool) bool {
    switch (obj) {
        .boolean => |val| {
            if (val.value != expected) {
                std.debug.print("object has wrong value got={} want={}\n", .{ val.value, expected });
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

test "test eval boolean expression" {
    const tests = .{
        .{ "true", true },
        .{ "false", false },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        //std.debug.print("{}\n", .{tes[1]});
        const obj = try testEval(tes[0], allocator);
        try std.testing.expect(testBooleanObject(obj, tes[1]));
    }
}

test "test bang operator" {
    const tests = .{
        .{ "!true", false },
        .{ "!false", true },
        .{ "!5", false },
        .{ "!!true", true },
        .{ "!!false", false },
        .{ "!!5", true },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        //std.debug.print("{}\n", .{tes[1]});
        const obj = try testEval(tes[0], allocator);
        try std.testing.expect(testBooleanObject(obj, tes[1]));
    }
}
