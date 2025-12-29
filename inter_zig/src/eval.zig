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

pub fn eval(node: ast.Node, env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
    return switch (node) {
        .program => |p| return try evalProgram(p, allocator),
        .statement => |_| return try createNull(allocator),
        .expression => |es| return try evalExpression(es, env, allocator),
    };
}

pub fn evalProgram(program: *ast.Program, env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
    var res: object.Object = undefined;
    if (program.statements.items.len == 0) {
        return try createNull(allocator);
    }
    //for (program.statements.items) |stmt| {
    res = try evalStatements(program.statements, env, allocator);
    //}

    return res;
}

//this is a replacement for eval program honestly
fn evalStatements(stmts: std.ArrayList(*ast.Statement), env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
    var res: object.Object = object.Object{ .nullx = NULL };

    for (stmts.items) |stmt| {
        switch (stmt.*) {
            .ExpressionStatement => |es| {
                res = try evalExpression(es.value.?, env, allocator);
                if (res == .errorMessage) {
                    const errObj = try allocator.create(object.Error);
                    errObj.* = object.Error{ .message = res.errorMessage.message };
                    return object.Object{ .errorMessage = errObj };
                }
            },
            .BlockStatement => |bs| {
                return try evalBlockStatement(bs, env, allocator);
            },
            .ReturnStatement => |rs| {
                const val = try evalExpression(rs.returnValue.?, env, allocator);
                const retObj = try allocator.create(object.ReturnValue);
                retObj.* = object.ReturnValue{ .value = val };
                if (val == .errorMessage) {
                    const errObj = try allocator.create(object.Error);
                    errObj.* = object.Error{ .message = val.errorMessage.message };
                    return object.Object{ .errorMessage = errObj };
                }
                return object.Object{ .returnValue = retObj };
            },
            .LetStatement => |ls| {
                const val = try evalExpression(ls.value.?, env, allocator);
                if (val == .errorMessage) {
                    const errObj = try allocator.create(object.Error);
                    errObj.* = object.Error{ .message = res.errorMessage.message };
                    return object.Object{ .errorMessage = errObj };
                }
                _ = try env.set(ls.name.?.value, val);
            },
        }
        if (res.type_obj() == object.ObjectTypes.RETURN_VALUE_OBJ) {
            return res;
        }
    }

    return res;
}

fn evalBlockStatement(block: *ast.BlockStatement, env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
    var res: object.Object = undefined;
    for (block.statements.items) |bstmt| {
        res = try evalStatement(bstmt, env, allocator);

        if (res.type_obj() != object.ObjectTypes.NULL_OBJ) {
            if (res.type_obj() == object.ObjectTypes.RETURN_VALUE_OBJ or res.type_obj() == object.ObjectTypes.ERROR_OBJ) {
                return res;
            }
        }
    }

    return res;
}

fn evalStatement(stmt: *ast.Statement, env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
    var res: object.Object = undefined;
    switch (stmt.*) {
        .BlockStatement => |bs| {
            return try evalBlockStatement(bs, env, allocator);
        },
        .ReturnStatement => |rs| {
            const val = try evalExpression(rs.returnValue.?, env, allocator);
            if (isError(val)) {
                return val;
            }
            const retObj = try allocator.create(object.ReturnValue);
            retObj.* = object.ReturnValue{ .value = val };
            return object.Object{ .returnValue = retObj };
        },
        .ExpressionStatement => |es| {
            res = try evalExpression(es.value.?, env, allocator);
        },
        else => |_| {
            //res = object.Object{ .nullx = NULL };
        },
    }
    return res;
}
fn evalExpression(expr: ast.Expression, env: *object.Environment, allocator: std.mem.Allocator) anyerror!object.Object {
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
            const right = try evalExpression(pe.right.?, env, allocator);
            if (isError(right)) {
                return right;
            }
            return try evalPrefixExpression(pe.operator, right, allocator);
        },
        .infixExpression => |ie| {
            const left = try evalExpression(ie.left.?, env, allocator);
            if (isError(left)) {
                return left;
            }
            const right = try evalExpression(ie.right.?, env, allocator);
            if (isError(right)) {
                return right;
            }
            return try evalInfixExpression(ie.operator, left, right, allocator);
        },
        .ifExpression => |ie| {
            return try evalIfExpression(ie, env, allocator);
        },
        .identifier => |ii| {
            return try evalIdentifier(ii, env, allocator);
        },
        .functionLiteral => |fl| {
            const params = fl.parameters;
            const body = fl.body;
            const fnObj = try allocator.create(object.Function);
            fnObj.* = object.Function{
                .body = body,
                .parameters = params,
                .env = env,
            };

            return object.Object{ .function = fnObj };
        },
        .callExpression => |ce| {
            const function = try evalExpression(ce.function.?, env, allocator);
            if (isError(function)) {
                return function;
            }
            const args = try evalExpressions(ce.arguments, env, allocator);
            if (args.items.len == 1 and isError(args.items[0])) {
                return args.items[0];
            }

            return applyFunction(function, args, allocator);
        },
    }
}

fn isTruthy(obj: object.Object) bool {
    switch (obj) {
        .boolean => |bl| {
            return bl.value;
        },
        .nullx => |_| {
            return false;
        },
        else => |_| {
            return true;
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

fn evalExpressions(exps: std.ArrayList(*ast.Expression), env: *object.Environment, allocator: std.mem.Allocator) !std.ArrayList(object.Object) {
    var result = std.ArrayList(object.Object).init(allocator);

    for (exps.items) |exp| {
        const evaluated = try evalExpression(exp.*, env, allocator);
        if (isError(evaluated)) {
            try result.append(evaluated);
            return result;
        }

        try result.append(evaluated);
    }

    return result;
}

fn applyFunction(funx: object.Object, args: std.ArrayList(object.Object), allocator: std.mem.Allocator) !object.Object {
    if (funx != .function) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "not a function: {s}\n", .{@tagName(funx.type_obj())});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        const extendedEnv = try extendFunctionEnv(funx.function.*, args, allocator);
        const funxBodyStmt = try allocator.create(ast.Statement);
        funxBodyStmt.* = ast.Statement{ .BlockStatement = funx.function.body };
        const evaluated = try evalStatement(funxBodyStmt, extendedEnv, allocator);

        return unwrapReturnValue(evaluated);
    }
}

fn extendFunctionEnv(funx: object.Function, args: std.ArrayList(object.Object), allocator: std.mem.Allocator) !*object.Environment {
    const env = try object.newEnclosedEnvironment(funx.env, allocator);

    for (funx.parameters.items, 0..funx.parameters.items.len) |param, paramIdx| {
        _ = try env.set(param.value, args.items[paramIdx]);
    }

    return env;
}

fn unwrapReturnValue(obj: object.Object) object.Object {
    if (obj.type_obj() == object.ObjectTypes.RETURN_VALUE_OBJ) {
        return obj.returnValue.value;
    }

    return obj;
}

fn evalPrefixExpression(operator: []const u8, right: object.Object, allocator: std.mem.Allocator) !object.Object {
    if (std.mem.eql(u8, operator, "!")) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, operator, "-")) {
        return try evalMinusPrevixOperatorExpression(right, allocator);
    } else {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "unknown operator: {s} {s}", .{ operator, @tagName(right.type_obj()) });
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    }
}

fn evalInfixExpression(operator: []const u8, left: object.Object, right: object.Object, allocator: std.mem.Allocator) !object.Object {
    if (left.type_obj() == object.ObjectTypes.INTEGER_OBJ and right.type_obj() == object.ObjectTypes.INTEGER_OBJ) {
        return evalIntegerInfixExpression(operator, left, right, allocator);
    } else if (std.mem.eql(u8, operator, "==")) {
        const val = left.boolean.value == right.boolean.value;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else if (std.mem.eql(u8, operator, "!=")) {
        const val = left.boolean.value != right.boolean.value;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else if (left.type_obj() != right.type_obj()) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "type mismatch: {s} {s} {s}", .{ @tagName(left.type_obj()), operator, @tagName(right.type_obj()) });
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "unknown operator: {s} {s} {s}", .{ @tagName(left.type_obj()), operator, @tagName(right.type_obj()) });
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    }
}

fn evalMinusPrevixOperatorExpression(right: object.Object, allocator: std.mem.Allocator) !object.Object {
    if (right.type_obj() != object.ObjectTypes.INTEGER_OBJ) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "unknown operator: -{s}", .{@tagName(right.type_obj())});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    }

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
    } else if (std.mem.eql(u8, operator, "<")) {
        const val = leftVal < rightVal;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else if (std.mem.eql(u8, operator, ">")) {
        const val = leftVal > rightVal;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else if (std.mem.eql(u8, operator, "==")) {
        const val = leftVal == rightVal;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else if (std.mem.eql(u8, operator, "!=")) {
        const val = leftVal != rightVal;
        return object.Object{ .boolean = nativeBoolToBooleanObject(val) };
    } else {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "unknown operator: {s} {s} {s}", .{ @tagName(left.type_obj()), operator, @tagName(right.type_obj()) });
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    }
}

fn evalIfExpression(ie: *ast.IfExpression, env: *object.Environment, allocator: std.mem.Allocator) !object.Object {
    const condition = try evalExpression(ie.condition.?, env, allocator);

    if (isError(condition)) {
        return condition;
    }

    if (isTruthy(condition)) {
        return try evalStatements(ie.consequence.statements, env, allocator);
    }
    if (ie.alternative) |alt| {
        return try evalStatements(alt.statements, env, allocator);
    } else {
        return object.Object{ .nullx = NULL };
    }
}

fn evalIdentifier(ei: *ast.Identifier, env: *object.Environment, allocator: std.mem.Allocator) !object.Object {
    if (env.get(ei.value)) |val| {
        return val;
    } else {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "identifier not found: {s}", .{ei.value});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    }
}
fn createNull(allocator: std.mem.Allocator) !object.Object {
    const intObj = try allocator.create(object.Null);
    intObj.* = object.Null{ .value = {} };
    return object.Object{ .nullx = intObj };
}

fn newError(allocator: std.mem.Allocator, message: []const u8) ![]const u8 {
    try std.fmt.allocPrint(allocator, "{s}", .{message});
}

fn isError(obj: object.Object) bool {
    if (obj.type_obj() != object.ObjectTypes.NULL_OBJ) {
        return obj.type_obj() == object.ObjectTypes.ERROR_OBJ;
    } else {
        return false;
    }
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
    const env = try object.newEnvironment(allocator);
    return try evalProgram(program, env, allocator);
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
        else => |elobj| {
            std.debug.print("object is not integer {s} \n", .{@tagName(elobj.type_obj())});
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
        .{ "1 < 2", true },
        .{ "1 > 2", false },
        .{ "1 < 1", false },
        .{ "1 > 1", false },
        .{ "1 == 1", true },
        .{ "1 != 1", false },
        .{ "1 == 2", false },
        .{ "1 != 2", true },
        .{ "true == true", true },
        .{ "false == false", true },
        .{ "true == false", false },
        .{ "true != false", true },
        .{ "false != true", true },
        .{ "(1 < 2) == true", true },
        .{ "(1 < 2) == false", false },
        .{ "(1 > 2) == true", false },
        .{ "(1 > 2) == false", true },
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

test "test if else expression" {
    const tests = .{
        .{ "if (true) { 10 } ", 10 },
        .{ "if (false) { 10 } ", null },
        .{ "if (1) { 10 }", 10 },
        .{ "if (1 < 2) { 10 }", 10 },
        .{ "if (1 > 2) { 10 }", null },
        .{ "if (1 > 2) { 10 } else { 20 }", 20 },
        .{ "if (1 < 2) { 10 } else { 20 }", 10 },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        const obj = try testEval(tes[0], allocator);
        if (@TypeOf(tes[1]) == comptime_int) {
            try std.testing.expect(testIntegerObject(obj, @as(i64, tes[1])));
        } else {
            try std.testing.expect(testNullObject(obj) == true);
        }
    }
}

fn testNullObject(obj: object.Object) bool {
    if (obj.type_obj() != object.ObjectTypes.NULL_OBJ) {
        std.debug.print("object is not NULL\n", .{});
        return false;
    }

    return true;
}

test "test return statements" {
    const tests = .{
        .{ "return 10;", 10 },
        .{ "return 10; 9", 10 },
        .{ "return 2 * 5 ;", 10 },
        .{ "9; return 2 * 5; 9;", 10 },
        .{
            \\if (10 > 1 ) {
            \\ if (10 > 1) {
            \\ return 10;
            \\}
            \\ return 1;
            \\}
            ,
            10,
        },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        const obj = try testEval(tes[0], allocator);
        switch (obj) {
            .returnValue => |rv| {
                try std.testing.expect(testIntegerObject(rv.value, @as(i64, tes[1])));
            },
            else => |_| {},
        }
    }
}

test "test error handling" {
    const tests = .{
        .{ "5 + true;", "type mismatch: INTEGER_OBJ + BOOLEAN_OBJ" },
        .{ "5 + true; 5;", "type mismatch: INTEGER_OBJ + BOOLEAN_OBJ" },
        .{ "-true", "unknown operator: -BOOLEAN_OBJ" },
        .{ "true + false", "unknown operator: BOOLEAN_OBJ + BOOLEAN_OBJ" },
        .{ "5; true + false; 5", "unknown operator: BOOLEAN_OBJ + BOOLEAN_OBJ" },
        .{ "if (10 > 1) { true + false; } ", "unknown operator: BOOLEAN_OBJ + BOOLEAN_OBJ" },
        .{
            \\ if (10 > 1) {
            \\ if (10 > 1) {
            \\ return true + false;
            \\}
            \\ return 1;
            \\}
            ,
            "unknown operator: BOOLEAN_OBJ + BOOLEAN_OBJ",
        },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        const val = testEval(tes[0], allocator) catch |err| {
            std.debug.print("catch err -> {any}\n", .{err});
            return;
        };

        switch (val) {
            .errorMessage => |em| {
                try std.testing.expect(std.mem.eql(u8, em.message, tes[1]));
            },
            else => |_| {
                std.debug.print("tidak err {s} {s} \n", .{ tes[0], try val.inspect(allocator) });
            },
        }
    }
}

test "test let statements" {
    const tests = .{
        .{ "let a = 5; a;", 5 },
        .{ "let a = 5 * 5; a;", 25 },
        .{ "let a = 5; let b = a; b;", 5 },
        .{ "let a = 5; let b = a; let c = a + b + 5; c;", 15 },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        const vali = testEval(tes[0], allocator) catch |err| {
            std.debug.print("catch err -> {any}\n", .{err});
            return;
        };
        try std.testing.expect(testIntegerObject(vali, @as(i64, tes[1])));
    }
}

test "test function object" {
    const input = "fn(x) { x + 2; };";
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const evaluated = try testEval(input, allocator);
    if (evaluated != .function) {
        std.debug.print("object is not function\n", .{});
        return;
    }

    if (evaluated.function.parameters.items.len != 1) {
        std.debug.print("function has wrong parameters. got={d}\n", .{evaluated.function.parameters.items.len});
        return;
    }

    if (!std.mem.eql(u8, evaluated.function.parameters.items[0].string(), "x")) {
        std.debug.print("function has wrong parameters {s}\n", .{evaluated.function.parameters.items[0].string()});
        return;
    }

    const expectedBody = "(x + 2)";
    const evlBody = try evaluated.function.body.string(allocator);
    if (!std.mem.eql(u8, evlBody, expectedBody)) {
        std.debug.print("body is not {s} got={any}\n", .{ expectedBody, evlBody });
    }
}

test "test function application" {
    const tests = .{
        .{ "let identity = fn(x) { x; }; identity(5);", 5 },
        .{ "let identity = fn(x) { return x; }; identity(5);", 5 },
        .{ "let double = fn(x) { x * 2; }; double(5);", 10 },
        .{ "let add = fn(x, y) { x + y; }; add(5, 5);", 10 },
        .{ "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20 },
        .{ "fn(x) { x; }(5)", 5 },
    };
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    inline for (tests) |tes| {
        const vali = testEval(tes[0], allocator) catch |err| {
            std.debug.print("catch err -> {any}\n", .{err});
            return;
        };

        //std.debug.print("-_- {s} {s} \n", .{ tes[0], try vali.inspect(allocator) });
        try std.testing.expect(testIntegerObject(vali, @as(i64, tes[1])));
    }
}
