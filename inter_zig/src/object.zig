const std = @import("std");
const ast = @import("ast.zig");

pub const BuiltinFn = *const fn (args: []Object, allocator: std.mem.Allocator) anyerror!Object;

pub const ObjectTypes = enum {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_VALUE_OBJ,
    ERROR_OBJ,
    FUNCTION_OBJ,
    STRING_OBJ,
    BUILTIN_OBJ,
};

pub const Object = union(enum) {
    integer: *Integer,
    boolean: *Boolean,
    nullx: *Null,
    returnValue: *ReturnValue,
    errorMessage: *Error,
    function: *Function,
    string: *String,
    builtin: *Builtin,

    pub fn inspect(self: Object, allocator: std.mem.Allocator) anyerror![]const u8 {
        return switch (self) {
            .integer => |int| return try int.inspect(allocator),
            .boolean => |bolx| return try bolx.inspect(allocator),
            .nullx => |nl| return try nl.inspect(allocator),
            .returnValue => |rv| return try rv.inspect(allocator),
            .errorMessage => |em| return try em.inspect(allocator),
            .function => |fun| return try fun.inspect(allocator),
            .string => |str| return str.inspect(),
            .builtin => |blt| return blt.inspect(),
        };
    }

    pub fn type_obj(self: Object) ObjectTypes {
        return switch (self) {
            .integer => |_| return ObjectTypes.INTEGER_OBJ,
            .boolean => |_| return ObjectTypes.BOOLEAN_OBJ,
            .nullx => |_| return ObjectTypes.NULL_OBJ,
            .returnValue => |_| return ObjectTypes.RETURN_VALUE_OBJ,
            .errorMessage => |_| return ObjectTypes.ERROR_OBJ,
            .function => |_| return ObjectTypes.FUNCTION_OBJ,
            .string => |_| return ObjectTypes.STRING_OBJ,
            .builtin => |_| return ObjectTypes.BUILTIN_OBJ,
        };
    }
};

pub const Integer = struct {
    value: i64,

    fn inspect(self: *Integer, allocator: std.mem.Allocator) anyerror![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    fn inspect(self: *Boolean, _: std.mem.Allocator) anyerror![]const u8 {
        if (self.value == true) {
            return "true";
        } else {
            return "false";
        }
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    value: void,

    fn inspect(_: *Null, _: std.mem.Allocator) anyerror![]const u8 {
        return "null";
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.NULL_OBJ;
    }
};

pub const ReturnValue = struct {
    value: Object,

    fn inspect(self: *ReturnValue, allocator: std.mem.Allocator) anyerror![]const u8 {
        return try self.value.inspect(allocator);
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.RETURN_VALUE_OBJ;
    }
};

pub const Error = struct {
    message: []const u8,

    fn inspect(self: *Error, allocator: std.mem.Allocator) anyerror![]const u8 {
        return try std.fmt.allocPrint(allocator, "{s}", .{self.message});
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.ERROR_OBJ;
    }
};

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,

    pub fn get(self: *Environment, name: []const u8) ?Object {
        var obj = self.store.get(name);

        if (obj == null) {
            if (self.outer != null) {
                obj = self.outer.?.get(name);
            }
        }

        return obj;
    }

    pub fn set(self: *Environment, name: []const u8, val: Object) !Object {
        try self.store.put(name, val);
        return val;
    }
};

pub fn newEnvironment(allocator: std.mem.Allocator) !*Environment {
    const s = std.StringHashMap(Object).init(allocator);

    const env = try allocator.create(Environment);
    env.* = Environment{
        .store = s,
        .outer = null,
    };

    return env;
}

pub fn newEnclosedEnvironment(outer: *Environment, allocator: std.mem.Allocator) !*Environment {
    var env = try newEnvironment(allocator);
    env.outer = outer;

    return env;
}

pub const Function = struct {
    parameters: std.ArrayList(*ast.Identifier),
    body: *ast.BlockStatement,
    env: *Environment,

    fn inspect(self: *Function, allocator: std.mem.Allocator) anyerror![]const u8 {
        var out = std.ArrayList(u8).init(allocator);

        var params = std.ArrayList([]const u8).init(allocator);

        for (self.parameters.items) |item| {
            const parStr = item.string();
            try params.append(parStr);
        }

        try out.appendSlice("fn");
        try out.appendSlice("(");
        try out.appendSlice(try ast.stringsJoin(params, ", ", allocator));
        try out.appendSlice(") {\n");
        try out.appendSlice(try self.body.string(allocator));
        try out.appendSlice("\n}");

        const saved = try allocator.dupe(u8, out.items);
        return saved;
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.FUNCTION_OBJ;
    }
};

pub const String = struct {
    value: []const u8,

    fn inspect(self: *String) []const u8 {
        return self.value;
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.STRING_OBJ;
    }
};

pub const Builtin = struct {
    name: []const u8,
    fun: BuiltinFn,

    fn inspect(_: *Builtin) []const u8 {
        return "builtin function";
    }
    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.STRING_OBJ;
    }
};
