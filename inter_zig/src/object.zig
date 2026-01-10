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
    ARRAY_OBJ,
    HASH_OBJ,
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
    array: *Array,
    hash: *Hash,
    hashable: *Hashable,

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
            .array => |arr| return try arr.inspect(allocator),
            .hash => |hs| return try hs.inspect(allocator),
            .hashable => |ha| return try ha.inspect(allocator),
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
            .array => |_| return ObjectTypes.ARRAY_OBJ,
            .hash => |_| return ObjectTypes.HASH_OBJ,
            .hashable => |_| return ObjectTypes.HASH_OBJ,
        };
    }
};

pub const Integer = struct {
    value: i64,

    fn inspect(self: *Integer, allocator: std.mem.Allocator) anyerror![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    //Type() in interpreter books
    fn type_obj(_: *Integer) ObjectTypes {
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
    fn type_obj(_: *Boolean) ObjectTypes {
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
        var out: std.ArrayList(u8) = .{};

        var params: std.ArrayList([]const u8) = .{};

        for (self.parameters.items) |item| {
            const parStr = item.string();
            try params.append(allocator, parStr);
        }

        try out.appendSlice(allocator, "fn");
        try out.appendSlice(allocator, "(");
        try out.appendSlice(allocator, try ast.stringsJoin(params, ", ", allocator));
        try out.appendSlice(allocator, ") {\n");
        try out.appendSlice(allocator, try self.body.string(allocator));
        try out.appendSlice(allocator, "\n}");

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
    fn type_obj(_: *String) ObjectTypes {
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

pub const Array = struct {
    elements: std.ArrayList(*Object),

    fn inspect(self: *Array, allocator: std.mem.Allocator) anyerror![]const u8 {
        var out: std.ArrayList(u8) = .{};

        var elems: std.ArrayList([]const u8) = .{};

        for (self.elements.items) |item| {
            const parStr = try item.inspect(allocator);
            try elems.append(allocator, parStr);
        }

        try out.appendSlice(allocator, "[");
        try out.appendSlice(allocator, try ast.stringsJoin(elems, ", ", allocator));
        try out.appendSlice(allocator, "]");
        try out.appendSlice(allocator, "\n");

        const saved = try allocator.dupe(u8, out.items);
        return saved;
    }
    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.ARRAY_OBJ;
    }
};

pub const HashKey = struct {
    type: ObjectTypes,
    value: ?u64,
};
//type HashKey struct in golang
pub const Hashable = union(enum) {
    //type: ObjectTypes,
    //value: ?u64,
    integer: *Integer,
    boolean: *Boolean,
    string: *String,

    pub fn inspect(self: Hashable, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => |int| return try int.inspect(allocator),
            .boolean => |bolx| return try bolx.inspect(allocator),
            .string => |str| return str.inspect(),
        };
    }
    pub fn hashkeyval(self: Hashable) HashKey {
        return switch (self) {
            .boolean => |bl| {
                var value: u64 = 0;
                if (bl.value == true) {
                    value = 1;
                } else {
                    value = 0;
                }
                return HashKey{
                    .type = bl.type_obj(),
                    .value = value,
                };
            },
            .string => |str| {
                var hh = std.hash.Fnv1a_64.init();
                hh.update(str.value);
                return HashKey{
                    .type = str.type_obj(),
                    .value = hh.final(),
                };
            },
            .integer => |int| {
                const hash_val: u64 = @bitCast(int.value);
                return HashKey{
                    .type = int.type_obj(),
                    .value = hash_val,
                };
            },
        };
    }
};

pub const HashPair = struct {
    key: Object,
    value: Object,
};

pub const Hash = struct {
    pairs: std.AutoHashMap(HashKey, HashPair),

    pub fn type_obj() ObjectTypes {
        return ObjectTypes.HASH_OBJ;
    }

    pub fn inspect(self: *Hash, allocator: std.mem.Allocator) anyerror![]const u8 {
        var out: std.ArrayList(u8) = .{};

        var pairs: std.ArrayList([]const u8) = .{};
        var iterator = self.pairs.iterator();

        while (iterator.next()) |pair| {
            const keyType = @tagName(pair.key_ptr.type);
            const valK = try pair.value_ptr.value.inspect(allocator);
            const pairstr = try std.fmt.allocPrint(
                allocator,
                "{s}: {s}",
                .{
                    keyType,
                    valK,
                },
            );
            try pairs.append(allocator, pairstr);
        }

        try out.appendSlice(allocator, "{");
        try out.appendSlice(allocator, try ast.stringsJoin(pairs, ", ", allocator));
        try out.appendSlice(allocator, "}");
        try out.appendSlice(allocator, "\n");

        const saved = try allocator.dupe(u8, out.items);
        return saved;
    }
};

test "string hashkey" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const hello = try allocator.create(String);
    hello.* = String{ .value = "Hello World" };
    const hellou = Hashable{ .string = hello };

    const hello2 = try allocator.create(String);
    hello2.* = String{ .value = "Hello World" };
    const hellou2 = Hashable{ .string = hello2 };

    const diff = try allocator.create(String);
    diff.* = String{ .value = "Jhonny Silverhand" };

    const diffu = Hashable{ .string = diff };

    if (hellou.hashkeyval().value.? != hellou2.hashkeyval().value.?) {
        std.debug.print("strig with same content have diff hash keys\n", .{});
    }

    if (hellou.hashkeyval().value.? == diffu.hashkeyval().value.?) {
        std.debug.print("strig with same content have same hash keys\n", .{});
    }
}
