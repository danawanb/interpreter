const std = @import("std");

pub const ObjectTypes = enum {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
    RETURN_VALUE_OBJ,
    ERROR_OBJ,
};

pub const Object = union(enum) {
    integer: *Integer,
    boolean: *Boolean,
    nullx: *Null,
    returnValue: *ReturnValue,
    errorMessage: *Error,
    pub fn inspect(self: Object, allocator: std.mem.Allocator) anyerror![]const u8 {
        return switch (self) {
            .integer => |int| return try int.inspect(allocator),
            .boolean => |bolx| return try bolx.inspect(allocator),
            .nullx => |nl| return try nl.inspect(allocator),
            .returnValue => |rv| return try rv.inspect(allocator),
            .errorMessage => |em| return try em.inspect(allocator),
        };
    }

    pub fn type_obj(self: Object) ObjectTypes {
        return switch (self) {
            .integer => |_| return ObjectTypes.INTEGER_OBJ,
            .boolean => |_| return ObjectTypes.BOOLEAN_OBJ,
            .nullx => |_| return ObjectTypes.NULL_OBJ,
            .returnValue => |_| return ObjectTypes.RETURN_VALUE_OBJ,
            .errorMessage => |_| return ObjectTypes.ERROR_OBJ,
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
