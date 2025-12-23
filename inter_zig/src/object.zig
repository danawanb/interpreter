const std = @import("std");

pub const ObjectTypes = enum {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
};

pub const Object = union(enum) {
    integer: *Integer,
    boolean: *Boolean,
    nullx: *Null,

    pub fn inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .integer => |int| return try int.inspect(allocator),
            .boolean => |bolx| return try bolx.inspect(allocator),
            .nullx => |nl| return try nl.inspect(allocator),
        };
    }

    pub fn type_obj(self: Object) ObjectTypes {
        return switch (self) {
            .integer => |_| return ObjectTypes.INTEGER_OBJ,
            .boolean => |_| return ObjectTypes.BOOLEAN_OBJ,
            .nullx => |_| return ObjectTypes.NULL_OBJ,
        };
    }
};

pub const Integer = struct {
    value: i64,

    fn inspect(self: *Integer, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    fn inspect(self: *Boolean, _: std.mem.Allocator) ![]const u8 {
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

    fn inspect(_: *Null, _: std.mem.Allocator) ![]const u8 {
        return "null";
    }

    //Type() in interpreter books
    fn type_obj() ObjectTypes {
        return ObjectTypes.NULL_OBJ;
    }
};
