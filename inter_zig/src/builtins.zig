const std = @import("std");
const object = @import("object.zig");

const builtins = [_]object.Builtin{
    .{ .name = "len", .fun = &builtinslen },
};

fn builtinslen(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len != 1) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=1\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        switch (args[0]) {
            .string => |str| {
                const intObj = try allocator.create(object.Integer);
                const valU: i64 = @intCast(str.value.len);
                intObj.* = object.Integer{ .value = valU };
                return object.Object{ .integer = intObj };
            },

            else => |_| {
                const errMsg = try allocator.create(object.Error);
                const msg = try std.fmt.allocPrint(allocator, "argument to `len` not supported \n", .{});
                errMsg.* = object.Error{ .message = msg };
                return object.Object{ .errorMessage = errMsg };
            },
        }
    }
}

pub fn getBuiltin(name: []const u8) ?object.BuiltinFn {
    inline for (builtins) |b| {
        if (std.mem.eql(u8, b.name, name)) {
            return b.fun;
        }
    }
    return null;
}
