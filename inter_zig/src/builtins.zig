const std = @import("std");
const object = @import("object.zig");

const builtins = [_]object.Builtin{
    .{ .name = "len", .fun = &builtinslen },
    .{ .name = "first", .fun = &builtinsfirst },
    .{ .name = "last", .fun = &builtinslast },
    .{ .name = "rest", .fun = &builtinsrest },
    .{ .name = "push", .fun = &builtinspush },
    .{ .name = "puts", .fun = &builtinsputs },
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
            .array => |arr| {
                const intObj = try allocator.create(object.Integer);
                const valU: i64 = @intCast(arr.elements.items.len);
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

fn builtinsfirst(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len != 1) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=1\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        switch (args[0]) {
            .array => |arr| {
                if (arr.elements.items.len > 0) {
                    return arr.elements.items[0].*;
                } else {
                    const nullObj = try allocator.create(object.Null);
                    nullObj.* = object.Null{ .value = {} };
                    return object.Object{ .nullx = nullObj };
                }
            },
            else => |_| {
                const errMsg = try allocator.create(object.Error);
                const msg = try std.fmt.allocPrint(allocator, "argument to `first` not supported \n", .{});
                errMsg.* = object.Error{ .message = msg };
                return object.Object{ .errorMessage = errMsg };
            },
        }
    }
}

fn builtinslast(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len != 1) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=1\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        switch (args[0]) {
            .array => |arr| {
                if (arr.elements.items.len > 0) {
                    const length = arr.elements.items.len;
                    return arr.elements.items[length - 1].*;
                } else {
                    const nullObj = try allocator.create(object.Null);
                    nullObj.* = object.Null{ .value = {} };
                    return object.Object{ .nullx = nullObj };
                }
            },
            else => |_| {
                const errMsg = try allocator.create(object.Error);
                const msg = try std.fmt.allocPrint(allocator, "argument to `last` not supported \n", .{});
                errMsg.* = object.Error{ .message = msg };
                return object.Object{ .errorMessage = errMsg };
            },
        }
    }
}
fn builtinsrest(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len != 1) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=1\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        switch (args[0]) {
            .array => |arr| {
                if (arr.elements.items.len > 0) {
                    const halfLen = arr.elements.items.len / 2;
                    var new_list: std.ArrayList(*object.Object) = .{};
                    try new_list.appendSlice(allocator, arr.elements.items[0..halfLen]);

                    const arrObj = try allocator.create(object.Array);
                    arrObj.* = object.Array{ .elements = new_list };
                    return object.Object{ .array = arrObj };
                } else {
                    const nullObj = try allocator.create(object.Null);
                    nullObj.* = object.Null{ .value = {} };
                    return object.Object{ .nullx = nullObj };
                }
            },
            else => |_| {
                const errMsg = try allocator.create(object.Error);
                const msg = try std.fmt.allocPrint(allocator, "argument to `rest` not supported \n", .{});
                errMsg.* = object.Error{ .message = msg };
                return object.Object{ .errorMessage = errMsg };
            },
        }
    }
}

fn builtinspush(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len != 2) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=2\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        switch (args[0]) {
            .array => |arr| {
                var new_list: std.ArrayList(*object.Object) = .{};
                try new_list.appendSlice(allocator, arr.elements.items);

                //heap
                const newElement = try allocator.create(object.Object);
                newElement.* = args[1];
                try new_list.append(allocator, newElement);

                const arrObj = try allocator.create(object.Array);
                arrObj.* = object.Array{ .elements = new_list };
                return object.Object{ .array = arrObj };
            },
            else => |_| {
                const errMsg = try allocator.create(object.Error);
                const msg = try std.fmt.allocPrint(allocator, "argument to `rest` not supported \n", .{});
                errMsg.* = object.Error{ .message = msg };
                return object.Object{ .errorMessage = errMsg };
            },
        }
    }
}

fn builtinsputs(args: []object.Object, allocator: std.mem.Allocator) !object.Object {
    if (args.len == 0) {
        const errMsg = try allocator.create(object.Error);
        const msg = try std.fmt.allocPrint(allocator, "wrong number of arguments. got {d} want=1\n", .{args.len});
        errMsg.* = object.Error{ .message = msg };
        return object.Object{ .errorMessage = errMsg };
    } else {
        for (args) |arg| {
            std.debug.print("{s} \n", .{try arg.inspect(allocator)});
        }

        const nullObj = try allocator.create(object.Null);
        nullObj.* = object.Null{ .value = {} };
        return object.Object{ .nullx = nullObj };
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
