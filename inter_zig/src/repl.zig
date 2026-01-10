const std = @import("std");

pub fn LineScanner(comptime ReaderType: type) type {
    return struct {
        reader: ReaderType,
        buf: std.ArrayList(u8),

        pub fn init(allocator: std.mem.Allocator, reader: ReaderType, initial_size: usize) !@This() {
            return .{
                .reader = reader,
                .buf = try std.ArrayList(u8).initCapacity(allocator, initial_size),
            };
        }

        pub fn next(self: *@This()) !?[]const u8 {
            self.buf.clearRetainingCapacity();

            var br = std.io.bufferedReader(self.reader);
            var r = br.reader();

            while (true) {
                const n = try r.readByteOrEof();
                if (n == null) break;
                const byte = n.?;
                if (byte == '\n') break;
                try self.buf.append(byte);
            }

            if (self.buf.items.len == 0) return null;
            return self.buf.items;
        }

        pub fn nextLine(self: *@This()) !?[]const u8 {
            return try self.reader.takeDelimiter('\n');
        }
    };
}
