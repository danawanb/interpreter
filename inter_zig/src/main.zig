const std = @import("std");
const inter_zig = @import("inter_zig");
const repl = @import("repl.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    //try inter_zig.bufferedPrint();

    try stdout.print("Hello This is the Monkey programming language! \n", .{});
    try stdout.print("Interactive mode - press Ctrl+C to exit\n", .{});

    const gpa = std.heap.page_allocator;
    const stdin = std.io.getStdIn().reader();
    var scanner = try repl.LineScanner(@TypeOf(stdin)).init(gpa, stdin, 256);

    while (true) {
        std.debug.print("> ", .{});
        const line = try scanner.nextLine();

        if (line == null) break;

        var lexerx = lexer.new(line.?);
        while (true) {
            const tok = lexerx.nextToken();
            if (tok.type == token.TokenTypes.EOF) break;

            const tokenStr = @tagName(tok.type);
            std.debug.print("[Type:{s} Literal:{s}] \n", .{ tok.literal, tokenStr });
        }
    }
}
