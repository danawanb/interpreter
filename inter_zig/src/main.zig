const std = @import("std");
const inter_zig = @import("inter_zig");
const repl = @import("repl.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const evaluator = @import("eval.zig");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    //try inter_zig.bufferedPrint();

    try stdout.print("Hello This is the Monkey programming language! \n", .{});
    try stdout.print("Interactive mode - press Ctrl+C to exit\n", .{});

    const gpa = std.heap.page_allocator;
    const stdin = std.io.getStdIn().reader();
    var scanner = try repl.LineScanner(@TypeOf(stdin)).init(gpa, stdin, 256);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    while (true) {
        std.debug.print("> ", .{});
        const line = try scanner.nextLine();

        if (line == null) break;

        var lexerx = lexer.new(line.?);

        const p = try parser.new(&lexerx, allocator);
        const program = try p.parseProgram(allocator);

        if (p.errors.items.len != 0) {
            try printParserErrors(stdout, p.errors.items);
            continue;
        }
        //const strProg = try program.string(allocator);
        //try stdout.print("{s}\n", .{strProg});
        const evaluated = try evaluator.evalProgram(program, allocator);
        try stdout.print("{s}\n", .{try evaluated.inspect(allocator)});
    }
}

fn printParserErrors(out: anytype, errors: []const []const u8) !void {
    try out.print(" parser errors:\n", .{});
    for (errors) |err| {
        try out.print("\t{s}", .{err});
    }
}
