const std = @import("std");
const Compiler = @import("compiler/Compiler.zig");
const Ast = std.zig.Ast;

/// Test script to parse
const ExampleCode =
    \\const a = 1_000;
    \\const b = 2;
    \\
    \\fn test_func(first: usize, second: usize) usize {
    \\      return first + second;
    \\}
;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const ally = arena.allocator();
    var ast = try Ast.parse(ally, ExampleCode, .zig);
    defer ast.deinit(ally);

    var compiler = Compiler.init(ally, &ast);
    defer compiler.deinit();

    // result is bytecode
    _ = try compiler.compile();
}
