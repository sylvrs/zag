const std = @import("std");
pub const Compiler = @import("compiler/Compiler.zig");
pub const Ast = std.zig.Ast;
pub const Vm = @import("Vm.zig");

/// Test script to parse
const ExampleCode =
    \\const a = 0xFF;
    \\const b = 0xFD;
    \\const c = a - b;
    \\var d = c ** 5;
    \\d *= 5;
;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const stderr = std.io.getStdErr().writer();
    const ally = arena.allocator();

    const preprocessed = try preprocess(ally, ExampleCode);

    var ast = try Ast.parse(ally, preprocessed, .zig);
    defer ast.deinit(ally);

    if (ast.errors.len > 0) {
        for (ast.errors) |err| {
            try ast.renderError(err, stderr);
            try stderr.writeByte('\n');
        }
        return;
    }

    var compiler = Compiler.init(ally, &ast);
    defer compiler.deinit();

    // result is bytecode
    var result = try compiler.compile();

    try result.dump(stderr);

    var vm = Vm.init(ally, result);
    defer vm.deinit();
    try vm.run();

    var global_iterator = vm.globals.iterator();
    while (global_iterator.next()) |global| {
        std.debug.print("global: {s} = {any}\n", .{ global.key_ptr.*, global.value_ptr.* });
    }
}

/// Preprocessses the source into a useable script format for the Zig parser
pub fn preprocess(ally: std.mem.Allocator, source: []const u8) ![:0]const u8 {
    var output = std.ArrayList(u8).init(ally);
    errdefer output.deinit();

    // todo: extract fns & check for name collision w/ source
    // random name gen?
    try output.appendSlice("fn run() void {");
    try output.appendSlice(source);
    try output.append('}');

    return output.toOwnedSliceSentinel(0);
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
