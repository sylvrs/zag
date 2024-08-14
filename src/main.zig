const std = @import("std");
const clarp = @import("clarp");

const parser = @import("parser.zig");
pub const Compiler = @import("compiler/Compiler.zig");
pub const Ast = std.zig.Ast;
pub const Vm = @import("Vm.zig");

/// Log options
pub const std_options = .{
    .log_level = .info,
};

/// Test script to parse
const ExampleCode = @embedFile("./test_code.zig");

const ArgParser = clarp.Parser(struct {
    source: bool,
    bytecode: bool,
    @"print-globals": bool,

    pub const clarp_options = clarp.Options(@This()){
        .derive_short_names = true,
    };
}, .{});

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const stderr = std.io.getStdErr().writer();
    const ally = arena.allocator();

    const args = try std.process.argsAlloc(ally);
    const parsed = ArgParser.parse(args, .{
        .err_writer = std.io.getStdErr().writer().any(),
    }) catch |e| switch (e) {
        error.HelpShown => return,
        else => return e,
    };

    if (parsed.root.source) {
        std.log.info("Source Code:", .{});
        var line_iterator = std.mem.splitScalar(u8, ExampleCode, '\n');
        while (line_iterator.next()) |line| {
            // don't print empty lines
            if (line.len == 0) continue;
            std.log.info("\t{s}", .{line});
        }
    }

    const preprocessed = try parser.preprocess(ally, ExampleCode);
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
    const result = try compiler.compile();

    if (parsed.root.bytecode) {
        try result.dump();
    }

    var vm = Vm.init(ally, result);
    defer vm.deinit();
    try vm.run();

    if (parsed.root.@"print-globals") {
        var global_iterator = vm.globals.iterator();

        std.log.info("Globals after execution:", .{});
        while (global_iterator.next()) |global| {
            std.log.info("\t | {s} = {any}", .{ global.key_ptr.*, global.value_ptr.* });
        }
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
