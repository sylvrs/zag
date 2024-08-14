const std = @import("std");

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
