const std = @import("std");

pub const Value = @import("value.zig").Value;

pub const Opcode = enum(u8) {
    ret = 0,
    @"const" = 1,
    add = 2,

    get_global = 10,
    set_global = 11,

    pub fn from(byte: u8) Opcode {
        // todo: error checking
        return @enumFromInt(byte);
    }

    /// Returns the byte associated with the opcode
    pub fn raw(self: Opcode) u8 {
        return @intFromEnum(self);
    }
};

pub const Instruction = union(Opcode) {
    /// returns from the program or back to the return addr
    ret: void,
    /// an index into the constant pool
    @"const": u16,
    /// Pops two values on the stack, adds them, and pushes them back onto the stack
    add: void,
    /// Gets the global value of the variable named using the constant index
    get_global: u16,
    /// Sets the global value by the name using the constant index
    set_global: u16,
};

const Self = @This();

/// Raw instructions
instructions: []const u8,
/// The constants that make up the program
constant_pool: []const Value,

pub fn dump(self: Self, writer: anytype) !void {
    var index: usize = 0;

    // padding around the stream
    if (self.instructions.len > 0) try writer.writeByte('\n');
    while (index < self.instructions.len) {
        const opcode = Opcode.from(self.instructions[index]);
        try writer.print("{x:0>4} ", .{index});
        index += 1;

        switch (opcode) {
            .@"const" => {
                const const_idx = self.fetchInt(u16, index);
                index += 2;

                try writer.print("const [{d} = {any}]", .{ const_idx, self.constant_pool[const_idx] });
            },
            .add => try writer.writeAll("add"),
            .set_global, .get_global => |inner| {
                const const_idx = self.fetchInt(u16, index);
                index += 2;
                try writer.print("{s} [{any}]", .{ @tagName(inner), self.constant_pool[const_idx] });
            },
            inline else => |tag| try writer.writeAll(@tagName(tag)),
        }
        try writer.writeByte('\n');
    }
    try writer.writeByte('\n');
}

pub fn fetchInt(self: Self, comptime IntType: type, start: usize) IntType {
    const int_type_info = @typeInfo(IntType);
    if (int_type_info != .Int) @compileError("Int type should be an integer");

    const int_bits_size = int_type_info.Int.bits;
    if (int_bits_size % 8 != 0) @compileError("Int type should be a multiple of 8");

    const bytes = int_bits_size / 8;

    const fetched: []const u8 = self.instructions[start..(start + bytes)];
    return std.mem.readInt(IntType, fetched[0..bytes], .big);
}
