const std = @import("std");

pub const Value = @import("value.zig").Value;

pub const Opcode = enum(u8) {
    ret = 1,
    @"const" = 2,
    pop = 3,
    true = 4,
    false = 5,
    null = 6,
    void = 7,
    // arithmetic
    add = 10,
    sub = 11,
    mul = 12,
    div = 13,
    pow = 14,
    mod = 15,
    // comparison
    gt = 16,
    gte = 17,
    lt = 18,
    lte = 19,
    eq = 20,
    not_eq = 21,

    // jumps
    jump_if_false = 30,
    jump_fwd = 31,
    jump_back = 32,
    // vars
    get_global = 40,
    set_global = 41,
    get_local = 42,
    set_local = 43,

    pub fn from(byte: u8) !Opcode {
        // todo: error checking
        return try std.meta.intToEnum(Opcode, byte);
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
    /// Pops a value from the stack
    pop: void,
    /// Represents a true value in the program
    true: void,
    /// Represents a false value in the program
    false: void,
    /// Represents a null value in the program
    null: void,
    /// Represents a void value in the program
    void: void,
    /// Pops two values on the stack, adds them, and pushes them back onto the stack
    add: void,
    /// Pops two values on the stack, subtracts them, and pushes them back onto the stack
    sub: void,
    /// Pops two values on the stack, multiplies them, and pushes them back onto the stack
    mul: void,
    /// Pops two values on the stack, divides them, and pushes them back onto the stack
    div: void,
    /// Pops two values on the stack, does a power op on them, and pushes them back onto the stack
    pow: void,
    /// Pops two values on the stack, does a mod op on them, and pushes them back onto the stack
    mod: void,
    /// Pops two values on the stack, check if lhs > rhs, and pushes the result (true or false)
    gt: void,
    /// Pops two values on the stack, check if lhs < rhs, and pushes the result (true or false)
    lt: void,
    /// Pops two values on the stack, check if lhs >= rhs, and pushes the result (true or false)
    gte: void,
    /// Pops two values on the stack, check if lhs <= rhs, and pushes the result (true or false)
    lte: void,
    /// Pops two values on the stack, check if lhs == rhs, and pushes the result (true or false)
    eq: void,
    /// Pops two values on the stack, check if lhs != rhs, and pushes the result (true or false)
    not_eq: void,
    /// Jumps forward a relative amount if the value popped is false
    jump_if_false: u16,
    /// Jumps forwards a relative amount in the program
    jump_fwd: u16,
    /// Jumps backwards in the program
    jump_back: u16,
    /// Gets the global value of the variable named using the constant index
    get_global: u16,
    /// Sets the global value by the name using the constant index
    set_global: u16,
    /// Gets the local value based on the stack offset provided
    get_local: u16,
    /// Pops a value from the stack and sets it as the value of the var at the stack offset
    set_local: u16,
};

const Self = @This();

/// Raw instructions
instructions: []const u8,
/// The constants that make up the program
constant_pool: []const Value,

pub fn dump(self: Self) !void {
    var index: usize = 0;

    std.log.info(" -- Bytecode --", .{});
    while (index < self.instructions.len) {
        const raw = self.instructions[index];
        const opcode = Opcode.from(raw) catch |err| {
            std.log.err("encountered unexpected opcode 0x{x:0>2} at index {d}", .{ raw, index });
            return err;
        };
        std.log.info("{x:0>4} ", .{index});
        index += 1;

        switch (opcode) {
            .@"const" => {
                const const_idx = self.fetchInt(u16, index);
                index += 2;

                std.log.info("const [#{d} => {any}]", .{ const_idx, self.constant_pool[const_idx] });
            },
            .jump_if_false, .jump_fwd, .jump_back => {
                const amount = self.fetchInt(u16, index);
                index += 2;

                std.log.info("{s} [{x:0>4}]", .{ @tagName(opcode), amount });
            },
            .set_global, .get_global, .set_local, .get_local => |inner| {
                const const_idx = self.fetchInt(u16, index);
                index += 2;
                std.log.info("{s} [{any}]", .{ @tagName(inner), self.constant_pool[const_idx] });
            },
            inline else => |tag| std.log.info("{s}", .{@tagName(tag)}),
        }
    }
    std.log.info("----------------", .{});
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
