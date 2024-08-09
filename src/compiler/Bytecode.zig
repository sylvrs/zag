const Value = @import("value.zig").Value;

const Instruction = union(enum) {
    /// returns from the program or back to the return addr
    ret: void,
};

/// Raw instructions
instructions: []const u8,
/// The constants that make up the program
constant_pool: []const Value,
