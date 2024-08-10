const std = @import("std");
const Bytecode = @import("compiler/Bytecode.zig");
const Value = Bytecode.Value;
const Opcode = Bytecode.Opcode;

const Stack = struct {
    items: std.ArrayList(Value),

    pub fn init(ally: std.mem.Allocator) Stack {
        return .{ .items = std.ArrayList(Value).init(ally) };
    }

    pub fn deinit(self: *Stack) void {
        self.items.deinit();
    }

    pub fn push(self: *Stack, value: Value) !void {
        try self.items.append(value);
    }

    pub fn pop(self: *Stack) !Value {
        if (self.items.items.len == 0) return error.StackUnderflow;
        return self.items.pop();
    }
};

const Self = @This();

ally: std.mem.Allocator,
program: Bytecode,
program_counter: usize = 0,
stack: Stack,
/// A list used to store the global variables
globals: std.StringArrayHashMap(Value),

pub fn init(ally: std.mem.Allocator, program: Bytecode) Self {
    return .{
        .ally = ally,
        .program = program,
        .stack = Stack.init(ally),
        .globals = std.StringArrayHashMap(Value).init(ally),
    };
}

pub fn deinit(self: *Self) void {
    self.stack.deinit();
    self.globals.deinit();
}

/// Fetch from the instructions
pub fn fetchSlice(self: *Self, comptime count: usize) []const u8 {
    const slice = self.program.instructions[self.program_counter..(self.program_counter + count)];
    self.program_counter += count;
    return slice;
}

pub fn fetchByte(self: *Self) u8 {
    const byte = self.program.instructions[self.program_counter];
    self.program_counter += 1;
    return byte;
}

pub fn fetchInt(self: *Self, comptime IntType: type) IntType {
    const int_type_info = @typeInfo(IntType);
    if (int_type_info != .Int) @compileError("Int type should be an integer");

    const int_bits_size = int_type_info.Int.bits;
    if (int_bits_size % 8 != 0) @compileError("Int type should be a multiple of 8");

    const bytes = int_bits_size / 8;

    const fetched = self.fetchSlice(bytes);

    return std.mem.readInt(IntType, fetched[0..bytes], .big);
}

pub fn fetchConstant(self: *Self) Value {
    const idx = self.fetchInt(u16);
    return self.program.constant_pool[idx];
}

pub fn run(self: *Self) !void {
    while (self.program_counter < self.program.instructions.len) {
        const raw_instr = self.fetchByte();
        const opcode = Opcode.from(raw_instr);
        try self.execute(opcode);
    }
}

/// Executes a single instruction given the opcode
pub fn execute(self: *Self, opcode: Opcode) !void {
    switch (opcode) {
        .ret => {
            // kind of hacky? maintain state instead?
            self.program_counter = self.program.instructions.len;
        },
        .@"const" => {
            const value = self.fetchConstant();
            try self.stack.push(value);
        },
        .add => {
            const first = try self.stack.pop();
            const second = try self.stack.pop();

            const result = try first.add(second);
            try self.stack.push(result);
        },
        .get_global => {
            const name_value = self.fetchConstant();
            const value = self.globals.get(name_value.identifier) orelse return error.UndefinedGlobal;

            try self.stack.push(value);
        },
        .set_global => {
            const name_value = self.fetchConstant();
            const new_value = try self.stack.pop();

            try self.globals.put(name_value.identifier, new_value);
        },
        // inline else => |tag| std.debug.panic("{s} is not implemented", .{@tagName(tag)}),
    }
}

test "ensure fetchInt works" {
    const input = Bytecode{
        .instructions = &.{
            0x00,
            0x02,
        },
        .constant_pool = &.{},
    };

    var vm = Self.init(std.testing.allocator, input);
    defer vm.deinit();

    const value = vm.fetchInt(u16);

    try std.testing.expectEqual(2, value);
}

test "execute simple add bytecode" {
    const input = Bytecode{
        .instructions = &.{
            // const 0
            Opcode.@"const".raw(),
            // u16
            0x00,
            0x00,
            // const 1
            Opcode.@"const".raw(),
            0x00,
            0x01,
            // add
            Opcode.add.raw(),
            Opcode.ret.raw(),
        },
        .constant_pool = &.{
            .{ .float = 1.1 },
            .{ .int = 110 },
        },
    };

    const expected_value = Value{ .float = 111.1 };

    // small vm
    var vm = Self.init(std.testing.allocator, input);
    defer vm.deinit();

    try vm.run();

    const popped = try vm.stack.pop();
    try std.testing.expectEqual(expected_value, popped);
}
