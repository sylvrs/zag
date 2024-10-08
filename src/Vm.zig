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

    pub fn get(self: *Stack, offset: u16) !Value {
        if (offset >= self.items.items.len) return error.StackOverflow;
        return self.items.items[offset];
    }

    pub fn set(self: *Stack, offset: u16, new_value: Value) !void {
        if (offset >= self.items.items.len) return error.StackOverflow;
        self.items.items[offset] = new_value;
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
        const opcode = Opcode.from(raw_instr) catch |err| {
            std.log.err("Encountered invalid opcode {x:0>2} at PC {x:0>4}", .{ raw_instr, self.program_counter - 1 });
            return err;
        };
        try self.execute(opcode);
    }

    if (self.stack.items.items.len > 0) {
        std.log.warn("Stack was not empty after program ended:", .{});
        for (self.stack.items.items, 0..) |item, idx| {
            std.log.warn("\t | Stack #{d}: {any}", .{ idx, item });
        }
    }
}

pub fn pop(self: *Self) !Value {
    return self.stack.pop() catch |err| {
        std.log.err("unable to pop stack at PC {x:0>4}", .{self.program_counter});
        return err;
    };
}

/// Executes a single instruction given the opcode
pub fn execute(self: *Self, opcode: Opcode) !void {
    std.log.debug("[{x:0>4}] {s}", .{ self.program_counter - 1, @tagName(opcode) });
    switch (opcode) {
        .ret => {
            // kind of hacky? maintain state instead?
            self.program_counter = self.program.instructions.len;
        },
        .@"const" => {
            const value = self.fetchConstant();
            try self.stack.push(value);
        },
        .pop => _ = try self.pop(),
        .true, .false => try self.stack.push(if (opcode == .true) Value.True else Value.False),
        .null => try self.stack.push(Value.Null),
        .void => try self.stack.push(Value.Void),
        .add, .sub, .mul, .div, .pow, .mod => {
            const first = try self.pop();
            const second = try self.pop();

            //            try @field(first, @tagName(opcode));
            const result = switch (opcode) {
                .add => if (first == .string) try first.concat(self, second) else try first.add(second),
                .sub => try first.sub(second),
                .mul => try first.mul(second),
                .div => try first.div(second),
                .pow => try first.pow(second),
                .mod => try first.mod(second),
                inline else => unreachable,
            };
            try self.stack.push(result);
        },
        .gt, .gte, .lt, .lte, .eq, .not_eq => {
            const first = try self.pop();
            const second = try self.pop();

            const result = switch (opcode) {
                .gt => boolToValue(try first.greaterThan(second)),
                .lt => boolToValue(try first.lessThan(second)),
                .gte => boolToValue(try first.greaterThanOrEqual(second)),
                .lte => boolToValue(try first.lessThanOrEqual(second)),
                .eq => boolToValue(first.eql(second)),
                .not_eq => boolToValue(!first.eql(second)),
                inline else => std.debug.panic("comparison op {s} not implemented", .{@tagName(opcode)}),
            };

            try self.stack.push(result);
        },
        .jump_if_false => {
            const addr = self.fetchInt(u16);

            const condition = try self.pop();
            if (condition != .boolean) std.debug.panic("expected boolean, but received {s}", .{@tagName(condition)});

            if (!condition.boolean) self.program_counter = addr;
        },
        .jump_fwd => {
            const addr = self.fetchInt(u16);
            self.program_counter += addr;
        },
        .jump_back => {
            const addr = self.fetchInt(u16);
            self.program_counter = addr;
        },
        .get_global => {
            const name_value = self.fetchConstant();
            const value = self.globals.get(name_value.identifier) orelse return error.UndefinedGlobal;

            try self.stack.push(value);
        },
        .set_global => {
            const name_value = self.fetchConstant();
            const new_value = try self.pop();

            try self.globals.put(name_value.identifier, new_value);
        },
        .get_local => {
            const stack_offset = self.fetchInt(u16);
            const value = try self.stack.get(stack_offset);

            try self.stack.push(value);
        },
        .set_local => {
            const new_value = try self.pop();
            const stack_offset = self.fetchInt(u16);
            try self.stack.set(stack_offset, new_value);
        },
        // inline else => |tag| std.debug.panic("{s} is not implemented", .{@tagName(tag)}),
    }
}

pub fn allocString(self: *Self, value: []const u8) !Value {
    const duped = try self.ally.dupe(u8, value);
    errdefer self.ally.free(duped);

    // todo: track string

    return Value{ .string = duped };
}

inline fn boolToValue(value: bool) Value {
    return if (value) Value.True else Value.False;
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
