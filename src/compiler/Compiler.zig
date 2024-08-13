// dichael mouglas
const std = @import("std");
const Ast = std.zig.Ast;
const Bytecode = @import("Bytecode.zig");

const Self = @This();

/// Represents an index in the constant pool
const ConstantIndex = u16;
/// Represents the max offset that a jump instruction can do
const MaxJumpLength: u16 = std.math.maxInt(u16);

const Variable = struct {
    /// TODO: we could potentially make this a tagged union and provide the scope index for local variables
    const Scope = enum { global, local };
    /// Whether the variable is constant or
    const Mutability = enum { @"const", @"var" };

    scope: Scope,
    mut: Mutability,
    name_index: ConstantIndex,
    /// The scope depth the variable exists in
    depth: usize = 0,
    /// For local variables, this is how far deep the variable is on the stack
    stack_offset: u16 = 0,
};

/// Context represents scope, depth, etc.
const Context = struct {
    current_scope_depth: u32 = 0,
    // const a = 1;
    // const b = 2;
    //
    // {
    //   const a = 1; // not valid
    //   const result = a + b;
    // }
    globals: std.AutoArrayHashMap(ConstantIndex, void),
    /// Mapping of local variables to their variable structs
    locals: std.AutoArrayHashMap(ConstantIndex, Variable),

    pub fn init(ally: std.mem.Allocator) Context {
        return .{ .globals = std.AutoArrayHashMap(ConstantIndex, void).init(ally), .locals = std.AutoArrayHashMap(ConstantIndex, Variable).init(ally) };
    }

    pub fn deinit(self: *Context) void {
        self.globals.deinit();
        self.locals.deinit();
    }

    pub fn hasGlobal(self: *Context, name_idx: ConstantIndex) bool {
        return self.globals.contains(name_idx);
    }

    pub fn addGlobal(self: *Context, name_idx: ConstantIndex) !void {
        try self.globals.put(name_idx, {});
    }

    pub fn hasLocal(self: *Context, name_idx: ConstantIndex) bool {
        return self.locals.contains(name_idx);
    }

    pub fn addLocal(self: *Context, name_idx: ConstantIndex, variable: Variable) !void {
        try self.locals.put(name_idx, variable);
    }

    pub fn getLocal(self: *Context, name_idx: ConstantIndex) ?Variable {
        return self.locals.get(name_idx);
    }

    pub fn startScope(self: *Context) void {
        self.current_scope_depth += 1;
    }

    pub fn endScope(self: *Context) void {
        self.current_scope_depth -= 1;

        var iterator = self.locals.iterator();
        while (iterator.next()) |entry| {
            const local = entry.value_ptr.*;
            // todo: stack-based?
            if (local.depth > self.current_scope_depth) {
                _ = self.locals.orderedRemove(entry.key_ptr.*);
            }
        }
    }

    pub fn getStackOffset(self: *Context) u16 {
        return @intCast(self.locals.count());
    }
};

const Keywords = std.StaticStringMap(Bytecode.Opcode).initComptime(.{
    .{ "true", .true },
    .{ "false", .false },
    .{ "null", .null },
});

// constant pool:
// 0: Value{ .string = "name" }
// 1: Value{ .int = 1 }
// 2: Value{ .float = 5 }

ally: std.mem.Allocator,
/// The AST parsed by zig
ast: *Ast,
/// Context about the current scope
context: Context,
/// An array list used to manage the constants encountered in the program
constant_pool: std.ArrayList(Bytecode.Value),
/// The current instruction state for the bytecode
instructions: std.ArrayList(u8),

pub fn init(ally: std.mem.Allocator, ast: *Ast) Self {
    return .{
        .ally = ally,
        .ast = ast,
        .context = Context.init(ally),
        .constant_pool = std.ArrayList(Bytecode.Value).init(ally),
        .instructions = std.ArrayList(u8).init(ally),
    };
}

pub fn deinit(self: *Self) void {
    self.context.deinit();
    self.constant_pool.deinit();
    self.instructions.deinit();
}
/// Adds a constant to the constant pool
fn addConstant(self: *Self, value: Bytecode.Value) !ConstantIndex {
    for (self.constant_pool.items, 0..) |item, item_idx| {
        if (item.eql(value)) return @intCast(item_idx);
    }

    try self.constant_pool.append(value);
    return @intCast(self.constant_pool.items.len - 1);
}

/// Gets a constant from the pool
fn getConstant(self: *Self, item_idx: ConstantIndex) !Bytecode.Value {
    if (item_idx >= self.constant_pool.items.len) return error.ConstantOutOfBounds;
    return self.constant_pool.items[item_idx];
}

fn addInstruction(self: *Self, opcode: Bytecode.Opcode) !void {
    try self.instructions.append(opcode.raw());
}

fn appendSliceToInstructions(self: *Self, data: []const u8) !void {
    try self.instructions.appendSlice(data);
}

fn compileVarDecl(self: *Self, var_decl: Ast.full.VarDecl) !void {
    // const/let
    const mut_token = self.ast.tokenSlice(var_decl.ast.mut_token);
    // name of var (e.g., "a", "b", etc.)
    const ident_token = self.ast.tokenSlice(var_decl.ast.mut_token + 1);
    const mut: Variable.Mutability = if (std.mem.eql(u8, mut_token, "var"))
        .@"var"
    else if (std.mem.eql(u8, mut_token, "const"))
        .@"const"
    else {
        // todo: diagnostics?
        return error.UnknownMutability;
    };
    const ident_idx = try self.addConstant(.{ .identifier = ident_token });

    const variable = Variable{
        .scope = if (self.context.current_scope_depth == 0) .global else .local,
        .mut = mut,
        .name_index = ident_idx,
        .depth = self.context.current_scope_depth,
        .stack_offset = self.context.getStackOffset(),
    };

    if (variable.scope == .global) {
        if (self.context.hasGlobal(ident_idx)) {
            std.debug.panic("whoa! global {s} already exists!", .{ident_token});
        }
        try self.context.addGlobal(ident_idx);

        try self.compileExpression(var_decl.ast.init_node);

        try self.addInstruction(.set_global);
        try self.compileInt(ident_idx);
    } else {
        if (self.context.hasGlobal(ident_idx)) {
            std.debug.panic("whoa! local {s} overshadows global with the same name!", .{ident_token});
        }
        if (self.context.hasLocal(ident_idx)) {
            std.debug.panic("whoa! local {s} already exists!", .{ident_token});
        }

        try self.context.addLocal(ident_idx, variable);
        try self.compileExpression(var_decl.ast.init_node);

        //try self.addInstruction(.set_local);
        // try self.compileInt(variable.stack_offset);
    }
}

pub fn compileAssign(self: *Self, node_idx: Ast.Node.Index) !void {
    const node_data: Ast.Node.Data = self.ast.nodes.items(.data)[node_idx];
    const op_idx: Ast.TokenIndex = self.ast.nodes.items(.main_token)[node_idx];

    const var_ident = self.ast.getNodeSource(node_data.lhs);
    const ident_idx = try self.addConstant(.{ .identifier = var_ident });

    const local = self.context.getLocal(ident_idx);

    if (local == null and !self.context.hasGlobal(ident_idx)) {
        std.debug.panic("global '{s}' does not exist", .{var_ident});
    }

    try self.compileExpression(node_data.rhs);

    const get_op: Bytecode.Opcode, const set_op: Bytecode.Opcode = if (local) |_|
        .{ .get_local, .set_local }
    else
        .{ .get_global, .set_global };
    const data = if (local) |local_var| local_var.stack_offset else ident_idx;

    const op_slice = self.ast.tokenSlice(op_idx);
    if (try assignToOp(op_slice)) |op| {
        try self.addInstruction(get_op);
        try self.compileInt(data);
        try self.addInstruction(op);
    }

    // todo: locals
    try self.addInstruction(set_op);
    try self.compileInt(data);
}

inline fn assignToOp(token: []const u8) !?Bytecode.Opcode {
    if (std.mem.eql(u8, token, "=")) {
        return null;
    } else if (std.mem.eql(u8, token, "+=")) {
        return .add;
    } else if (std.mem.eql(u8, token, "-=")) {
        return .sub;
    } else if (std.mem.eql(u8, token, "*=")) {
        return .mul;
    } else if (std.mem.eql(u8, token, "/=")) {
        return .div;
    } else if (std.mem.eql(u8, token, "**=")) {
        return .pow;
    } else if (std.mem.eql(u8, token, "%=")) {
        return .mod;
    } else {
        return error.UnknownAssignType;
    }
}

pub fn compileExpression(self: *Self, node_idx: Ast.Node.Index) !void {
    const node_datas: []const Ast.Node.Data = self.ast.nodes.items(.data);
    const node_tag: Ast.Node.Tag = self.ast.nodes.items(.tag)[node_idx];
    const node_data: Ast.Node.Data = node_datas[node_idx];
    const node_source = self.ast.getNodeSource(node_idx);
    switch (node_tag) {
        .block, .block_two, .block_semicolon, .block_two_semicolon => {
            self.context.startScope();
            defer {
                self.context.endScope();
                self.addInstruction(.pop) catch unreachable;
            }
            var statements_buf: [2]Ast.Node.Index = undefined;
            const statements = switch (node_tag) {
                .block_two,
                .block_two_semicolon,
                => b: {
                    statements_buf = .{ node_data.lhs, node_data.rhs };
                    if (node_datas[node_idx].lhs == 0) {
                        break :b statements_buf[0..0];
                    } else if (node_datas[node_idx].rhs == 0) {
                        break :b statements_buf[0..1];
                    } else {
                        break :b statements_buf[0..2];
                    }
                },
                .block,
                .block_semicolon,
                => self.ast.extra_data[node_data.lhs..node_data.rhs],
                else => unreachable,
            };

            for (statements) |expr| {
                // std.debug.print("node_tag: {s}\n", .{@tagName(self.ast.nodes.items(.tag)[expr])});
                try self.compileStatement(@intCast(expr));
            }
        },
        .number_literal => {
            const parsed = std.zig.parseNumberLiteral(node_source);
            const value: Bytecode.Value = switch (parsed) {
                // double parsing sux
                .float => .{ .float = try std.fmt.parseFloat(f64, node_source) },
                .int => |value| .{ .int = @as(isize, @intCast(value)) },
                .big_int => @panic("big int not implemented"),
                .failure => return error.NumberParseError,
            };
            const const_idx = try self.addConstant(value);
            try self.addInstruction(.@"const");
            try self.compileInt(const_idx);
        },
        .identifier => {
            const keyword = Keywords.get(node_source);
            if (keyword) |op| {
                try self.addInstruction(op);
                return;
            }
            const const_idx = try self.addConstant(.{ .identifier = node_source });
            if (self.context.getLocal(const_idx)) |variable| {
                try self.addInstruction(.get_local);
                try self.compileInt(variable.stack_offset);
            } else if (self.context.hasGlobal(const_idx)) {
                try self.addInstruction(.get_global);
                try self.compileInt(const_idx);
            } else {
                std.debug.panic("global {s} not defined", .{node_source});
            }
        },
        .string_literal => {
            const const_idx = try self.addConstant(.{ .string = std.mem.trim(u8, node_source, "\"") });
            try self.addInstruction(.@"const");
            try self.compileInt(const_idx);
        },
        .add, .sub, .mul, .div, .array_mult => {
            try self.compileExpression(node_data.rhs);
            try self.compileExpression(node_data.lhs);
            try self.addInstruction(switch (node_tag) {
                .add => .add,
                .sub => .sub,
                .mul => .mul,
                .div => .div,
                .array_mult => .pow,
                inline else => unreachable,
            });
        },
        .greater_than, .less_than, .greater_or_equal, .less_or_equal, .equal_equal, .bang_equal => {
            try self.compileExpression(node_data.rhs);
            try self.compileExpression(node_data.lhs);
            try self.addInstruction(switch (node_tag) {
                .greater_than => .gt,
                .less_than => .lt,
                .greater_or_equal => .gte,
                .less_or_equal => .lte,
                .equal_equal => .eq,
                .bang_equal => .not_eq,
                inline else => unreachable,
            });
        },
        .@"if", .if_simple => {
            const if_expr = self.ast.fullIf(node_idx) orelse return error.IfExpected;
            const if_data = self.ast.nodes.items(.data)[node_idx];
            _ = if_data; // autofix

            // if (true) 2
            // 000 true
            // 001 jump_if_false (007 - 001 = 006)
            // 004 const [2]
            // 007 jump_fwd (00d - 007 = 003)
            // ...
            // else 5
            // 00a const [5]
            // ...
            // 00d void
            try self.compileExpression(if_expr.ast.cond_expr);

            try self.addInstruction(.jump_if_false);
            const jif_pos: u16 = @intCast(self.instructions.items.len);
            try self.compileInt(MaxJumpLength);
            const jif_offset_start = self.instructions.items.len;

            // body:
            try self.compileExpression(if_expr.ast.then_expr);

            try self.addInstruction(.jump_fwd);
            const jfwd_pos = self.instructions.items.len;
            try self.compileInt(MaxJumpLength);
            const jfwd_offset_start = self.instructions.items.len;

            const jif_offset_end = self.instructions.items.len;
            // else:
            if (if_expr.ast.else_expr != 0) {
                try self.compileExpression(if_expr.ast.else_expr);
            } else {
                try self.addInstruction(.void);
            }
            const jfwd_offset_end = self.instructions.items.len;

            // patch the relative jumps
            try self.replaceAt(jif_pos, u16, @intCast(jif_offset_end - jif_offset_start));
            try self.replaceAt(jfwd_pos, u16, @intCast(jfwd_offset_end - jfwd_offset_start));
        },
        inline else => |tag| std.debug.panic("expr {s} is not implemented", .{@tagName(tag)}),
    }
}

/// Replaces a section of our instruction bytes with the data given
pub fn replaceAt(self: *Self, start: usize, comptime DataType: type, data: DataType) !void {
    const data_type_info = @typeInfo(DataType);
    switch (data_type_info) {
        .Int => {
            const encoded = try self.encodeInt(data);
            @memcpy(self.instructions.items[start..(start + @sizeOf(DataType))], encoded);
        },
        inline else => std.debug.panic("{s} not implemented for replaceAt", .{@typeName(DataType)}),
    }
}

pub inline fn encodeInt(self: *Self, value: anytype) ![]const u8 {
    _ = self; // autofix
    const ValueType = @TypeOf(value);
    const value_type_info = @typeInfo(ValueType);
    if (value_type_info != .Int) @compileError("can not compile non-int in compileInt");

    const bits = value_type_info.Int.bits;
    if (bits % 8 != 0) @compileError("int must be divisible by 8");

    const bytes: u16 = bits / 8;
    var byte_buf: [bytes]u8 = undefined;

    std.mem.writeInt(ValueType, &byte_buf, value, .big);
    return &byte_buf;
}

pub fn compileInt(self: *Self, value: anytype) !void {
    const bytes = try self.encodeInt(value);
    try self.appendSliceToInstructions(bytes);
}

pub fn compileStatement(self: *Self, node_idx: Ast.Node.Index) anyerror!void {
    const node_data = self.ast.nodes.items(.data);
    const node_tags = self.ast.nodes.items(.tag);
    switch (node_tags[node_idx]) {
        .simple_var_decl => {
            const var_decl = self.ast.simpleVarDecl(node_idx);
            try self.compileVarDecl(var_decl);
        },
        // todo: **=
        .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => try self.compileAssign(node_idx),
        .fn_decl => {
            const fn_data = node_data[node_idx];
            const inner_fn_proto_idx = fn_data.lhs;

            // todo: fn decls
            // simple has no params?
            // multi has multi params?

            std.debug.print("fn: {any}\n", .{node_tags[inner_fn_proto_idx]});

            var buf: [1]Ast.Node.Index = undefined;
            const fn_proto = self.ast.fullFnProto(&buf, inner_fn_proto_idx).?;

            // const fn_body_idx = fn_data.rhs;
            // const fn_proto = self.ast.fnProto(fn_proto_idx);
            // const fn_decl = self.ast.fnProto(decl);

            const name_token = self.ast.tokenSlice(fn_proto.name_token.?);
            std.debug.print("fn {s}(", .{name_token});

            var iterator = fn_proto.iterate(self.ast);

            var param = iterator.next();
            while (param != null) {
                // const comptime_name = self.ast.tokenSlice(param.comptime_noalias);
                const param_name = self.ast.tokenSlice(param.?.name_token.?);
                const type_name = self.ast.getNodeSource(param.?.type_expr);
                std.debug.print("{s}: {s}", .{ param_name, type_name });

                param = iterator.next();
                if (param) |_| std.debug.print(", ", .{});
            }

            // todo: handle function return type & body
            std.debug.print(") {{}}\n", .{});
        },
        .@"if", .if_simple => try self.compileExpression(node_idx),
        inline else => |inner| {
            std.debug.panic("unhandled decl type: {s}\n", .{@tagName(inner)});
        },
    }
}

pub fn compile(self: *Self) !Bytecode {
    const root_decls = self.ast.rootDecls();
    std.debug.assert(root_decls.len != 0);

    const node_tags: []const Ast.Node.Tag = self.ast.nodes.items(.tag);
    const node_data: []const Ast.Node.Data = self.ast.nodes.items(.data);

    // this should always be 0(?)
    // preprocessor should have wrapped code in fn body
    const fn_decl_idx = root_decls[0];
    std.debug.assert(node_tags[fn_decl_idx] == .fn_decl);
    const fn_decl_data = node_data[fn_decl_idx];

    const fn_body = node_data[fn_decl_data.rhs];

    // 1. assert that the fn is the root
    // 2. walk the nodes inside of the fn body
    // 3. parse that as the base script
    const decls = self.ast.extra_data[fn_body.lhs..fn_body.rhs];

    // todo: if consts are referenced in a fn body, we need to declare that as global
    // iterate over the body
    for (decls) |decl| {
        try self.compileStatement(decl);
    }
    return Bytecode{
        .instructions = try self.instructions.toOwnedSlice(),
        .constant_pool = try self.constant_pool.toOwnedSlice(),
    };
}
