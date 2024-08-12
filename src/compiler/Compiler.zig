const std = @import("std");
const Ast = std.zig.Ast;
const Bytecode = @import("Bytecode.zig");

const Self = @This();

/// Represents an index in the constant pool
const ConstantIndex = u16;

const Variable = struct {
    /// TODO: we could potentially make this a tagged union and provide the scope index for local variables
    const Scope = enum { global, local };
    /// Whether the variable is constant or
    const Mutability = enum { @"const", @"var" };

    scope: Scope,
    mut: Mutability,
    name_index: ConstantIndex,
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

    pub fn deinit(self: *Context) void {
        self.globals.deinit();
    }

    pub fn hasGlobal(self: *Context, name_idx: ConstantIndex) bool {
        return self.globals.contains(name_idx);
    }

    pub fn addGlobal(self: *Context, name_idx: ConstantIndex) !void {
        try self.globals.put(name_idx, {});
    }
};

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
        .context = Context{
            .globals = std.AutoArrayHashMap(ConstantIndex, void).init(ally),
        },
        .constant_pool = std.ArrayList(Bytecode.Value).init(ally),
        .instructions = std.ArrayList(u8).init(ally),
    };
}

pub fn deinit(self: *Self) void {
    self.context.globals.deinit();
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
    };

    if (variable.scope == .global) {
        if (self.context.hasGlobal(ident_idx)) {
            @panic("whoa. it already exists!");
        }
        try self.context.addGlobal(ident_idx);

        try self.compileExpression(var_decl.ast.init_node);

        try self.addInstruction(.set_global);
        try self.compileInt(ident_idx);
    } else {
        @panic("local vars are not supported yet");
    }
}

pub fn compileAssign(self: *Self, node_idx: Ast.Node.Index) !void {
    const node_data: Ast.Node.Data = self.ast.nodes.items(.data)[node_idx];
    const op_idx: Ast.TokenIndex = self.ast.nodes.items(.main_token)[node_idx];

    const var_ident = self.ast.getNodeSource(node_data.lhs);
    const ident_idx = try self.addConstant(.{ .identifier = var_ident });

    if (!self.context.hasGlobal(ident_idx)) {
        std.debug.panic("global '{s}' does not exist", .{var_ident});
    }

    try self.compileExpression(node_data.rhs);

    const op_slice = self.ast.tokenSlice(op_idx);
    if (try assignToOp(op_slice)) |op| {
        try self.addInstruction(.get_global);
        try self.compileInt(ident_idx);
        try self.addInstruction(op);
    }

    // todo: locals
    try self.addInstruction(.set_global);
    try self.compileInt(ident_idx);
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
    const node_tag: Ast.Node.Tag = self.ast.nodes.items(.tag)[node_idx];
    const node_data: Ast.Node.Data = self.ast.nodes.items(.data)[node_idx];
    const node_source = self.ast.getNodeSource(node_idx);
    switch (node_tag) {
        .number_literal => {
            const parsed = std.zig.parseNumberLiteral(node_source);
            const value: Bytecode.Value = switch (parsed) {
                // double parsing sux
                .float => .{ .float = try std.fmt.parseFloat(f64, node_source) },
                .int => |value| .{ .int = @as(usize, @intCast(value)) },
                .big_int => @panic("big int not implemented"),
                .failure => return error.NumberParseError,
            };
            const const_idx = try self.addConstant(value);
            try self.addInstruction(.@"const");
            try self.compileInt(const_idx);
        },
        .identifier => {
            const const_idx = try self.addConstant(.{ .identifier = node_source });
            if (self.context.hasGlobal(const_idx)) {
                try self.addInstruction(.get_global);
                try self.compileInt(const_idx);
            } else {
                std.debug.panic("global {s} not defined", .{node_source});
            }
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
        inline else => |tag| std.debug.panic("expr {s} is not implemented", .{@tagName(tag)}),
    }
}

pub fn compileInt(self: *Self, value: anytype) !void {
    const ValueType = @TypeOf(value);
    const value_type_info = @typeInfo(ValueType);
    if (value_type_info != .Int) @compileError("can not compile non-int in compileInt");

    const bits = value_type_info.Int.bits;
    if (bits % 8 != 0) @compileError("int must be divisible by 8");

    const bytes: u16 = bits / 8;
    var byte_buf: [bytes]u8 = undefined;

    std.mem.writeInt(u16, &byte_buf, value, .big);
    try self.appendSliceToInstructions(&byte_buf);
}

pub fn compile(self: *Self) !Bytecode {
    const root_decls = self.ast.rootDecls();
    std.debug.assert(root_decls.len != 0);

    const node_tags: []const Ast.Node.Tag = self.ast.nodes.items(.tag);
    const main_tokens: []const Ast.TokenIndex = self.ast.nodes.items(.main_token);
    _ = main_tokens; // autofix
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
    //

    const decls = self.ast.extra_data[fn_body.lhs..fn_body.rhs];

    // todo: if consts are referenced in a fn body, we need to declare that as global
    // iterate over the body
    for (decls) |decl| {
        const tag = node_tags[decl];
        //        std.debug.print("tag: {s}\n", .{@tagName(tag)});
        switch (tag) {
            .simple_var_decl => {
                const var_decl = self.ast.simpleVarDecl(decl);
                try self.compileVarDecl(var_decl);
            },
            // todo: **=
            .assign, .assign_add, .assign_sub, .assign_mul, .assign_div, .assign_mod => try self.compileAssign(decl),
            .fn_decl => {
                const fn_data = node_data[decl];
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
            inline else => |inner| {
                std.debug.panic("unhandled decl type: {s}\n", .{@tagName(inner)});
            },
        }
    }
    return Bytecode{
        .instructions = try self.instructions.toOwnedSlice(),
        .constant_pool = try self.constant_pool.toOwnedSlice(),
    };
}
