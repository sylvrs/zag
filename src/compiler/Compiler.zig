// dichael mouglas
const std = @import("std");
const Ast = std.zig.Ast;

const parser = @import("../parser.zig");
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
    const ScopeIndex = u32;

    ally: std.mem.Allocator,

    current_scope_depth: ScopeIndex = 0,
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

    /// A flag to determine whether we can handle break/continues in the current state of the program
    current_loop_depth: ScopeIndex = 0,
    /// A map of the scope depth to the list of break instruction offsets
    break_statements: std.AutoArrayHashMap(ScopeIndex, std.ArrayList(u16)),
    /// A map of the scope depth to the list of continue instruction offsets
    continue_statements: std.AutoArrayHashMap(ScopeIndex, std.ArrayList(u16)),

    pub fn init(ally: std.mem.Allocator) Context {
        return .{
            .ally = ally,
            .globals = std.AutoArrayHashMap(ConstantIndex, void).init(ally),
            .locals = std.AutoArrayHashMap(ConstantIndex, Variable).init(ally),
            .break_statements = std.AutoArrayHashMap(ScopeIndex, std.ArrayList(u16)).init(ally),
            .continue_statements = std.AutoArrayHashMap(ScopeIndex, std.ArrayList(u16)).init(ally),
        };
    }

    pub fn deinit(self: *Context) void {
        self.globals.deinit();
        self.locals.deinit();
        for (self.break_statements.values()) |*stmts| {
            stmts.deinit();
        }
        self.break_statements.deinit();
        for (self.continue_statements.values()) |*stmts| {
            stmts.deinit();
        }
        self.continue_statements.deinit();
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

    pub fn endScope(self: *Context) usize {
        self.current_scope_depth -= 1;

        var popped: usize = 0;
        var iterator = self.locals.iterator();
        while (iterator.next()) |entry| {
            const local = entry.value_ptr.*;
            // todo: stack-based?
            if (local.depth > self.current_scope_depth) {
                _ = self.locals.orderedRemove(entry.key_ptr.*);
                popped += 1;
            }
        }
        return popped;
    }

    pub fn addBreakStatement(self: *Context, instr_offset: u16) !void {
        // create the scope if we don't have one
        const scope = try self.break_statements.getOrPutValue(self.current_loop_depth, std.ArrayList(u16).init(self.ally));
        // append the instr offset to the list
        try scope.value_ptr.append(instr_offset);
    }

    pub fn addContinueStatement(self: *Context, instr_offset: u16) !void {
        // create the scope if we don't have one
        const scope = try self.continue_statements.getOrPutValue(self.current_loop_depth, std.ArrayList(u16).init(self.ally));
        // append the instr offset to the list
        try scope.value_ptr.append(instr_offset);
    }

    pub fn getCurrentContinues(self: *Context) ?*std.ArrayList(u16) {
        return self.continue_statements.getPtr(self.current_loop_depth);
    }

    pub fn getCurrentBreaks(self: *Context) ?*std.ArrayList(u16) {
        return self.break_statements.getPtr(self.current_loop_depth);
    }

    pub fn startLoopScope(self: *Context) void {
        self.current_loop_depth += 1;
    }

    pub fn clearLoopScope(self: *Context) void {
        self.current_loop_depth -= 1;
        const conts = self.getCurrentContinues();
        const breaks = self.getCurrentBreaks();
        // clear lists before removing
        if (conts) |list| list.deinit();
        if (breaks) |list| list.deinit();

        // remove scope from map when done
        if (self.continue_statements.contains(self.current_loop_depth)) {
            _ = self.continue_statements.orderedRemove(self.current_loop_depth);
        }
        if (self.break_statements.contains(self.current_loop_depth)) {
            _ = self.break_statements.orderedRemove(self.current_loop_depth);
        }
    }

    pub fn patchLoop(self: *Context, compiler: *Self, loop_cond_start: u16, loop_end: u16, cont_start: ?u16) void {
        // if we have continues, patch them with the instr offset where the loop condition starts
        if (self.getCurrentContinues()) |list| {
            for (list.items) |instr_offset| {
                try compiler.replaceAt(instr_offset, u16, if (cont_start) |cont| cont else loop_cond_start);
            }
            // i < 100
            // info:    | 000c const [#3 => 100]
            // info:    | 000f get_global [i]
            // info:    | 0012 lt
            // info:    | 0013 jump_if_false [001d]
            // info:    | 0016 const [#4 => 50]
            // info:    | 0019 get_global [i]
        }

        // jump here!
        // - i < 10
        // const 10
        // get i
        // lt
        // - i += 1
        // const 1
        // get i
        // add
        // set i
        // - continue;
        // jump_back loop_cond_start
        // loop end here
        //
        // while (i < 10) {
        // i += 1;
        // continue;
        // }

        // if we have breaks, patch them with the instr offset where the loop ends
        if (self.getCurrentBreaks()) |list| {
            for (list.items) |instr_offset| {
                try compiler.replaceAt(instr_offset, u16, loop_end);
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

/// Returns the current instruction offset
fn getInstructionOffset(self: *Self) u16 {
    return @intCast(self.instructions.items.len);
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

    // todo: don't allow overwriting if var is const
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

            // fetch statements from block
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

            const popped = self.context.endScope();
            // if we removed vars from the scope, we should pop them after we exit scope
            for (0..popped) |_| try self.addInstruction(.pop);
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
        .@"while", .while_cont, .while_simple => {
            self.context.startLoopScope();
            defer self.context.clearLoopScope();
            const while_expr = self.ast.fullWhile(node_idx) orelse return error.WhileExpected;

            const jb_target = self.getInstructionOffset();
            try self.compileExpression(while_expr.ast.cond_expr);
            try self.addInstruction(.jump_if_false);
            const jif_data_pos = self.getInstructionOffset();
            try self.compileInt(MaxJumpLength);
            // const jif_start = self.getInstructionOffset();
            // todo: compile the cont expression
            try self.compileExpression(while_expr.ast.then_expr);

            var cont_pos: ?u16 = null;
            // todo: is this the correct behavior?
            if (while_expr.ast.cont_expr != 0) {
                cont_pos = self.getInstructionOffset();
                try self.compileStatement(while_expr.ast.cont_expr);
            }

            try self.addInstruction(.jump_back);
            const jb_data_pos = self.getInstructionOffset();
            try self.compileInt(MaxJumpLength);
            // todo: compile the else expression

            const jif_target = self.getInstructionOffset();

            try self.replaceAt(jif_data_pos, u16, jif_target);
            try self.replaceAt(jb_data_pos, u16, jb_target);
            // patch in breaks and conts if we have them
            self.context.patchLoop(self, jb_target, jif_target, cont_pos);
        },
        .@"if", .if_simple => {
            const if_expr = self.ast.fullIf(node_idx) orelse return error.IfExpected;
            try self.compileExpression(if_expr.ast.cond_expr);

            try self.addInstruction(.jump_if_false);
            const jif_pos = self.getInstructionOffset();
            try self.compileInt(MaxJumpLength);

            // body:
            try self.compileStatement(if_expr.ast.then_expr);

            // else:
            if (if_expr.ast.else_expr != 0) {
                try self.addInstruction(.jump_fwd);
                const jfwd_pos = self.getInstructionOffset();
                try self.compileInt(MaxJumpLength);

                try self.compileStatement(if_expr.ast.else_expr);
                const jfwd_target = self.getInstructionOffset();
                try self.replaceAt(jfwd_pos, u16, jfwd_target);
            } else {
                // todo: how should we manage voids here
                // try self.addInstruction(.void);
            }

            const jif_target = self.getInstructionOffset();
            // patch the relative jumps
            try self.replaceAt(jif_pos, u16, jif_target);
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
    const node_data: []const Ast.Node.Data = self.ast.nodes.items(.data);
    const node_tags: []const Ast.Node.Tag = self.ast.nodes.items(.tag);
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
        .block, .block_two, .block_semicolon, .block_two_semicolon => try self.compileExpression(node_idx),
        .@"if", .if_simple => try self.compileExpression(node_idx),
        .@"while", .while_cont, .while_simple => try self.compileExpression(node_idx),
        .@"continue" => {
            if (self.context.current_loop_depth == 0) {
                return error.ContinueNotInLoop;
            }
            try self.addInstruction(.jump_back);
            const jump_data_pos = self.getInstructionOffset();
            try self.compileInt(MaxJumpLength);
            try self.context.addContinueStatement(jump_data_pos);
        },
        .@"break" => {
            if (self.context.current_loop_depth == 0) {
                return error.BreakNotInLoop;
            }

            // todo: handle break values
            try self.addInstruction(.jump_fwd);
            const jump_data_pos = self.getInstructionOffset();
            try self.context.addBreakStatement(jump_data_pos);
            try self.compileInt(MaxJumpLength);
        },
        inline else => |inner| {
            self.compileExpression(node_idx) catch {
                const source = self.ast.getNodeSource(node_idx);
                std.debug.panic("unhandled decl type '{s}':\n{s}", .{ @tagName(inner), source });
            };
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
    var decls_buf: [2]Ast.Node.Index = undefined;
    const decls = switch (node_tags[fn_decl_data.rhs]) {
        .block_two,
        .block_two_semicolon,
        => b: {
            decls_buf = .{ fn_body.lhs, fn_body.rhs };
            if (fn_body.lhs == 0) {
                break :b decls_buf[0..0];
            } else if (fn_body.rhs == 0) {
                break :b decls_buf[0..1];
            } else {
                break :b decls_buf[0..2];
            }
        },
        .block,
        .block_semicolon,
        => self.ast.extra_data[fn_body.lhs..fn_body.rhs],
        else => unreachable,
    };

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

fn compileAndTest(expected: []const u8, input: []const u8) !void {
    const Vm = @import("../Vm.zig");
    const ally = std.testing.allocator;

    const preprocessed = try parser.preprocess(ally, input);
    defer ally.free(preprocessed);
    var ast = try Ast.parse(ally, preprocessed, .zig);
    defer ast.deinit(ally);

    var compiler = Self.init(ally, &ast);
    defer compiler.deinit();
    const program = try compiler.compile();
    defer ally.free(program.instructions);
    defer ally.free(program.constant_pool);

    var vm = Vm.init(ally, program);
    defer vm.deinit();

    try vm.run();

    try std.testing.expectFmt(expected, "{s}", .{program});
}

test "ensure basic while loop works" {
    const source =
        \\var i = 0;
        \\while (i < 100): (i += 1) {
        \\    // do something here...
        \\}
    ;
    const expected =
        \\0000 const [#1 => 0]
        \\0003 set_global [i]
        \\0006 const [#2 => 100]
        \\0009 get_global [i]
        \\000c lt
        \\000d jump_if_false [001d]
        \\0010 const [#3 => 1]
        \\0013 get_global [i]
        \\0016 add
        \\0017 set_global [i]
        \\001a jump_back [0006]
    ;
    try compileAndTest(expected, source);
}

test "ensure continue statement works" {
    const source =
        \\var i = 0;
        \\while (i < 100): (i += 1) {
        \\    if (i < 50) continue;
        \\    // do something here...
        \\}
    ;
    const expected =
        \\0000 const [#1 => 0]
        \\0003 set_global [i]
        \\0006 const [#2 => 100]
        \\0009 get_global [i]
        \\000c lt
        \\000d jump_if_false [002a]
        \\0010 const [#3 => 50]
        \\0013 get_global [i]
        \\0016 lt
        \\0017 jump_if_false [001d]
        \\001a jump_back [001d]
        \\001d const [#4 => 1]
        \\0020 get_global [i]
        \\0023 add
        \\0024 set_global [i]
        \\0027 jump_back [0006]
    ;
    try compileAndTest(expected, source);
}
