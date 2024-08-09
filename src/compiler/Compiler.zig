const std = @import("std");
const Ast = std.zig.Ast;
const Bytecode = @import("Bytecode.zig");

const Self = @This();

/// Context represents scope, depth, etc.
const Context = struct {};

// constant pool:
// 0: Value{ .string = "name" }
// 1: Value{ .int = 1 }
// 2: Value{ .float = 5 }

arena: std.heap.ArenaAllocator,
/// The AST parsed by zig
ast: *Ast,
context: Context,

pub fn init(ally: std.mem.Allocator, ast: *Ast) Self {
    return .{ .arena = std.heap.ArenaAllocator.init(ally), .ast = ast, .context = .{} };
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn compile(self: *Self) !Bytecode {
    const decls = self.ast.rootDecls();
    std.debug.assert(decls.len != 0);
    //    std.debug.print("decls: {any}\n", .{decls});
    const root = decls[0];
    _ = root;

    const node_tags: []const Ast.Node.Tag = self.ast.nodes.items(.tag);
    const main_tokens: []const Ast.TokenIndex = self.ast.nodes.items(.main_token);
    _ = main_tokens;
    const node_data: []const Ast.Node.Data = self.ast.nodes.items(.data);
    for (decls) |decl| {
        const tag = node_tags[decl];

        switch (tag) {
            .simple_var_decl => {
                const var_decl = self.ast.simpleVarDecl(decl);

                // mut token = const/var
                const mut_token_idx = var_decl.ast.mut_token;
                // identifier = "a" or "b", etc.
                const identifier_token_idx = mut_token_idx + 1;

                const mut_token = self.ast.tokenSlice(mut_token_idx);
                const identifier_token = self.ast.tokenSlice(identifier_token_idx);

                const value_token = self.ast.getNodeSource(var_decl.ast.init_node);

                std.debug.print("{s} {s} = {s};\n", .{ mut_token, identifier_token, value_token });
            },
            .fn_decl => {
                const fn_data = node_data[decl];
                const fn_proto_idx = fn_data.lhs;

                const fn_proto = self.ast.fnProtoMulti(fn_proto_idx);

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
    return Bytecode{ .instructions = &.{}, .constant_pool = &.{} };
}
