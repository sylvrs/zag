const std = @import("std");

pub const Value = union(enum) {
    string: []const u8,
    identifier: []const u8,
    int: usize,
    float: f64,

    /// Returns true if the value provided has the same tag type and inner value
    pub fn eql(self: Value, other: Value) bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .string => |str| std.mem.eql(u8, str, other.string),
            .identifier => |ident| std.mem.eql(u8, ident, other.identifier),
            .int => |int| int == other.int,
            .float => |float| float == other.float,
        };
    }

    pub fn add(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = int + other_int },
                .float => |other_float| .{ .float = @as(f64, @floatFromInt(int)) + other_float },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = float + @as(f64, @floatFromInt(other_int)) },
                .float => |other_float| .{ .float = float + other_float },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .string => |str| try writer.print("\"{s}\"", .{str}),
            .identifier => |ident| try writer.writeAll(ident),
            .int => |int| try writer.print("{d}", .{int}),
            .float => |float| try writer.print("{d}", .{float}),
        }
    }
};
