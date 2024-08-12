const std = @import("std");
const Vm = @import("../Vm.zig");

pub const Value = union(enum) {
    pub const True = Value{ .boolean = true };
    pub const False = Value{ .boolean = false };
    pub const Void = Value{ .void = {} };
    pub const Null = Value{ .null = {} };

    string: []const u8,
    identifier: []const u8,
    int: isize,
    float: f64,
    boolean: bool,
    void: void,
    null: void,

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
            .boolean => |boolean| boolean == other.boolean,
            .void, .null => true,
        };
    }

    pub fn greaterThan(self: Value, other: Value) !bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| int > other_int,
                .float => |other_float| @as(f64, @floatFromInt(int)) > other_float,
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| float > @as(f64, @floatFromInt(other_int)),
                .float => |other_float| float > other_float,
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn greaterThanOrEqual(self: Value, other: Value) !bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| int >= other_int,
                .float => |other_float| @as(f64, @floatFromInt(int)) >= other_float,
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| float >= @as(f64, @floatFromInt(other_int)),
                .float => |other_float| float >= other_float,
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn lessThan(self: Value, other: Value) !bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| int < other_int,
                .float => |other_float| @as(f64, @floatFromInt(int)) < other_float,
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| float < @as(f64, @floatFromInt(other_int)),
                .float => |other_float| float < other_float,
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn lessThanOrEqual(self: Value, other: Value) !bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag != other_tag) return false;

        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| int <= other_int,
                .float => |other_float| @as(f64, @floatFromInt(int)) <= other_float,
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| float >= @as(f64, @floatFromInt(other_int)),
                .float => |other_float| float >= other_float,
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn concat(self: Value, vm: *Vm, other: Value) !Value {
        if (self != .string) return error.StringExpected;
        const str = self.string;

        const fmted = try std.fmt.allocPrint(vm.ally, "{s}{s}", .{ str, other });
        defer vm.ally.free(fmted);

        return vm.allocString(fmted);
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

    pub fn sub(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = int - other_int },
                .float => |other_float| .{ .float = @as(f64, @floatFromInt(int)) - other_float },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = float - @as(f64, @floatFromInt(other_int)) },
                .float => |other_float| .{ .float = float - other_float },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn mul(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = int * other_int },
                .float => |other_float| .{ .float = @as(f64, @floatFromInt(int)) * other_float },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = float * @as(f64, @floatFromInt(other_int)) },
                .float => |other_float| .{ .float = float * other_float },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn div(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = @divTrunc(int, other_int) },
                .float => |other_float| .{ .float = @as(f64, @floatFromInt(int)) / other_float },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = float / @as(f64, @floatFromInt(other_int)) },
                .float => |other_float| .{ .float = float / other_float },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn pow(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = std.math.pow(isize, int, other_int) },
                .float => |other_float| .{ .float = std.math.pow(f64, @as(f64, @floatFromInt(int)), other_float) },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = std.math.pow(f64, float, @as(f64, @floatFromInt(other_int))) },
                .float => |other_float| .{ .float = std.math.pow(f64, float, other_float) },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn mod(self: Value, other: Value) !Value {
        return switch (self) {
            .int => |int| switch (other) {
                .int => |other_int| .{ .int = @mod(int, other_int) },
                .float => |other_float| .{ .float = @mod(@as(f64, @floatFromInt(int)), other_float) },
                inline else => error.UnsupportedOperation,
            },
            .float => |float| switch (other) {
                .int => |other_int| .{ .float = @mod(float, @as(f64, @floatFromInt(other_int))) },
                .float => |other_float| .{ .float = @mod(float, other_float) },
                inline else => error.UnsupportedOperation,
            },
            inline else => error.UnsupportedOperation,
        };
    }

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .string => |str| try writer.print("{s}", .{str}),
            .identifier => |ident| try writer.writeAll(ident),
            .int => |int| try writer.print("{d}", .{int}),
            .float => |float| try writer.print("{d}", .{float}),
            .boolean => |boolean| try writer.print("{}", .{boolean}),
            .null, .void => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};
