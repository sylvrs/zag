pub const Value = union(enum) {
    string: []const u8,
    identifier: []const u8,
    int: usize,
    float: f64,
};
