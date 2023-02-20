const std = @import("std");

pub fn build(b: *std.Build) void {
    b.addModule(.{
        .name = "tres",
        .source_file = .{ .path = "tres.zig" },
    });
}
