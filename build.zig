const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("tres", .{
        .root_source_file = .{ .path = "tres.zig" },
    });
}
