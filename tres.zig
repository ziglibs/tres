const std = @import("std");

pub const ParseOptions = struct {
    suppress_error_logs: bool = true,
    /// Allocator, needed for non-u8/std.json.Value arrays
    allocator: ?std.mem.Allocator = null,
};

pub fn parse(comptime T: type, tree: std.json.Value, options: ParseOptions) ParseInternalError(T)!T {
    return try parseInternal(T, "root", tree, options);
}

pub fn Undefinedable(comptime T: type) type {
    return struct {
        const __json_T = T;
        const __json_is_undefinedable = true;

        value: T,
        missing: bool,
    };
}

pub fn ParseInternalError(comptime T: type) type {
    if ((@typeInfo(T) == .Struct or @typeInfo(T) == .Enum or @typeInfo(T) == .Union) and @hasDecl(T, "tresParse")) {
        const tresParse_return = @typeInfo(@typeInfo(@TypeOf(T.tresParse)).Fn.return_type.?);
        if (tresParse_return == .ErrorUnion) {
            return tresParse_return.ErrorUnion.error_set;
        } else {
            return error{};
        }
    }

    // `inferred_types` is used to avoid infinite recursion for recursive type definitions.
    const inferred_types = [_]type{};
    return ParseInternalErrorImpl(T, &inferred_types);
}

fn ParseInternalErrorImpl(comptime T: type, comptime inferred_types: []const type) type {
    for (inferred_types) |ty| {
        if (T == ty) return error{};
    }

    switch (@typeInfo(T)) {
        .Bool, .Float => return error{UnexpectedFieldType},
        .Pointer => |info| switch (info.child) {
            u8, std.json.Value => return error{UnexpectedFieldType},
            else => return error{ UnexpectedFieldType, OutOfMemory } || (if (info.child != std.json.Value) error{AllocatorRequired} else error{}) || ParseInternalErrorImpl(info.child, inferred_types ++ [_]type{T}),
        },
        .Optional => |info| return ParseInternalErrorImpl(info.child, inferred_types ++ [_]type{T}),
        .Enum => return error{ InvalidEnumTag, UnexpectedFieldType },
        .Int => return error{ UnexpectedFieldType, Overflow },
        .Union => |info| {
            var errors = error{UnexpectedFieldType};
            for (info.fields) |field| {
                errors = errors || ParseInternalErrorImpl(field.field_type, inferred_types ++ [_]type{T});
            }
            return errors;
        },
        .Struct => |info| {
            var errors = error{
                UnexpectedFieldType,
                InvalidFieldValue,
                MissingRequiredField,
            };
            if (@hasDecl(T, "KV") and std.meta.fields(T.KV)[1].field_type != std.json.Value) errors = errors || error{AllocatorRequired};
            for (info.fields) |field| {
                errors = errors || ParseInternalErrorImpl(field.field_type, inferred_types ++ [_]type{T});
            }
            return errors;
        },
        else => return error{},
    }
}

const logger = std.log.scoped(.json);
fn parseInternal(comptime T: type, comptime name: []const u8, value: std.json.Value, options: ParseOptions) ParseInternalError(T)!T {
    if (T == std.json.Value) return value;
    if ((@typeInfo(T) == .Struct or @typeInfo(T) == .Enum or @typeInfo(T) == .Union) and @hasDecl(T, "tresParse")) {
        return T.tresParse(value, options);
    }

    switch (@typeInfo(T)) {
        .Bool => {
            if (value == .Bool) {
                return value.Bool;
            } else {
                if (!options.suppress_error_logs) logger.err("expected Bool, found {s} at {s}", .{ @tagName(value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Float => {
            if (value == .Float) {
                return @floatCast(T, value.Float);
            } else {
                if (!options.suppress_error_logs) logger.err("expected Float, found {s} at {s}", .{ @tagName(value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Int => {
            if (value == .Integer) {
                return try std.math.cast(T, value.Integer);
            } else {
                if (!options.suppress_error_logs) logger.err("expected Integer, found {s} at {s}", .{ @tagName(value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Optional => |info| {
            if (value == .Null) {
                return null;
            } else {
                return try parseInternal(info.child, name ++ ".?", value, options);
            }
        },
        .Enum => {
            if (value == .Integer) {
                // we use this to convert signed to unsigned and check if it actually fits.
                const tag = std.math.cast(std.meta.Tag(T), value.Integer) catch {
                    if (!options.suppress_error_logs) logger.err("invalid enum tag for {s}, found {d} at {s}", .{ @typeName(T), value.Integer, name });

                    return error.InvalidEnumTag;
                };

                return try std.meta.intToEnum(T, tag);
            } else if (value == .String) {
                return std.meta.stringToEnum(T, value.String) orelse {
                    if (!options.suppress_error_logs) logger.err("invalid enum tag for {s}, found '{s}' at {s}", .{ @typeName(T), value.String, name });

                    return error.InvalidEnumTag;
                };
            } else {
                if (!options.suppress_error_logs) logger.err("expected Integer or String, found {s} at {s}", .{ @tagName(value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Union => |info| {
            if (info.tag_type != null) {
                inline for (info.fields) |field| {
                    var union_options = options;
                    union_options.suppress_error_logs = true;

                    if (parseInternal(field.field_type, name ++ "." ++ field.name, value, union_options)) |parsed_value| {
                        return @unionInit(T, field.name, parsed_value);
                    } else |_| {}
                }

                if (!options.suppress_error_logs) logger.err("union fell through for {s}, found {s} at {s}", .{ @typeName(T), @tagName(value), name });

                return error.UnexpectedFieldType;
            } else {
                @compileError("cannot parse an untagged union: " ++ @typeName(T));
            }
        },
        .Struct => |info| {
            if (@hasDecl(T, "KV")) {
                const Key = std.meta.fields(T.KV)[0].field_type;
                const Value = std.meta.fields(T.KV)[1].field_type;

                if (Key != []const u8) @compileError("ArrayHashMap key must be of type []const u8!");

                if (value == .Object) {
                    if (Value == std.json.Value) return value.Object;

                    var map = T.init(options.allocator orelse return error.AllocatorRequired);
                    var map_iterator = value.Object.iterator();

                    while (map_iterator.next()) |entry| {
                        try map.put(entry.key_ptr.*, try parseInternal(Value, name ++ ".entry", entry.value_ptr.*, options));
                    }

                    return map;
                } else {
                    if (!options.suppress_error_logs) logger.err("expected map of {s} at {s}, found {s}", .{ @typeName(Value), name, @tagName(value) });
                    return error.UnexpectedFieldType;
                }
            }

            if (info.is_tuple) {
                if (value != .Array) {
                    if (!options.suppress_error_logs) logger.err("expected Array, found {s} at {s}", .{ @tagName(value), name });
                    return error.UnexpectedFieldType;
                }

                if (value.Array.items.len != std.meta.fields(T).len) {
                    if (!options.suppress_error_logs) logger.err("expected Array to match length of Tuple {s} but it doesn't; at {s}", .{ @typeName(T), name });
                    return error.UnexpectedFieldType;
                }

                var tuple: T = undefined;
                comptime var index: usize = 0;

                inline while (index < std.meta.fields(T).len) : (index += 1) {
                    tuple[index] = try parseInternal(std.meta.fields(T)[index].field_type, name ++ "." ++ std.fmt.comptimePrint("{d}", .{index}), value.Array.items[index], options);
                }

                return tuple;
            }

            if (value == .Object) {
                var result: T = undefined;

                // Must use in order to bypass [#2727](https://github.com/ziglang/zig/issues/2727) :(
                var missing_field = false;

                inline for (info.fields) |field| {
                    const field_value = value.Object.get(field.name);

                    if (field.is_comptime) {
                        if (field_value == null) {
                            if (!options.suppress_error_logs) logger.err("comptime field {s}.{s} missing, at {s}", .{ @typeName(T), field.name, name });

                            return error.InvalidFieldValue;
                        }

                        const parsed_value = try parseInternal(field.field_type, name ++ "." ++ field.name, field_value.?, options);
                        // NOTE: This only works for strings!
                        if (!std.mem.eql(u8, parsed_value, field.default_value.?)) {
                            if (!options.suppress_error_logs) logger.err("comptime field {s}.{s} does not match", .{ @typeName(T), field.name });

                            return error.InvalidFieldValue;
                        }
                    } else {
                        if (field_value) |fv| {
                            if (@typeInfo(field.field_type) == .Struct and @hasDecl(field.field_type, "__json_is_undefinedable"))
                                @field(result, field.name) = .{ .value = try parseInternal(field.field_type.__json_T, name ++ "." ++ field.name, fv, options), .missing = false }
                            else
                                @field(result, field.name) = try parseInternal(field.field_type, name ++ "." ++ field.name, fv, options);
                        } else {
                            if (@typeInfo(field.field_type) == .Struct and @hasDecl(field.field_type, "__json_is_undefinedable")) {
                                @field(result, field.name) = .{
                                    .value = undefined,
                                    .missing = true,
                                };
                            } else if (field.default_value) |default| {
                                @field(result, field.name) = default;
                            } else {
                                if (!options.suppress_error_logs) logger.err("required field {s}.{s} missing, at {s}", .{ @typeName(T), field.name, name });

                                missing_field = true;
                            }
                        }
                    }
                }

                if (missing_field) return error.MissingRequiredField;

                return result;
            } else {
                if (!options.suppress_error_logs) logger.err("expected Object, found {s} at {s}", .{ @tagName(value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Pointer => |info| {
            switch (info.size) {
                .Slice => {
                    if (info.child == u8) {
                        if (value == .String) {
                            return value.String;
                        } else {
                            if (!options.suppress_error_logs) logger.err("expected String, found {s} at {s}", .{ @tagName(value), name });

                            return error.UnexpectedFieldType;
                        }
                    } else {
                        if (value == .Array) {
                            if (info.child == std.json.Value) return value.Array.items;

                            var array = try (options.allocator orelse return error.AllocatorRequired).alloc(info.child, value.Array.items.len);
                            for (value.Array.items) |item, index|
                                array[index] = try parseInternal(info.child, name ++ "[...]", item, options);

                            return array;
                        } else {
                            if (!options.suppress_error_logs) logger.err("expected Array, found {s} at {s}", .{ @tagName(value), name });

                            return error.UnexpectedFieldType;
                        }
                    }
                },
                else => @compileError("unhandled pointer type: " ++ @typeName(T) ++ " at " ++ name),
            }
        },
        else => {
            @compileError("unhandled json type: " ++ @typeName(T) ++ " at " ++ name);
        },
    }
}

test "json.parse simple struct" {
    @setEvalBranchQuota(10_000);

    const Role = enum(i64) { crewmate, impostor, ghost };

    const Union = union(enum) {
        a: i64,
        b: []const u8,
    };

    const Substruct = struct {
        value: std.json.Value,
        slice_of_values: []std.json.Value,

        union_a: Union,
        union_b: Union,
    };

    const Player = struct {
        name: []const u8,
        based: bool,
    };

    const MyTuple = std.meta.Tuple(&[_]type{ i64, bool });

    const Struct = struct {
        bool_true: bool,
        bool_false: bool,
        integer: u8,
        float: f64,
        optional: ?f32,
        an_enum: Role,
        an_enum_string: Role,
        slice: []i64,
        substruct: Substruct,

        random_map: std.json.ObjectMap,
        number_map: std.StringArrayHashMap(i64),
        players: std.StringArrayHashMap(Player),

        my_tuple: MyTuple,
        // my_array: [2]u8,
    };

    const json =
        \\{
        \\    "bool_true": true,
        \\    "bool_false": false,
        \\    "integer": 100,
        \\    "float": 4.2069,
        \\    "optional": null,
        \\    "an_enum": 1,
        \\    "an_enum_string": "crewmate",
        \\    "slice": [1, 2, 3, 4, 5, 6],
        \\    "substruct": {
        \\        "value": "hello",
        \\        "slice_of_values": ["hello", "world"],
        \\        "union_a": -42,
        \\        "union_b": "hello"
        \\    },
        \\    "random_map": {
        \\        "a": 123,
        \\        "b": "Amogus!!"
        \\    },
        \\    "number_map": {
        \\        "a": 123,
        \\        "b": 456
        \\    },
        \\    "players": {
        \\        "aurame": {"name": "Auguste", "based": true},
        \\        "mattnite": {"name": "Matt", "based": true}
        \\    },
        \\    "my_tuple": [10, false]
        \\}
    ;

    // NOTE: In practice, we're going to use an arena, thus no parseFree exists because it is not required :)
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const tree = try testing_parser.parse(json);

    const parsed = try parse(Struct, tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expectEqual(true, parsed.bool_true);
    try std.testing.expectEqual(false, parsed.bool_false);
    try std.testing.expectEqual(@as(u8, 100), parsed.integer);
    try std.testing.expectApproxEqRel(@as(f64, 4.2069), parsed.float, std.math.epsilon(f64));
    try std.testing.expectEqual(@as(?f32, null), parsed.optional);
    try std.testing.expectEqual(Role.impostor, parsed.an_enum);
    try std.testing.expectEqual(Role.crewmate, parsed.an_enum_string);
    try std.testing.expectEqualSlices(i64, &[_]i64{ 1, 2, 3, 4, 5, 6 }, parsed.slice);

    try std.testing.expect(parsed.substruct.value == .String);
    try std.testing.expectEqualStrings("hello", parsed.substruct.value.String);
    try std.testing.expect(parsed.substruct.slice_of_values.len == 2);
    try std.testing.expect(parsed.substruct.slice_of_values[0] == .String);
    try std.testing.expectEqualStrings("hello", parsed.substruct.slice_of_values[0].String);
    try std.testing.expect(parsed.substruct.slice_of_values[1] == .String);
    try std.testing.expectEqualStrings("world", parsed.substruct.slice_of_values[1].String);
    try std.testing.expect(parsed.substruct.union_a == .a);
    try std.testing.expectEqual(@as(i64, -42), parsed.substruct.union_a.a);
    try std.testing.expect(parsed.substruct.union_b == .b);
    try std.testing.expectEqualStrings("hello", parsed.substruct.union_b.b);

    try std.testing.expectEqual(@as(i64, 123), parsed.random_map.get("a").?.Integer);
    try std.testing.expectEqualStrings("Amogus!!", parsed.random_map.get("b").?.String);

    try std.testing.expectEqual(@as(i64, 123), parsed.number_map.get("a").?);
    try std.testing.expectEqual(@as(i64, 456), parsed.number_map.get("b").?);

    try std.testing.expectEqualStrings("Auguste", parsed.players.get("aurame").?.name);
    try std.testing.expectEqualStrings("Matt", parsed.players.get("mattnite").?.name);
    try std.testing.expectEqual(true, parsed.players.get("aurame").?.based);
    try std.testing.expectEqual(true, parsed.players.get("mattnite").?.based);

    try std.testing.expectEqual(MyTuple{ 10, false }, parsed.my_tuple);
}

test "json.parse missing field" {
    const Struct = struct {
        my_super_duper_important_field: bool,
    };

    const json =
        \\{}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const tree = try testing_parser.parse(json);

    const parsed = parse(Struct, tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expectError(error.MissingRequiredField, parsed);
}

test "json.parse undefinedable fields and default values" {
    const Struct = struct {
        meh: Undefinedable(i64),
        meh2: Undefinedable(i64),
        default: u8 = 123,
    };

    const json =
        \\{
        \\    "meh": 42069
        \\} 
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const tree = try testing_parser.parse(json);

    const parsed = try parse(Struct, tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expectEqual(@as(i64, 42069), parsed.meh.value);
    try std.testing.expectEqual(true, parsed.meh2.missing);
    try std.testing.expectEqual(@as(u8, 123), parsed.default);
}

test "json.parse comptime fields" {
    const YoureTheImpostorMessage = struct {
        comptime method: []const u8 = "ship/impostor",
        sussiness: f64,
    };

    const YoureCuteUwUMessage = struct {
        comptime method: []const u8 = "a/cutiepie",
        cuteness: i64,
    };

    const Message = union(enum) {
        youre_the_impostor: YoureTheImpostorMessage,
        youre_cute_uwu: YoureCuteUwUMessage,
    };

    const first_message =
        \\{
        \\    "method": "ship/impostor",
        \\    "sussiness": 69.420
        \\}
    ;

    const second_message =
        \\{
        \\    "method": "a/cutiepie",
        \\    "cuteness": 100
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);

    const first_tree = try testing_parser.parse(first_message);
    const first_parsed = try parse(Message, first_tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expect(first_parsed == .youre_the_impostor);

    testing_parser.reset();

    const second_tree = try testing_parser.parse(second_message);
    const second_parsed = try parse(Message, second_tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expect(second_parsed == .youre_cute_uwu);
}

test "json.parse custom check functions for unions" {
    // jsonrpc request
    const RequestId = union(enum) { string: []const u8, integer: i64 };

    const AmogusRequest = struct {
        const method = "spaceship/amogus";

        sussy: bool,
    };

    const MinecraftNotification = struct {
        const method = "game/minecraft";

        crafted: i64,
        mined: i64,
    };

    const RequestParams = union(enum) {
        amogus: AmogusRequest,
        minecraft: MinecraftNotification,
    };

    const RequestOrNotification = struct {
        const Self = @This();

        jsonrpc: []const u8,
        id: ?RequestId = null,
        method: []const u8,
        params: RequestParams,

        fn RequestOrNotificationParseError() type {
            var err = ParseInternalError(RequestId);
            inline for (std.meta.fields(RequestParams)) |field| {
                err = err || ParseInternalError(field.field_type);
            }
            return err;
        }

        pub fn tresParse(value: std.json.Value, options: ParseOptions) RequestOrNotificationParseError()!Self {
            // var allocator = options.allocator orelse return error.AllocatorRequired;
            var object = value.Object;
            var request_or_notif: Self = undefined;

            request_or_notif.jsonrpc = object.get("jsonrpc").?.String;
            request_or_notif.id = if (object.get("id")) |id| try parse(RequestId, id, options) else null;
            request_or_notif.method = object.get("method").?.String;

            inline for (std.meta.fields(RequestParams)) |field| {
                if (std.mem.eql(u8, request_or_notif.method, field.field_type.method)) {
                    request_or_notif.params = @unionInit(RequestParams, field.name, try parse(field.field_type, object.get("params").?, options));
                }
            }

            return request_or_notif;
        }
    };

    const first_message =
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": 10,
        \\    "method": "spaceship/amogus",
        \\    "params": {"sussy": true}
        \\}
    ;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const first_tree = try testing_parser.parse(first_message);
    const first_parsed = try parse(RequestOrNotification, first_tree.root, .{ .allocator = arena.allocator() });

    try std.testing.expectEqualStrings("2.0", first_parsed.jsonrpc);
    try std.testing.expect(first_parsed.id != null);
    try std.testing.expect(first_parsed.id.? == .integer);
    try std.testing.expectEqual(@as(i64, 10), first_parsed.id.?.integer);
    try std.testing.expectEqualStrings("spaceship/amogus", first_parsed.method);
    try std.testing.expect(first_parsed.params == .amogus);
    try std.testing.expectEqual(true, first_parsed.params.amogus.sussy);

    // TODO: Add second test
}

test "json.parse allocator required errors" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);

    try std.testing.expectError(error.AllocatorRequired, parse([]i64, (try testing_parser.parse("[1, 2, 3, 4]")).root, .{}));
    testing_parser.reset();
    try std.testing.expectError(error.AllocatorRequired, parse(std.StringArrayHashMap(i64), (try testing_parser.parse(
        \\{"a": 123, "b": -69}
    )).root, .{}));
}
