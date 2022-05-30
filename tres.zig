const std = @import("std");

fn isHashMap(comptime T: type) bool {
    if (!@hasDecl(T, "KV")) return false;

    if (!@hasField(T.KV, "key")) return false;
    if (!@hasField(T.KV, "value")) return false;

    const Key = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "key") orelse unreachable].field_type;
    const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].field_type;

    if (!@hasDecl(T, "init")) return false;
    if (!@hasDecl(T, "put")) return false;

    const init = @TypeOf(T.init);

    if (init != fn (std.mem.Allocator) T) return false;

    const put = @typeInfo(@TypeOf(T.put));

    if (put != .Fn) return false;

    if (put.Fn.args.len != 3) return false;
    if (put.Fn.args[0].arg_type.? != *T) return false;
    if (put.Fn.args[1].arg_type.? != Key) return false;
    if (put.Fn.args[2].arg_type.? != Value) return false;
    if (put.Fn.return_type == null) return false;

    const put_return = @typeInfo(put.Fn.return_type.?);
    if (put_return != .ErrorUnion) return false;
    if (put_return.ErrorUnion.payload != void) return false;

    return true;
}

pub fn parse(comptime T: type, tree: std.json.Value, allocator: ?std.mem.Allocator) ParseInternalError(T)!T {
    return try parseInternal(T, "root", @typeName(T), tree, allocator, false);
}

pub fn Undefinedable(comptime T: type) type {
    return struct {
        const __json_T = T;
        const __json_is_undefinedable = true;

        value: T,
        missing: bool,

        pub fn asOptional(self: @This()) ?T {
            return if (self.missing)
                null
            else
                self.value;
        }
    };
}

pub fn ParseInternalError(comptime T: type) type {
    // `inferred_types` is used to avoid infinite recursion for recursive type definitions.
    const inferred_types = [_]type{};
    return ParseInternalErrorImpl(T, &inferred_types);
}

fn ParseInternalErrorImpl(comptime T: type, comptime inferred_types: []const type) type {
    if (comptime std.meta.trait.isContainer(T) and @hasDecl(T, "tresParse")) {
        const tresParse_return = @typeInfo(@typeInfo(@TypeOf(T.tresParse)).Fn.return_type.?);
        if (tresParse_return == .ErrorUnion) {
            return tresParse_return.ErrorUnion.error_set;
        } else {
            return error{};
        }
    }

    for (inferred_types) |ty| {
        if (T == ty) return error{};
    }

    const inferred_set = inferred_types ++ [_]type{T};

    switch (@typeInfo(T)) {
        .Bool, .Float => return error{UnexpectedFieldType},
        .Int => return error{ UnexpectedFieldType, Overflow },
        .Optional => |info| return ParseInternalErrorImpl(info.child, inferred_set),
        .Enum => return error{ InvalidEnumTag, UnexpectedFieldType },
        .Union => |info| {
            var errors = error{UnexpectedFieldType};

            for (info.fields) |field| {
                errors = errors || ParseInternalErrorImpl(field.field_type, inferred_set);
            }

            return errors;
        },
        .Struct => |info| {
            var errors = error{
                UnexpectedFieldType,
                InvalidFieldValue,
                MissingRequiredField,
            };

            if (isAllocatorRequired(T)) {
                errors = errors || error{AllocatorRequired} || std.mem.Allocator.Error;
            }

            for (info.fields) |field| {
                errors = errors || ParseInternalErrorImpl(field.field_type, inferred_set);
            }

            return errors;
        },
        .Pointer => |info| {
            var errors = error{UnexpectedFieldType};

            if (isAllocatorRequired(T)) {
                errors = errors || error{AllocatorRequired} || std.mem.Allocator.Error;
            }

            if (info.size == .Slice and info.child == u8 or info.child == std.json.Value)
                return errors;

            errors = errors || ParseInternalErrorImpl(info.child, inferred_set);

            return errors;
        },
        .Array => |info| {
            var errors = error{UnexpectedFieldType};

            errors = errors || ParseInternalErrorImpl(info.child, inferred_set);

            return errors;
        },
        .Vector => |info| {
            var errors = error{UnexpectedFieldType};

            errors = errors || ParseInternalErrorImpl(info.child, inferred_set);

            return errors;
        },

        else => return error{},
    }
}

pub fn isAllocatorRequired(comptime T: type) bool {
    // `inferred_types` is used to avoid infinite recursion for recursive type definitions.
    const inferred_types = [_]type{};
    return isAllocatorRequiredImpl(T, &inferred_types);
}

fn isAllocatorRequiredImpl(comptime T: type, comptime inferred_types: []const type) bool {
    for (inferred_types) |ty| {
        if (T == ty) return false;
    }

    const inferred_set = inferred_types ++ [_]type{T};

    switch (@typeInfo(T)) {
        .Optional => |info| return isAllocatorRequiredImpl(info.child, inferred_set),
        .Union => |info| {
            for (info.fields) |field| {
                if (isAllocatorRequiredImpl(field.field_type, inferred_set))
                    return true;
            }
        },
        .Struct => |info| {
            if (isHashMap(T)) {
                if (T == std.json.ObjectMap)
                    return false;

                return true;
            }

            for (info.fields) |field| {
                if (@typeInfo(field.field_type) == .Struct and @hasDecl(field.field_type, "__json_is_undefinedable")) {
                    if (isAllocatorRequiredImpl(field.field_type.__json_T, inferred_set))
                        return true;
                } else if (isAllocatorRequiredImpl(field.field_type, inferred_set))
                    return true;
            }
        },
        .Pointer => |info| {
            if (info.size == .Slice and info.child == u8 or info.child == std.json.Value)
                return false;

            return true;
        },
        .Array => |info| {
            return isAllocatorRequiredImpl(info.child, inferred_set);
        },
        .Vector => |info| {
            return isAllocatorRequiredImpl(info.child, inferred_set); // is it even possible for this to be true?
        },
        else => {},
    }

    return false;
}

const logger = std.log.scoped(.json);
fn parseInternal(
    comptime T: type,
    comptime parent_name: []const u8,
    comptime field_name: []const u8,
    json_value: std.json.Value,
    maybe_allocator: ?std.mem.Allocator,
    comptime suppress_error_logs: bool,
) ParseInternalError(T)!T {
    // TODO: Revert name fixes when stage2 comes out
    // and properly memoizes comptime strings
    const name = parent_name ++ "." ++ field_name;

    if (T == std.json.Value) return json_value;
    if (comptime std.meta.trait.isContainer(T) and @hasDecl(T, "tresParse")) {
        return T.tresParse(json_value, maybe_allocator);
    }

    switch (@typeInfo(T)) {
        .Bool => {
            if (json_value == .Bool) {
                return json_value.Bool;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Bool, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Float => {
            if (json_value == .Float) {
                return @floatCast(T, json_value.Float);
            } else if (json_value == .Integer) {
                return @intToFloat(T, json_value.Integer);
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Float, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Int => {
            if (json_value == .Integer) {
                return std.math.cast(T, json_value.Integer) orelse return error.Overflow;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Integer, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Optional => |info| {
            if (json_value == .Null) {
                return null;
            } else {
                return try parseInternal(
                    info.child,
                    @typeName(T),
                    "?",
                    json_value,
                    maybe_allocator,
                    suppress_error_logs,
                );
            }
        },
        .Enum => {
            if (json_value == .Integer) {
                // we use this to convert signed to unsigned and check if it actually fits.
                const tag = std.math.cast(std.meta.Tag(T), json_value.Integer) orelse {
                    if (comptime !suppress_error_logs) logger.debug("invalid enum tag for {s}, found {d} at {s}", .{ @typeName(T), json_value.Integer, name });

                    return error.InvalidEnumTag;
                };

                return try std.meta.intToEnum(T, tag);
            } else if (json_value == .String) {
                return std.meta.stringToEnum(T, json_value.String) orelse {
                    if (comptime !suppress_error_logs) logger.debug("invalid enum tag for {s}, found '{s}' at {s}", .{ @typeName(T), json_value.String, name });

                    return error.InvalidEnumTag;
                };
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Integer or String, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Union => |info| {
            if (info.tag_type != null) {
                inline for (info.fields) |field| {
                    if (parseInternal(
                        field.field_type,
                        @typeName(T),
                        field.name,
                        json_value,
                        maybe_allocator,
                        true,
                    )) |parsed_value| {
                        return @unionInit(T, field.name, parsed_value);
                    } else |_| {}
                }

                if (comptime !suppress_error_logs) logger.debug("union fell through for {s}, found {s} at {s}", .{ @typeName(T), @tagName(json_value), name });

                return error.UnexpectedFieldType;
            } else {
                @compileError("cannot parse an untagged union: " ++ @typeName(T));
            }
        },
        .Struct => |info| {
            if (comptime isHashMap(T)) {
                const Key = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "key") orelse unreachable].field_type;
                const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].field_type;

                if (Key != []const u8) @compileError("HashMap key must be of type []const u8!");

                if (json_value == .Object) {
                    if (T == std.json.ObjectMap) return json_value.Object;

                    const allocator = maybe_allocator orelse return error.AllocatorRequired;

                    var map = T.init(allocator);
                    var map_iterator = json_value.Object.iterator();

                    while (map_iterator.next()) |entry| {
                        try map.put(entry.key_ptr.*, try parseInternal(
                            Value,
                            @typeName(T),
                            ".(hashmap entry)",
                            entry.value_ptr.*,
                            maybe_allocator,
                            suppress_error_logs,
                        ));
                    }

                    return map;
                } else {
                    if (comptime !suppress_error_logs) logger.debug("expected map of {s} at {s}, found {s}", .{ @typeName(Value), name, @tagName(json_value) });
                    return error.UnexpectedFieldType;
                }
            }

            if (info.is_tuple) {
                if (json_value != .Array) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array, found {s} at {s}", .{ @tagName(json_value), name });
                    return error.UnexpectedFieldType;
                }

                if (json_value.Array.items.len != std.meta.fields(T).len) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of Tuple {s} but it doesn't; at {s}", .{ @typeName(T), name });
                    return error.UnexpectedFieldType;
                }

                var tuple: T = undefined;
                comptime var index: usize = 0;

                inline while (index < std.meta.fields(T).len) : (index += 1) {
                    tuple[index] = try parseInternal(
                        std.meta.fields(T)[index].field_type,
                        @typeName(T),
                        comptime std.fmt.comptimePrint("[{d}]", .{index}),
                        json_value.Array.items[index],
                        maybe_allocator,
                        suppress_error_logs,
                    );
                }

                return tuple;
            }

            if (json_value == .Object) {
                var result: T = undefined;

                // Must use in order to bypass [#2727](https://github.com/ziglang/zig/issues/2727) :(
                var missing_field = false;

                inline for (info.fields) |field| {
                    const field_value = json_value.Object.get(field.name);

                    if (field.is_comptime) {
                        if (field_value == null) {
                            if (comptime !suppress_error_logs) logger.debug("comptime field {s}.{s} missing, at {s}", .{ @typeName(T), field.name, name });

                            return error.InvalidFieldValue;
                        }

                        if (field.default_value) |default| {
                            const parsed_value = try parseInternal(
                                field.field_type,
                                @typeName(T),
                                field.name,
                                field_value.?,
                                maybe_allocator,
                                suppress_error_logs,
                            );
                            const default_value = @ptrCast(*const field.field_type, default).*;

                            // NOTE: This only works for strings!
                            // TODODODODODODO ASAP
                            if (!std.mem.eql(u8, parsed_value, default_value)) {
                                if (comptime !suppress_error_logs) logger.debug("comptime field {s}.{s} does not match", .{ @typeName(T), field.name });

                                return error.InvalidFieldValue;
                            }
                        } else unreachable; // zig requires comptime fields to have a default initialization value
                    } else {
                        if (field_value) |fv| {
                            if (@typeInfo(field.field_type) == .Struct and @hasDecl(field.field_type, "__json_is_undefinedable"))
                                @field(result, field.name) = .{
                                    .value = try parseInternal(
                                        field.field_type.__json_T,
                                        @typeName(T),
                                        field.name,
                                        fv,
                                        maybe_allocator,
                                        suppress_error_logs,
                                    ),
                                    .missing = false,
                                }
                            else
                                @field(result, field.name) = try parseInternal(
                                    field.field_type,
                                    @typeName(T),
                                    field.name,
                                    fv,
                                    maybe_allocator,
                                    suppress_error_logs,
                                );
                        } else {
                            if (@typeInfo(field.field_type) == .Struct and @hasDecl(field.field_type, "__json_is_undefinedable")) {
                                @field(result, field.name) = .{
                                    .value = undefined,
                                    .missing = true,
                                };
                            } else if (field.default_value) |default| {
                                const default_value = @ptrCast(*const field.field_type, default).*;
                                @field(result, field.name) = default_value;
                            } else {
                                if (comptime !suppress_error_logs) logger.debug("required field {s}.{s} missing, at {s}", .{ @typeName(T), field.name, name });

                                missing_field = true;
                            }
                        }
                    }
                }

                if (missing_field) return error.MissingRequiredField;

                return result;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Object, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Pointer => |info| {
            if (info.size == .Slice) {
                if (info.child == u8) {
                    if (json_value == .String) {
                        return json_value.String;
                    } else {
                        if (comptime !suppress_error_logs) logger.debug("expected String, found {s} at {s}", .{ @tagName(json_value), name });

                        return error.UnexpectedFieldType;
                    }
                } else if (info.child == std.json.Value) {
                    return json_value.Array.items;
                }
            }

            const allocator = maybe_allocator orelse return error.AllocatorRequired;
            switch (info.size) {
                .Slice, .Many => {
                    const sentinel = if (info.sentinel) |ptr| @ptrCast(*const info.child, ptr).* else null;

                    if (info.child == u8 and json_value == .String) {
                        const array = try allocator.allocWithOptions(
                            info.child,
                            json_value.String.len,
                            info.alignment,
                            sentinel,
                        );

                        std.mem.copy(u8, array, json_value.String);

                        return @ptrCast(T, array);
                    }

                    if (json_value == .Array) {
                        if (info.child == std.json.Value) return json_value.Array.items;

                        const array = try allocator.allocWithOptions(
                            info.child,
                            json_value.Array.items.len,
                            info.alignment,
                            sentinel,
                        );

                        for (json_value.Array.items) |item, index|
                            array[index] = try parseInternal(
                                info.child,
                                @typeName(T),
                                "[...]",
                                item,
                                maybe_allocator,
                                suppress_error_logs,
                            );

                        return @ptrCast(T, array);
                    } else {
                        if (comptime !suppress_error_logs) logger.debug("expected Array, found {s} at {s}", .{ @tagName(json_value), name });

                        return error.UnexpectedFieldType;
                    }
                },
                .One, .C => {
                    const data = try allocator.allocAdvanced(info.child, info.alignment, 1, .exact);

                    data[0] = try parseInternal(
                        info.child,
                        @typeName(T),
                        "*",
                        json_value,
                        maybe_allocator,
                        suppress_error_logs,
                    );

                    return &data[0];
                },
            }
        },
        .Array => |info| {
            if (json_value == .Array) {
                var array: T = undefined;

                if (info.sentinel) |ptr| {
                    const sentinel = @ptrCast(*const info.child, ptr).*;

                    array[array.len] = sentinel;
                }

                if (json_value.Array.items.len != info.len) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of {s} but it doesn't; at {s}", .{ @typeName(T), name });
                    return error.UnexpectedFieldType;
                }

                for (array) |*item, index|
                    item.* = try parseInternal(
                        info.child,
                        @typeName(T),
                        "[...]",
                        json_value.Array.items[index],
                        maybe_allocator,
                        suppress_error_logs,
                    );

                return array;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Array, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
            }
        },
        .Vector => |info| {
            if (json_value == .Array) {
                var vector: T = undefined;

                if (json_value.Array.items.len != info.len) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of {s} ({d}) but it doesn't; at {s}", .{ @typeName(T), info.len, name });
                    return error.UnexpectedFieldType;
                }

                for (vector) |*item|
                    item.* = try parseInternal(
                        info.child,
                        @typeName(T),
                        "[...]",
                        item,
                        maybe_allocator,
                        suppress_error_logs,
                    );

                return vector;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Array, found {s} at {s}", .{ @tagName(json_value), name });

                return error.UnexpectedFieldType;
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
        players: std.StringHashMap(Player),

        my_tuple: MyTuple,
        my_array: [2]u8,

        a_pointer: *u8,
        a_weird_string: [*:0]u8,
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
        \\    "my_tuple": [10, false],
        \\    "my_array": [1, 255],
        \\    "a_pointer": 5,
        \\    "a_weird_string": "hello"
        \\}
    ;

    // NOTE: In practice, we're going to use an arena, thus no parseFree exists because it is not required :)
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const tree = try testing_parser.parse(json);

    const parsed = try parse(Struct, tree.root, arena.allocator());

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

    try std.testing.expectEqual([2]u8{ 1, 255 }, parsed.my_array);

    try std.testing.expectEqual(@as(u8, 5), parsed.a_pointer.*);

    try std.testing.expectEqualStrings("hello", std.mem.sliceTo(parsed.a_weird_string, 0));
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

    const parsed = parse(Struct, tree.root, arena.allocator());

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

    const parsed = try parse(Struct, tree.root, arena.allocator());

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
    const first_parsed = try parse(Message, first_tree.root, arena.allocator());

    try std.testing.expect(first_parsed == .youre_the_impostor);

    testing_parser.reset();

    const second_tree = try testing_parser.parse(second_message);
    const second_parsed = try parse(Message, second_tree.root, arena.allocator());

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

        pub fn tresParse(value: std.json.Value, allocator: ?std.mem.Allocator) RequestOrNotificationParseError()!Self {
            // var allocator = options.allocator orelse return error.AllocatorRequired;
            var object = value.Object;
            var request_or_notif: Self = undefined;

            request_or_notif.jsonrpc = object.get("jsonrpc").?.String;
            request_or_notif.id = if (object.get("id")) |id| try parse(RequestId, id, allocator) else null;
            request_or_notif.method = object.get("method").?.String;

            inline for (std.meta.fields(RequestParams)) |field| {
                if (std.mem.eql(u8, request_or_notif.method, field.field_type.method)) {
                    request_or_notif.params = @unionInit(RequestParams, field.name, try parse(field.field_type, object.get("params").?, allocator));
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
    const first_parsed = try parse(RequestOrNotification, first_tree.root, arena.allocator());

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

    try std.testing.expectError(error.AllocatorRequired, parse([]i64, (try testing_parser.parse("[1, 2, 3, 4]")).root, null));
    testing_parser.reset();
    try std.testing.expectError(error.AllocatorRequired, parse(std.StringArrayHashMap(i64), (try testing_parser.parse(
        \\{"a": 123, "b": -69}
    )).root, null));
}
