const std = @import("std");

/// Use after `isArrayList` and/or `isHashMap`
pub fn isManaged(comptime T: type) bool {
    return @hasField(T, "allocator");
}

pub fn isArrayList(comptime T: type) bool {
    // TODO: Improve this ArrayList check, specifically by actually checking the functions we use
    // TODO: Consider unmanaged ArrayLists
    if (!@hasField(T, "items")) return false;
    if (!@hasField(T, "capacity")) return false;

    return true;
}

pub fn isHashMap(comptime T: type) bool {
    // TODO: Consider unmanaged HashMaps

    if (!@hasDecl(T, "KV")) return false;

    if (!@hasField(T.KV, "key")) return false;
    if (!@hasField(T.KV, "value")) return false;

    const Key = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "key") orelse unreachable].type;
    const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].type;

    if (!@hasDecl(T, "put")) return false;

    const put = @typeInfo(@TypeOf(T.put));

    if (put != .Fn) return false;

    switch (put.Fn.params.len) {
        3 => {
            if (put.Fn.params[0].type.? != *T) return false;
            if (put.Fn.params[1].type.? != Key) return false;
            if (put.Fn.params[2].type.? != Value) return false;
        },
        4 => {
            if (put.Fn.params[0].type.? != *T) return false;
            if (put.Fn.params[1].type.? != std.mem.Allocator) return false;
            if (put.Fn.params[2].type.? != Key) return false;
            if (put.Fn.params[3].type.? != Value) return false;
        },
        else => return false,
    }

    if (put.Fn.return_type == null) return false;

    const put_return = @typeInfo(put.Fn.return_type.?);
    if (put_return != .ErrorUnion) return false;
    if (put_return.ErrorUnion.payload != void) return false;

    return true;
}

test "isManaged, isArrayList, isHashMap" {
    const T1 = std.ArrayList(u8);
    try std.testing.expect(isArrayList(T1) and isManaged(T1));
    const T2 = std.ArrayListUnmanaged(u8);
    try std.testing.expect(isArrayList(T2) and !isManaged(T2));

    const T3 = std.AutoHashMap(u8, u16);
    try std.testing.expect(isHashMap(T3) and isManaged(T3));
    const T4 = std.AutoHashMapUnmanaged(u8, u16);
    try std.testing.expect(isHashMap(T4) and !isManaged(T4));
}

/// Arena recommended.
pub fn parse(comptime T: type, tree: std.json.Value, allocator: ?std.mem.Allocator) ParseInternalError(T)!T {
    return try parseInternal(T, tree, allocator, false);
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

        pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            _ = fmt;
            _ = options;

            if (self.missing)
                try writer.print("Undefinedable({s}){{ missing }}", .{@typeName(T)})
            else {
                try writer.print("Undefinedable({s}){{ .value = {any} }}", .{ @typeName(T), self.value });
            }
        }
    };
}

const NullMeaning = enum {
    /// ?T; a null leads to the field not being written
    field,
    /// ?T; a null leads to the field being written with the value null
    value,
    /// ??T; first null is field, second null is value
    dual,
};

fn dualable(comptime T: type) bool {
    return @typeInfo(T) == .Optional and @typeInfo(@typeInfo(T).Optional.child) == .Optional;
}

// TODO: Respect stringify options
fn nullMeaning(comptime T: type, comptime field: std.builtin.Type.StructField) ?NullMeaning {
    const true_default = td: {
        if (dualable(T)) break :td NullMeaning.dual;
        break :td null;
    };
    if (!@hasDecl(T, "tres_null_meaning")) return true_default;
    const tnm = @field(T, "tres_null_meaning");
    if (!@hasField(@TypeOf(tnm), field.name)) return true_default;
    return @field(tnm, field.name);
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
                errors = errors || ParseInternalErrorImpl(field.type, inferred_set);
            }

            return errors;
        },
        .Struct => |info| {
            var errors = error{
                UnexpectedFieldType,
                InvalidFieldValue,
                MissingRequiredField,
            };

            if (isArrayList(T)) {
                const Child = std.meta.Child(@field(T, "Slice"));

                errors = errors || ParseInternalErrorImpl(Child, inferred_set);
            }

            if (isHashMap(T)) {
                const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].type;

                errors = errors || ParseInternalErrorImpl(Value, inferred_set);
            }

            if (isAllocatorRequired(T)) {
                errors = errors || error{AllocatorRequired} || std.mem.Allocator.Error;
            }

            for (info.fields) |field| {
                errors = errors || ParseInternalErrorImpl(field.type, inferred_set);
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
                if (isAllocatorRequiredImpl(field.type, inferred_set))
                    return true;
            }
        },
        .Struct => |info| {
            if (isArrayList(T)) {
                if (T == std.json.Array)
                    return false;

                return true;
            }

            if (isHashMap(T)) {
                if (T == std.json.ObjectMap)
                    return false;

                return true;
            }

            for (info.fields) |field| {
                if (@typeInfo(field.type) == .Struct and @hasDecl(field.type, "__json_is_undefinedable")) {
                    if (isAllocatorRequiredImpl(field.type.__json_T, inferred_set))
                        return true;
                } else if (isAllocatorRequiredImpl(field.type, inferred_set))
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
    json_value: std.json.Value,
    maybe_allocator: ?std.mem.Allocator,
    comptime suppress_error_logs: bool,
) ParseInternalError(T)!T {
    if (T == std.json.Value) return json_value;
    if (comptime std.meta.trait.isContainer(T) and @hasDecl(T, "tresParse")) {
        return T.tresParse(json_value, maybe_allocator);
    }

    switch (@typeInfo(T)) {
        .Bool => {
            if (json_value == .Bool) {
                return json_value.Bool;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Bool, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Float => {
            if (json_value == .Float) {
                return @floatCast(T, json_value.Float);
            } else if (json_value == .Integer) {
                return @intToFloat(T, json_value.Integer);
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Float, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Int => {
            if (json_value == .Integer) {
                return std.math.cast(T, json_value.Integer) orelse return error.Overflow;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Integer, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Optional => |info| {
            if (json_value == .Null) {
                return null;
            } else {
                return try parseInternal(
                    info.child,
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
                    if (comptime !suppress_error_logs) logger.debug("invalid enum tag for {s}, found {d}", .{ @typeName(T), json_value.Integer });

                    return error.InvalidEnumTag;
                };

                return try std.meta.intToEnum(T, tag);
            } else if (json_value == .String) {
                return std.meta.stringToEnum(T, json_value.String) orelse {
                    if (comptime !suppress_error_logs) logger.debug("invalid enum tag for {s}, found '{s}'", .{ @typeName(T), json_value.String });

                    return error.InvalidEnumTag;
                };
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Integer or String, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Union => |info| {
            if (info.tag_type != null) {
                inline for (info.fields) |field| {
                    if (parseInternal(
                        field.type,
                        json_value,
                        maybe_allocator,
                        true,
                    )) |parsed_value| {
                        return @unionInit(T, field.name, parsed_value);
                    } else |_| {}
                }

                if (comptime !suppress_error_logs) logger.debug("union fell through for {s}, found {s}", .{ @typeName(T), @tagName(json_value) });

                return error.UnexpectedFieldType;
            } else {
                @compileError("cannot parse an untagged union: " ++ @typeName(T));
            }
        },
        .Struct => |info| {
            if (comptime isArrayList(T)) {
                const Child = std.meta.Child(@field(T, "Slice"));

                if (json_value == .Array) {
                    if (T == std.json.Array) return json_value.Array;

                    const allocator = maybe_allocator orelse return error.AllocatorRequired;

                    var array_list = try T.initCapacity(allocator, json_value.Array.capacity);

                    for (json_value.Array.items) |item| {
                        if (comptime isManaged(T))
                            try array_list.append(try parseInternal(
                                Child,
                                item,
                                maybe_allocator,
                                suppress_error_logs,
                            ))
                        else
                            try array_list.append(allocator, try parseInternal(
                                Child,
                                item,
                                maybe_allocator,
                                suppress_error_logs,
                            ));
                    }

                    return array_list;
                } else {
                    if (comptime !suppress_error_logs) logger.debug("expected array of {s}, found {s}", .{ @typeName(Child), @tagName(json_value) });
                    return error.UnexpectedFieldType;
                }
            }

            if (comptime isHashMap(T)) {
                const managed = comptime isManaged(T);

                const Key = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "key") orelse unreachable].type;
                const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].type;

                if (Key != []const u8) @compileError("HashMap key must be of type []const u8!");

                if (json_value == .Object) {
                    if (T == std.json.ObjectMap) return json_value.Object;

                    const allocator = maybe_allocator orelse return error.AllocatorRequired;

                    var map: T = if (managed) T.init(allocator) else .{};
                    var map_iterator = json_value.Object.iterator();

                    while (map_iterator.next()) |entry| {
                        if (managed)
                            try map.put(entry.key_ptr.*, try parseInternal(
                                Value,
                                entry.value_ptr.*,
                                maybe_allocator,
                                suppress_error_logs,
                            ))
                        else
                            try map.put(allocator, entry.key_ptr.*, try parseInternal(
                                Value,
                                entry.value_ptr.*,
                                maybe_allocator,
                                suppress_error_logs,
                            ));
                    }

                    return map;
                } else {
                    if (comptime !suppress_error_logs) logger.debug("expected map of {s} found {s}", .{ @typeName(Value), @tagName(json_value) });
                    return error.UnexpectedFieldType;
                }
            }

            if (info.is_tuple) {
                if (json_value != .Array) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array, found {s}", .{@tagName(json_value)});
                    return error.UnexpectedFieldType;
                }

                if (json_value.Array.items.len != std.meta.fields(T).len) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of Tuple {s} but it doesn't", .{@typeName(T)});
                    return error.UnexpectedFieldType;
                }

                var tuple: T = undefined;
                comptime var index: usize = 0;

                inline while (index < std.meta.fields(T).len) : (index += 1) {
                    tuple[index] = try parseInternal(
                        std.meta.fields(T)[index].type,
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
                    const nm = nullMeaning(T, field) orelse .value;

                    const field_value = json_value.Object.get(field.name);

                    if (field.is_comptime) {
                        if (field_value == null) {
                            if (comptime !suppress_error_logs) logger.debug("comptime field {s}.{s} missing", .{ @typeName(T), field.name });

                            return error.InvalidFieldValue;
                        }

                        if (field.default_value) |default| {
                            const parsed_value = try parseInternal(
                                field.type,
                                field_value.?,
                                maybe_allocator,
                                suppress_error_logs,
                            );
                            const default_value = @ptrCast(*const field.type, @alignCast(@alignOf(field.type), default)).*;

                            // NOTE: This only works for strings!
                            // TODODODODODODO ASAP
                            if (!std.mem.eql(u8, parsed_value, default_value)) {
                                if (comptime !suppress_error_logs) logger.debug("comptime field {s}.{s} does not match", .{ @typeName(T), field.name });

                                return error.InvalidFieldValue;
                            }
                        } else unreachable; // zig requires comptime fields to have a default initialization value
                    } else if (comptime dualable(field.type) and nm == .dual) {
                        if (field_value == null) {
                            @field(result, field.name) = null;
                        } else {
                            @field(result, field.name) = try parseInternal(@typeInfo(@TypeOf(@field(result, field.name))).Optional.child, field_value.?, maybe_allocator, suppress_error_logs);
                        }
                    } else {
                        if (field_value) |fv| {
                            if (@typeInfo(field.type) == .Struct and @hasDecl(field.type, "__json_is_undefinedable"))
                                @field(result, field.name) = .{
                                    .value = try parseInternal(
                                        field.type.__json_T,
                                        fv,
                                        maybe_allocator,
                                        suppress_error_logs,
                                    ),
                                    .missing = false,
                                }
                            else
                                @field(result, field.name) = try parseInternal(
                                    field.type,
                                    fv,
                                    maybe_allocator,
                                    suppress_error_logs,
                                );
                        } else {
                            if (@typeInfo(field.type) == .Struct and @hasDecl(field.type, "__json_is_undefinedable")) {
                                @field(result, field.name) = .{
                                    .value = undefined,
                                    .missing = true,
                                };
                            } else if (field.default_value) |default| {
                                const default_value = @ptrCast(*const field.type, @alignCast(@alignOf(field.type), default)).*;
                                @field(result, field.name) = default_value;
                            } else if (@typeInfo(field.type) == .Optional and nm == .field) {
                                @field(result, field.name) = null;
                            } else {
                                if (comptime !suppress_error_logs) logger.debug("required field {s}.{s} missing", .{ @typeName(T), field.name });

                                missing_field = true;
                            }
                        }
                    }
                }

                if (missing_field) return error.MissingRequiredField;

                return result;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Object, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Pointer => |info| {
            if (info.size == .Slice) {
                if (info.child == u8) {
                    if (json_value == .String) {
                        return json_value.String;
                    } else {
                        if (comptime !suppress_error_logs) logger.debug("expected String, found {s}", .{@tagName(json_value)});

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
                                item,
                                maybe_allocator,
                                suppress_error_logs,
                            );

                        return @ptrCast(T, array);
                    } else {
                        if (comptime !suppress_error_logs) logger.debug("expected Array, found {s}", .{@tagName(json_value)});

                        return error.UnexpectedFieldType;
                    }
                },
                .One, .C => {
                    const data = try allocator.allocWithOptions(info.child, 1, info.alignment, null);

                    data[0] = try parseInternal(
                        info.child,
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
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of {s} but it doesn't", .{@typeName(T)});
                    return error.UnexpectedFieldType;
                }

                for (array) |*item, index|
                    item.* = try parseInternal(
                        info.child,
                        json_value.Array.items[index],
                        maybe_allocator,
                        suppress_error_logs,
                    );

                return array;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Array, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Vector => |info| {
            if (json_value == .Array) {
                var vector: T = undefined;

                if (json_value.Array.items.len != info.len) {
                    if (comptime !suppress_error_logs) logger.debug("expected Array to match length of {s} ({d}) but it doesn't", .{ @typeName(T), info.len });
                    return error.UnexpectedFieldType;
                }

                for (vector) |*item|
                    item.* = try parseInternal(
                        info.child,
                        item,
                        maybe_allocator,
                        suppress_error_logs,
                    );

                return vector;
            } else {
                if (comptime !suppress_error_logs) logger.debug("expected Array, found {s}", .{@tagName(json_value)});

                return error.UnexpectedFieldType;
            }
        },
        .Void => return,
        else => {
            @compileError("unhandled json type: " ++ @typeName(T));
        },
    }
}

fn outputUnicodeEscape(
    codepoint: u21,
    out_stream: anytype,
) !void {
    if (codepoint <= 0xFFFF) {
        // If the character is in the Basic Multilingual Plane (U+0000 through U+FFFF),
        // then it may be represented as a six-character sequence: a reverse solidus, followed
        // by the lowercase letter u, followed by four hexadecimal digits that encode the character's code point.
        try out_stream.writeAll("\\u");
        try std.fmt.formatIntValue(codepoint, "x", std.fmt.FormatOptions{ .width = 4, .fill = '0' }, out_stream);
    } else {
        std.debug.assert(codepoint <= 0x10FFFF);
        // To escape an extended character that is not in the Basic Multilingual Plane,
        // the character is represented as a 12-character sequence, encoding the UTF-16 surrogate pair.
        const high = @intCast(u16, (codepoint - 0x10000) >> 10) + 0xD800;
        const low = @intCast(u16, codepoint & 0x3FF) + 0xDC00;
        try out_stream.writeAll("\\u");
        try std.fmt.formatIntValue(high, "x", std.fmt.FormatOptions{ .width = 4, .fill = '0' }, out_stream);
        try out_stream.writeAll("\\u");
        try std.fmt.formatIntValue(low, "x", std.fmt.FormatOptions{ .width = 4, .fill = '0' }, out_stream);
    }
}

fn outputJsonString(value: []const u8, options: std.json.StringifyOptions, out_stream: anytype) !void {
    try out_stream.writeByte('\"');
    var i: usize = 0;
    while (i < value.len) : (i += 1) {
        switch (value[i]) {
            // normal ascii character
            0x20...0x21, 0x23...0x2E, 0x30...0x5B, 0x5D...0x7F => |c| try out_stream.writeByte(c),
            // only 2 characters that *must* be escaped
            '\\' => try out_stream.writeAll("\\\\"),
            '\"' => try out_stream.writeAll("\\\""),
            // solidus is optional to escape
            '/' => {
                if (options.string.String.escape_solidus) {
                    try out_stream.writeAll("\\/");
                } else {
                    try out_stream.writeByte('/');
                }
            },
            // control characters with short escapes
            // TODO: option to switch between unicode and 'short' forms?
            0x8 => try out_stream.writeAll("\\b"),
            0xC => try out_stream.writeAll("\\f"),
            '\n' => try out_stream.writeAll("\\n"),
            '\r' => try out_stream.writeAll("\\r"),
            '\t' => try out_stream.writeAll("\\t"),
            else => {
                const ulen = std.unicode.utf8ByteSequenceLength(value[i]) catch unreachable;
                // control characters (only things left with 1 byte length) should always be printed as unicode escapes
                if (ulen == 1 or options.string.String.escape_unicode) {
                    const codepoint = std.unicode.utf8Decode(value[i .. i + ulen]) catch unreachable;
                    try outputUnicodeEscape(codepoint, out_stream);
                } else {
                    try out_stream.writeAll(value[i .. i + ulen]);
                }
                i += ulen - 1;
            },
        }
    }
    try out_stream.writeByte('\"');
}

pub fn stringify(
    value: anytype,
    options: std.json.StringifyOptions,
    out_stream: anytype,
) @TypeOf(out_stream).Error!void {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .Float, .ComptimeFloat => {
            return std.fmt.formatFloatScientific(value, std.fmt.FormatOptions{}, out_stream);
        },
        .Int, .ComptimeInt => {
            return std.fmt.formatIntValue(value, "", std.fmt.FormatOptions{}, out_stream);
        },
        .Bool => {
            return out_stream.writeAll(if (value) "true" else "false");
        },
        .Null => {
            return out_stream.writeAll("null");
        },
        .Optional => {
            if (value) |payload| {
                return try stringify(payload, options, out_stream);
            } else {
                return try stringify(null, options, out_stream);
            }
        },
        .Enum => {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            if (@hasDecl(T, "tres_string_enum")) {
                return try stringify(@tagName(value), options, out_stream);
            } else {
                return try stringify(@enumToInt(value), options, out_stream);
            }
        },
        .Union => {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            const info = @typeInfo(T).Union;
            if (info.tag_type) |UnionTagType| {
                inline for (info.fields) |u_field| {
                    if (value == @field(UnionTagType, u_field.name)) {
                        return try stringify(@field(value, u_field.name), options, out_stream);
                    }
                }
                return;
            } else {
                @compileError("Unable to stringify untagged union '" ++ @typeName(T) ++ "'");
            }
        },
        .Struct => |S| {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            if (comptime isArrayList(T)) {
                return stringify(value.items, options, out_stream);
            }

            try out_stream.writeByte('{');
            var field_output = false;
            var child_options = options;
            if (child_options.whitespace) |*child_whitespace| {
                child_whitespace.indent_level += 1;
            }

            if (comptime isHashMap(T)) {
                var iterator = value.iterator();

                while (iterator.next()) |entry| {
                    if (!field_output) {
                        field_output = true;
                    } else {
                        try out_stream.writeByte(',');
                    }
                    if (child_options.whitespace) |child_whitespace| {
                        try child_whitespace.outputIndent(out_stream);
                    }
                    try outputJsonString(entry.key_ptr.*, options, out_stream);
                    try out_stream.writeByte(':');
                    if (child_options.whitespace) |child_whitespace| {
                        if (child_whitespace.separator) {
                            try out_stream.writeByte(' ');
                        }
                    }
                    try stringify(entry.value_ptr.*, child_options, out_stream);
                }
            } else {
                inline for (S.fields) |Field| {
                    const nm = nullMeaning(T, Field) orelse (if (options.emit_null_optional_fields) NullMeaning.value else NullMeaning.field);

                    // don't include void fields
                    if (Field.type == void) continue;

                    var emit_field = true;

                    // don't include optional fields that are null when emit_null_optional_fields is set to false
                    if (@typeInfo(Field.type) == .Optional) {
                        if (nm == .field or nm == .dual) {
                            if (@field(value, Field.name) == null) {
                                emit_field = false;
                            }
                        }
                    }

                    const is_undefinedable = comptime @typeInfo(@TypeOf(@field(value, Field.name))) == .Struct and @hasDecl(@TypeOf(@field(value, Field.name)), "__json_is_undefinedable");
                    if (is_undefinedable) {
                        if (@field(value, Field.name).missing)
                            emit_field = false;
                    }

                    if (emit_field) {
                        if (!field_output) {
                            field_output = true;
                        } else {
                            try out_stream.writeByte(',');
                        }
                        if (child_options.whitespace) |child_whitespace| {
                            try child_whitespace.outputIndent(out_stream);
                        }
                        try outputJsonString(Field.name, options, out_stream);
                        try out_stream.writeByte(':');
                        if (child_options.whitespace) |child_whitespace| {
                            if (child_whitespace.separator) {
                                try out_stream.writeByte(' ');
                            }
                        }

                        if (is_undefinedable) {
                            try stringify(@field(value, Field.name).value, child_options, out_stream);
                        } else if (comptime dualable(Field.type) and nm == .dual)
                            try stringify(@field(value, Field.name).?, child_options, out_stream)
                        else {
                            try stringify(@field(value, Field.name), child_options, out_stream);
                        }
                    }
                }
            }

            if (field_output) {
                if (options.whitespace) |whitespace| {
                    try whitespace.outputIndent(out_stream);
                }
            }
            try out_stream.writeByte('}');
            return;
        },
        .ErrorSet => return stringify(@as([]const u8, @errorName(value)), options, out_stream),
        .Pointer => |ptr_info| switch (ptr_info.size) {
            .One => switch (@typeInfo(ptr_info.child)) {
                .Array => {
                    const Slice = []const std.meta.Elem(ptr_info.child);
                    return stringify(@as(Slice, value), options, out_stream);
                },
                else => {
                    // TODO: avoid loops?
                    return stringify(value.*, options, out_stream);
                },
            },
            // TODO: .Many when there is a sentinel (waiting for https://github.com/ziglang/zig/pull/3972)
            .Slice => {
                if (ptr_info.child == u8 and options.string == .String and std.unicode.utf8ValidateSlice(value)) {
                    try outputJsonString(value, options, out_stream);
                    return;
                }

                try out_stream.writeByte('[');
                var child_options = options;
                if (child_options.whitespace) |*whitespace| {
                    whitespace.indent_level += 1;
                }
                for (value) |x, i| {
                    if (i != 0) {
                        try out_stream.writeByte(',');
                    }
                    if (child_options.whitespace) |child_whitespace| {
                        try out_stream.writeByte('\n');
                        try child_whitespace.outputIndent(out_stream);
                    }
                    try stringify(x, child_options, out_stream);
                }
                if (value.len != 0) {
                    if (options.whitespace) |whitespace| {
                        try out_stream.writeByte('\n');
                        try whitespace.outputIndent(out_stream);
                    }
                }
                try out_stream.writeByte(']');
                return;
            },
            else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'"),
        },
        .Array => return stringify(&value, options, out_stream),
        .Vector => |info| {
            const array: [info.len]info.child = value;
            return stringify(&array, options, out_stream);
        },
        .Void => return try out_stream.writeAll("{}"),
        else => @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'"),
    }
    unreachable;
}

pub const ToValueOptions = struct {
    copy_strings: bool = false,
    // TODO: Add string options
};

/// Arena recommended.
pub fn toValue(
    allocator: std.mem.Allocator,
    value: anytype,
    options: ToValueOptions,
) std.mem.Allocator.Error!std.json.Value {
    const T = @TypeOf(value);

    if (T == std.json.Value) return value;
    if (comptime std.meta.trait.isContainer(T) and @hasDecl(T, "tresParse")) {
        return T.tresToValue(allocator, value, options);
    }

    switch (@typeInfo(T)) {
        .Bool => {
            return .{
                .Bool = value,
            };
        },
        .Float => {
            return .{
                .Float = value,
            };
        },
        .Int => |i| {
            return if (i.bits > 64) .{
                .NumberString = std.fmt.allocPrint(allocator, "{d}", .{value}),
            } else .{
                .Integer = value,
            };
        },
        .Optional => {
            return if (value) |val|
                toValue(allocator, val, options)
            else
                .Null;
        },
        .Enum => {
            return if (@hasDecl(T, "tres_string_enum")) .{ .String = @tagName(value) } else toValue(allocator, @enumToInt(value), options);
        },
        .Union => |info| {
            if (info.tag_type != null) {
                inline for (info.fields) |field| {
                    if (@field(T, field.name) == value) {
                        return toValue(allocator, @field(value, field.name), options);
                    }
                }

                unreachable;
            } else {
                @compileError("cannot toValue an untagged union: " ++ @typeName(T));
            }
        },
        .Struct => |info| {
            if (comptime isArrayList(T)) {
                const Child = std.meta.Child(@field(T, "Slice"));

                if (Child == u8) {
                    return .{ .String = if (options.copy_strings)
                        try allocator.dupe(u8, value.items)
                    else
                        value.items };
                } else {
                    var arr = std.json.Array.initCapacity(allocator, value.items);
                    for (value.items) |item| try arr.append(try toValue(allocator, item, options));
                    return .{ .Array = arr };
                }
            }

            if (comptime isHashMap(T)) {
                const Key = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "key") orelse unreachable].type;
                // const Value = std.meta.fields(T.KV)[std.meta.fieldIndex(T.KV, "value") orelse unreachable].type;

                if (Key != []const u8) @compileError("HashMap key must be of type []const u8!");

                var obj = std.json.ObjectMap.init(allocator);
                var it = value.iterator();
                while (it.next()) |entry| {
                    try obj.put(if (options.copy_strings)
                        try allocator.dupe(u8, entry.key_ptr.*)
                    else
                        entry.key_ptr.*, try toValue(allocator, entry.value_ptr.*, options));
                }
                return .{ .Object = obj };
            }

            if (info.is_tuple) {
                var arr = std.json.Array.initCapacity(allocator, info.fields.len);
                inline for (value) |item| try arr.append(try toValue(allocator, item, options));
                return .{ .Array = arr };
            }

            var obj = std.json.ObjectMap.init(allocator);

            inline for (info.fields) |field| {
                const field_val = @field(value, field.name);
                const nm = nullMeaning(T, field) orelse .value;

                if (field.is_comptime) {
                    if (field.default_value) |default| {
                        const default_value = @ptrCast(*const field.type, @alignCast(@alignOf(field.type), default)).*;
                        try obj.put(field.name, try toValue(allocator, default_value, options));
                    } else unreachable; // zig requires comptime fields to have a default initialization value
                } else if (comptime dualable(field.type) and nm == .dual) {
                    if (field_val) |val| {
                        if (val) |val2| {
                            try obj.put(field.name, try toValue(allocator, val2, options));
                        } else try obj.put(field.name, .Null);
                    }
                } else if (@typeInfo(field.type) == .Optional and nm == .field) {
                    if (field_val) |val| {
                        try obj.put(field.name, try toValue(allocator, val, options));
                    }
                } else if (@typeInfo(field.type) == .Struct and @hasDecl(field.type, "__json_is_undefinedable")) {
                    if (!field_val.missing) {
                        try obj.put(field.name, try toValue(allocator, field_val.value, options));
                    }
                } else {
                    try obj.put(field.name, try toValue(allocator, field_val, options));
                }
            }

            return .{ .Object = obj };
        },
        // .Pointer => |info| {
        //     if (info.size == .Slice) {
        //         if (info.child == u8) {
        //             if (json_value == .String) {
        //                 return json_value.String;
        //             } else {
        //                 if (comptime !suppress_error_logs) logger.debug("expected String, found {s}", .{@tagName(json_value)});

        //                 return error.UnexpectedFieldType;
        //             }
        //         } else if (info.child == std.json.Value) {
        //             return json_value.Array.items;
        //         }
        //     }

        //     const allocator = maybe_allocator orelse return error.AllocatorRequired;
        //     switch (info.size) {
        //         .Slice, .Many => {
        //             const sentinel = if (info.sentinel) |ptr| @ptrCast(*const info.child, ptr).* else null;

        //             if (info.child == u8 and json_value == .String) {
        //                 const array = try allocator.allocWithOptions(
        //                     info.child,
        //                     json_value.String.len,
        //                     info.alignment,
        //                     sentinel,
        //                 );

        //                 std.mem.copy(u8, array, json_value.String);

        //                 return @ptrCast(T, array);
        //             }

        //             if (json_value == .Array) {
        //                 if (info.child == std.json.Value) return json_value.Array.items;

        //                 const array = try allocator.allocWithOptions(
        //                     info.child,
        //                     json_value.Array.items.len,
        //                     info.alignment,
        //                     sentinel,
        //                 );

        //                 for (json_value.Array.items) |item, index|
        //                     array[index] = try parseInternal(
        //                         info.child,
        //                         item,
        //                         maybe_allocator,
        //                         suppress_error_logs,
        //                     );

        //                 return @ptrCast(T, array);
        //             } else {
        //                 if (comptime !suppress_error_logs) logger.debug("expected Array, found {s}", .{@tagName(json_value)});

        //                 return error.UnexpectedFieldType;
        //             }
        //         },
        //         .One, .C => {
        //             const data = try allocator.allocWithOptions(info.child, 1, info.alignment, null);

        //             data[0] = try parseInternal(
        //                 info.child,
        //                 json_value,
        //                 maybe_allocator,
        //                 suppress_error_logs,
        //             );

        //             return &data[0];
        //         },
        //     }
        // },
        .Array => |info| {
            const l = info.len + if (info.sentinel) 1 else 0;
            var arr = try std.json.Array.initCapacity(allocator, l);
            arr.items.len = l;

            if (info.sentinel) |ptr| {
                const sentinel = @ptrCast(*const info.child, ptr).*;

                arr.items[l - 1] = sentinel;
            }

            for (arr) |*item, index|
                item.* = try toValue(allocator, value[index], options);

            return arr;
        },
        .Vector => |info| {
            var arr = try std.json.Array.initCapacity(allocator, info.len);
            arr.items.len = info.len;

            for (arr.items) |*item, i|
                item.* = try toValue(allocator, value[i], options);

            return arr;
        },
        .Void => return .{ .Object = std.json.ObjectMap.init(allocator) },
        else => {
            @compileError("unhandled json type: " ++ @typeName(T));
        },
    }
}

const FullStruct = struct {
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

    bingus: std.StringHashMapUnmanaged(u8),
    dumbo_shrimp: std.ArrayListUnmanaged([]const u8),

    my_tuple: MyTuple,
    my_array: [2]u8,
    my_array_of_any: [2]std.json.Value,
    my_array_list: std.ArrayList(i64),
    my_array_list_of_any: std.json.Array,

    a_pointer: *u8,
    a_weird_string: [*:0]u8,
};

test "json.parse simple struct" {
    @setEvalBranchQuota(10_000);

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
        \\    "bingus": {"bingus1": 10, "bingus2": 25},
        \\    "dumbo_shrimp": ["Me", "You", "Everybody"],
        \\    "my_tuple": [10, false],
        \\    "my_array": [1, 255],
        \\    "my_array_of_any": ["a", 2],
        \\    "my_array_list": [2, 254],
        \\    "my_array_list_of_any": ["b", 3],
        \\    "a_pointer": 5,
        \\    "a_weird_string": "hello"
        \\}
    ;

    // NOTE: In practice, we're going to use an arena, thus no parseFree exists because it is not required :)
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);
    const tree = try testing_parser.parse(json);

    const parsed = try parse(FullStruct, tree.root, arena.allocator());

    try std.testing.expectEqual(true, parsed.bool_true);
    try std.testing.expectEqual(false, parsed.bool_false);
    try std.testing.expectEqual(@as(u8, 100), parsed.integer);
    try std.testing.expectApproxEqRel(@as(f64, 4.2069), parsed.float, std.math.epsilon(f64));
    try std.testing.expectEqual(@as(?f32, null), parsed.optional);
    try std.testing.expectEqual(FullStruct.Role.impostor, parsed.an_enum);
    try std.testing.expectEqual(FullStruct.Role.crewmate, parsed.an_enum_string);
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

    try std.testing.expectEqual(@as(u8, 10), parsed.bingus.get("bingus1").?);
    try std.testing.expectEqual(@as(u8, 25), parsed.bingus.get("bingus2").?);

    try std.testing.expectEqualStrings("Me", parsed.dumbo_shrimp.items[0]);
    try std.testing.expectEqualStrings("You", parsed.dumbo_shrimp.items[1]);
    try std.testing.expectEqualStrings("Everybody", parsed.dumbo_shrimp.items[2]);

    try std.testing.expectEqual(FullStruct.MyTuple{ 10, false }, parsed.my_tuple);

    try std.testing.expectEqual([2]u8{ 1, 255 }, parsed.my_array);

    try std.testing.expect(parsed.my_array_of_any[0] == .String);
    try std.testing.expectEqualStrings("a", parsed.my_array_of_any[0].String);
    try std.testing.expect(parsed.my_array_of_any[1] == .Integer);
    try std.testing.expectEqual(@as(i64, 2), parsed.my_array_of_any[1].Integer);

    try std.testing.expectEqual(@as(usize, 2), parsed.my_array_list.items.len);
    try std.testing.expectEqualSlices(i64, &[_]i64{ 2, 254 }, parsed.my_array_list.items);

    try std.testing.expectEqual(@as(usize, 2), parsed.my_array_list_of_any.items.len);
    try std.testing.expect(parsed.my_array_list_of_any.items[0] == .String);
    try std.testing.expectEqualStrings("b", parsed.my_array_list_of_any.items[0].String);
    try std.testing.expect(parsed.my_array_list_of_any.items[1] == .Integer);
    try std.testing.expectEqual(@as(i64, 3), parsed.my_array_list_of_any.items[1].Integer);

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
                err = err || ParseInternalError(field.type);
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
                if (std.mem.eql(u8, request_or_notif.method, field.type.method)) {
                    request_or_notif.params = @unionInit(RequestParams, field.name, try parse(field.type, object.get("params").?, allocator));
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

test "json.stringify basics" {
    var stringify_buf: [28]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    const Basic = struct {
        unsigned: u16,
        signed: i16,
    };

    var basic = Basic{
        .unsigned = 69,
        .signed = -69,
    };

    try stringify(basic, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\{"unsigned":69,"signed":-69}
    , &stringify_buf);
}

test "json.stringify undefinedables" {
    var furry_buf: [49]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&furry_buf);

    const Furry = struct {
        name: Undefinedable([]const u8),
        age: Undefinedable(i64),
        plays_amogus: bool,
        joe: Undefinedable([]const u8),
    };

    var rimu = Furry{
        .name = .{ .value = "Rimu", .missing = false },
        .age = .{ .value = undefined, .missing = true },
        .plays_amogus = false,
        .joe = .{ .value = "Mama", .missing = false },
    };

    try stringify(rimu, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\{"name":"Rimu","plays_amogus":false,"joe":"Mama"}
    , &furry_buf);
}

test "json.stringify arraylist" {
    const allocator = std.testing.allocator;

    var stringify_buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    const Database = struct {
        names_of_my_pals: std.ArrayList([]const u8),
        more_peeps: std.ArrayListUnmanaged([]const u8),
    };

    var db = Database{
        .names_of_my_pals = std.ArrayList([]const u8).init(allocator),
        .more_peeps = .{},
    };
    defer db.names_of_my_pals.deinit();
    defer db.more_peeps.deinit(allocator);

    try db.names_of_my_pals.append("Travis");
    try db.names_of_my_pals.append("Rimu");
    try db.names_of_my_pals.append("Flandere");

    try db.more_peeps.append(allocator, "Matt");
    try db.more_peeps.append(allocator, "Felix");
    try db.more_peeps.append(allocator, "Ben");

    try stringify(db, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\{"names_of_my_pals":["Travis","Rimu","Flandere"],"more_peeps":["Matt","Felix","Ben"]}
    , fbs.getWritten());
}

test "json.stringify hashmaps" {
    const allocator = std.testing.allocator;

    var stringify_buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    const Database = struct {
        coolness: std.StringHashMap(f64),
        height: std.StringHashMapUnmanaged(usize),
    };

    var db = Database{
        .coolness = std.StringHashMap(f64).init(allocator),
        .height = .{},
    };
    defer db.coolness.deinit();
    defer db.height.deinit(allocator);

    try db.coolness.put("Montreal", -20);
    try db.coolness.put("Beirut", 20);

    try db.height.put(allocator, "Hudson", 0);
    try db.height.put(allocator, "Me", 100_000);

    try stringify(db, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\{"coolness":{"Montreal":-2.0e+01,"Beirut":2.0e+01},"height":{"Me":100000,"Hudson":0}}
    , fbs.getWritten());
}

test "json.stringify enums" {
    const NumericEnum = enum(u8) {
        a = 0,
        b = 1,
    };

    const StringEnum = enum(u64) {
        const tres_string_enum = {};

        a = 0,
        b = 1,
    };

    var stringify_buf: [51]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    try stringify(NumericEnum.a, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\0
    , fbs.getWritten());

    try fbs.seekTo(0);

    try stringify(StringEnum.a, .{}, fbs.writer());

    try std.testing.expectEqualStrings(
        \\"a"
    , fbs.getWritten());
}

test "parse and stringify null meaning" {
    const A = struct {
        pub const tres_null_meaning = .{
            .a = .field,
            .b = .value,
            .c = .dual,
        };

        a: ?u8,
        b: ?u8,
        c: ??u8,
    };

    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var testing_parser = std.json.Parser.init(arena.allocator(), false);

    var stringify_buf: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    try stringify(A{ .a = null, .b = null, .c = null }, .{}, fbs.writer());
    try std.testing.expectEqualStrings(
        \\{"b":null}
    , fbs.getWritten());

    const a1 = try parse(A, (try testing_parser.parse(fbs.getWritten())).root, allocator);
    try std.testing.expectEqual(@as(?u8, null), a1.a);
    try std.testing.expectEqual(@as(?u8, null), a1.b);
    try std.testing.expectEqual(@as(??u8, null), a1.c);

    fbs.reset();
    testing_parser.reset();

    try stringify(A{ .a = 5, .b = 7, .c = @as(?u8, null) }, .{}, fbs.writer());
    try std.testing.expectEqualStrings(
        \\{"a":5,"b":7,"c":null}
    , fbs.getWritten());

    const a2 = try parse(A, (try testing_parser.parse(fbs.getWritten())).root, allocator);
    try std.testing.expectEqual(@as(u8, 5), a2.a.?);
    try std.testing.expectEqual(@as(u8, 7), a2.b.?);
    try std.testing.expectEqual(@as(?u8, null), a2.c.?);

    fbs.reset();
    testing_parser.reset();

    try stringify(A{ .a = 5, .b = 7, .c = 10 }, .{}, fbs.writer());
    try std.testing.expectEqualStrings(
        \\{"a":5,"b":7,"c":10}
    , fbs.getWritten());

    const a3 = try parse(A, (try testing_parser.parse(fbs.getWritten())).root, allocator);
    try std.testing.expectEqual(@as(u8, 5), a3.a.?);
    try std.testing.expectEqual(@as(u8, 7), a3.b.?);
    try std.testing.expectEqual(@as(u8, 10), a3.c.?.?);

    fbs.reset();
    testing_parser.reset();
}

test "custom standard stringify" {
    const Bruh = struct {
        pub fn jsonStringify(
            _: @This(),
            _: std.json.StringifyOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try writer.writeAll("slay");
        }
    };

    var stringify_buf: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&stringify_buf);

    try stringify(Bruh{}, .{}, fbs.writer());

    try std.testing.expectEqualStrings("slay", fbs.getWritten());
}

test "json.toValue: basics" {
    const allocator = std.testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const bool_simple = try toValue(arena.allocator(), @as(bool, false), .{});
    try std.testing.expect(bool_simple == .Bool);
    try std.testing.expectEqual(false, bool_simple.Bool);

    const float_simple = try toValue(arena.allocator(), @as(f64, 7.89), .{});
    try std.testing.expect(float_simple == .Float);
    try std.testing.expectEqual(@as(f64, 7.89), float_simple.Float);

    const int_simple = try toValue(arena.allocator(), @as(i64, 10), .{});
    try std.testing.expect(int_simple == .Integer);
    try std.testing.expectEqual(@as(i64, 10), int_simple.Integer);

    const optional_simple_1 = try toValue(arena.allocator(), @as(?f64, 7.89), .{});
    try std.testing.expect(optional_simple_1 == .Float);
    try std.testing.expectEqual(@as(f64, 7.89), optional_simple_1.Float);

    const optional_simple_2 = try toValue(arena.allocator(), @as(?f64, null), .{});
    try std.testing.expect(optional_simple_2 == .Null);

    const SimpleEnum1 = enum(u32) { a = 0, b = 69, c = 420, d = 42069 };
    const simple_enum_1 = try toValue(arena.allocator(), SimpleEnum1.b, .{});
    try std.testing.expect(simple_enum_1 == .Integer);
    try std.testing.expectEqual(@as(i64, 69), simple_enum_1.Integer);

    const SimpleEnum2 = enum(u32) {
        pub const tres_string_enum = .{};

        a = 0,
        b = 69,
        c = 420,
        d = 42069,
    };

    const simple_enum_2 = try toValue(arena.allocator(), SimpleEnum2.b, .{});
    try std.testing.expect(simple_enum_2 == .String);
    try std.testing.expectEqualStrings("b", simple_enum_2.String);

    const SimpleUnion = union(enum) {
        a: i64,
        b: bool,
    };

    const simple_union_1 = try toValue(arena.allocator(), SimpleUnion{ .a = 25 }, .{});
    try std.testing.expect(simple_union_1 == .Integer);
    try std.testing.expectEqual(@as(i64, 25), simple_union_1.Integer);

    const simple_union_2 = try toValue(arena.allocator(), SimpleUnion{ .b = true }, .{});
    try std.testing.expect(simple_union_2 == .Bool);
    try std.testing.expectEqual(true, simple_union_2.Bool);

    const SimpleStruct = struct {
        abc: u8,
        def: SimpleEnum1,
        ghi: SimpleEnum2,
    };

    const simple_struct = try toValue(arena.allocator(), SimpleStruct{ .abc = 25, .def = .c, .ghi = .d }, .{});
    try std.testing.expect(simple_struct == .Object);
    try std.testing.expectEqual(@as(i64, 25), simple_struct.Object.get("abc").?.Integer);
    try std.testing.expectEqual(@as(i64, 420), simple_struct.Object.get("def").?.Integer);
    try std.testing.expectEqualStrings("d", simple_struct.Object.get("ghi").?.String);
}
