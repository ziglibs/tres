# tres

`std.json.parse` but on `ValueTree`s. Stringify that supports Undefinedables, ArrayLists, and HashMaps.

## Features

- `parse` (std.json.Value -> T)
- `stringify` (T -> []const u8)
- `toValue` (T -> std.json.Value)

All the above modes support `std.json` standard features as well as:
- Enhanced optionals (`tres_null_meaning`, see test "parse and stringify null meaning")
- String enums (`tres_string_enum`, see test "json.stringify enums")
- Field name remapping (`tres_remap`, see test "remapping")
- Map and ArrayList support (unmanaged too!)

## License

MIT
