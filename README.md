# tres

`std.json.parse` but on `ValueTree`s. Stringify that supports Undefinedables, ArrayLists, and HashMaps.

## Features

- `parse` (std.json.Value -> T)
- `stringify` (T -> []const u8)
- `toValue` (T -> std.json.Value)

All the above modes support `std.json` standard features as well as:
- Enhanced optionals
- String enums
- Map and ArrayList support (unmanaged too!)

## Credit

Credit where credit is due :)

Originally written by [Nameless](https://github.com/truemedian). Modified to a compilable state and tested by [Auguste Rame](https://github.com/SuperAuguste).

## License

MIT
