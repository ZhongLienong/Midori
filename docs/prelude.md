# Midori Prelude

`MidoriPrelude/` is the standard-library layer that ships with the repository. It wraps the runtime FFI surface in Midori-facing modules and collects the core algebraic data types, collections, and built-in helpers that ordinary programs use.

The examples below assume a program that lives next to `MidoriPrelude/` in the repo or in a project with the same layout. If you expose top-level prelude modules on `MIDORI_PATH`, imports such as `IO`, `System`, `DateTime`, `TextUtil`, and `ArrayUtil` can also use search-path imports.

Complete `.mdr` source files still need an explicit `module` declaration even when a focused snippet below omits it.

The documented examples in this file are mirrored by `test/prelude/success/documentation_examples.mdr`.

## Module Map

- `Prelude/Option.mdr`, `Prelude/Result.mdr`, and `Prelude/List.mdr` provide the core ADTs and helper functions used throughout the rest of the prelude.
- `Collections/Map.mdr` and `Collections/Set.mdr` provide hash-based collections. `MapInsert` is insert-only, `MapUpdate` updates existing keys only, and `SetInsert` is idempotent.
- `IO.mdr`, `System.mdr`, and `DateTime.mdr` are the effectful modules. Their public surface favors `Option` and `Result` wrappers rather than sentinel return values.
- `TextUtil.mdr`, `ArrayUtil.mdr`, and `Math.mdr` provide the common text, array, and numeric helpers that sit above the raw runtime builtins.

## Result Naming

`Prelude/Result.mdr` uses `Result::Ok` and `Result::Err` as the only public constructor spellings.

Migration note:

- Replace `Result::OK` with `Result::Ok`.
- Replace `Result::Error` with `Result::Err`.
- The legacy spellings are no longer part of the public prelude API.

## Typed IO and System APIs

`IO` models file-system failures as `Result<_, IOError>`. `System` uses `Option<Text>` for environment lookup, `Result<_, SystemError>` for fallible process and directory operations, and a `Platform` union for platform detection.

```midori
import
{
    "./MidoriPrelude/IO.mdr",
    "./MidoriPrelude/System.mdr",
    "./MidoriPrelude/Prelude/Option.mdr",
    "./MidoriPrelude/Prelude/Result.mdr"
}
use Option.{Option}
use Result.{Result}
use System.{Platform}

def config_text =
    match IO::TryReadFile("app.conf") with
        case Result::Ok(contents) => contents
        case Result::Err(_) => "release=debug"
    ;

def cache_dir =
    match System::TryGetEnv("MIDORI_CACHE_DIR") with
        case Option::Some(path) => path
        case Option::None() => ".midori-cache"
    ;

def platform_name =
    match System::CurrentPlatform() with
        case Platform::Windows() => "windows"
        case Platform::MacOS() => "macos"
        case Platform::Linux() => "linux"
        case Platform::WebAssembly() => "webassembly"
        case Platform::Unknown(name) => name
    ;
```

Public IO entry points:

- `TryReadFile`, `TryWriteFile`, `TryAppendToFile`
- `TryReadBinaryFile`, `TryWriteBinaryFile`
- `TryDeleteFile`, `TryRenameFile`, `TryGetFileSize`
- Console helpers such as `PrintLine`, `PrintErrorLine`, `ReadInput`, and `ReadLine`

Public system entry points:

- `TryGetEnv`, `SetEnv`
- `CurrentDirectory`, `TrySetCurrentDirectory`
- `Run`, `CurrentPlatform`, `CurrentProcessId`
- `Exit`, `Sleep`

The raw foreign declarations remain module-internal implementation details and are not the public API to build against.

## Date and Time

`DateTime` exposes explicit local and UTC structs instead of leaving callers to reconstruct ambient parts manually.

```midori
import { "./MidoriPrelude/DateTime.mdr" }

def local = DateTime::LocalNow();
def utc = DateTime::UtcNow();
def timestamp = DateTime::NowUnixMillis();
def formatted = DateTime::FormatLocal("%Y-%m-%d %H:%M:%S");
def offset = local.timezone_offset_minutes;
```

Public date/time entry points:

- `LocalNow`, `UtcNow`
- `NowUnixMillis`, `TimezoneOffsetMinutes`
- `FormatLocal`

## Text and Array Helpers

`TextUtil` groups the common string-style operations already available in the runtime, while `ArrayUtil` collects the ordinary mutable array helpers. `ArrayUtil::Append`, `Prepend`, and `Extend` mutate the target array. `Slice` and `Reverse` return new arrays.

```midori
import
{
    "./MidoriPrelude/TextUtil.mdr",
    "./MidoriPrelude/ArrayUtil.mdr"
}

def words = TextUtil::Split(TextUtil::Trim("  alpha beta  "), " ");
def headline = TextUtil::Replace("midori docs", "docs", "prelude");

def numbers = [2, 3];
ArrayUtil::Prepend(numbers, 1);
ArrayUtil::Append(numbers, 4);

def reversed = ArrayUtil::Reverse(numbers);
def window = ArrayUtil::Slice(numbers, 1, 3);
```

Public text helpers:

- `Length`, `Contains`, `Substring`
- `Split`, `Replace`, `Trim`, `Reverse`

Public array helpers:

- `Length`, `Contains`
- `Append`, `Prepend`, `Extend`
- `Slice`, `Reverse`

## Core ADTs and Collections

The core prelude modules stay intentionally small:

- `Prelude/Option.mdr`: `Option`, `OptionMap`, `OptionBind`, `OptionIsSome`, `OptionIsNone`, `OptionOrElse`, `OptionUnwrap`, `OptionUnwrapOrElse`, `OptionUnwrapOrPanic`
- `Prelude/Result.mdr`: `Result`, `ResultMap`, `ResultBind`, `ResultMapError`, `ResultIsOk`, `ResultIsError`, `ResultUnwrap`, `ResultUnwrapOrElse`, `ResultUnwrapOrPanic`
- `Prelude/List.mdr`: `List`, `ListMap`, `ListLength`, `ListAppend`, `ListFilter`, `ListReverse`, `ListFold`, `ListFromArray`, `ListToArray`
- `Collections/Map.mdr`: `MapNew`, `MapInsert`, `MapGet`, `MapGetOr`, `MapUpdate`, `MapRemove`, `MapContains`, `MapKeys`, `MapValues`, `MapEntries`
- `Collections/Set.mdr`: `SetNew`, `SetInsert`, `SetContains`, `SetRemove`, `SetUnion`, `SetIntersection`, `SetDifference`, `SetToArray`

## Math

`Math.mdr` exports the numeric constants and wrappers used by ordinary Midori code:

- constants such as `Pi`, `E`, and `Tau`
- transcendental and trigonometric helpers such as `SquareRoot`, `Pow`, `Sin`, and `Atan2`
- bounds helpers such as `Min`, `Max`, `Clamp`, `Sign`, and `Hypot`
- randomness helpers such as `Random`, `RandomInt`, and `RandomFloat`
- unit-conversion helpers such as `ToRadians` and `ToDegrees`
