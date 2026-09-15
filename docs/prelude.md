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
- `Concatenable.mdr`, `Convertable.mdr`, `Countable.mdr`, `Equatable.mdr`, `Hashable.mdr`, `Indexable.mdr`, `Iterable.mdr`, `Orderable.mdr`, and `Transferable.mdr` expose the helper and typeclass surface used by operators, collections, and concurrency.
- `Prelude/Panic.mdr` contains the simple panic helper used by many tests and examples.
- `Concurrency.mdr` declares `WorkerError` (`Cancelled | Failed(Text)`), the error half of the `Result<T, WorkerError>` that `join` evaluates to, and `JoinedOrPanic` for code that treats a worker failure as fatal. The compiler requires the `WorkerError` declaration to have exactly that shape.

## Helper and Typeclass Modules

The prelude is not only collections and IO wrappers. It also ships the public helper/typeclass modules that the compiler and standard data structures lean on:

- `Concatenable` backs `++` and currently ships concrete instances for `Text` and `Array<T>`.
- `Convertable` exposes the generic conversion surface used by `as` in constrained code and ships the current primitive conversion instances.
- `Countable` exposes the generic counting surface used by `#`. The prelude currently ships a `Text` instance; arrays and several standard collections also have direct lowering paths in the compiler/runtime.
- `Equatable` and `Hashable` provide the comparison and hashing surface used by derived code and collections.
- `Indexable` backs `x[i]`. It takes two type parameters, `Indexable<C, I>`, so the index type is not fixed to `Int`, and exposes an `Element` associated type. The prelude ships an `Array<T>` instance; arrays also keep a direct lowering path in the compiler, which the instance body itself relies on. `Text` has no instance yet - its element type follows from the planned newtype over `Array<Byte>`.
- `Iterable` provides the `Item` associated type and `Next` method used by `for` loops and iterable-based comprehensions.
- `Orderable` defines the ordering interface used by comparison operators for user-defined types. The module exports the class surface; concrete instances are typically user-defined.
- `Transferable` is the marker typeclass for values that can cross worker boundaries in the concurrency system. Built-in instances cover all primitive types, `Array<T>`, and `Channel<T>`. User-defined structs and unions can `deriving (Transferable)`. Transferability is enforced at compile time by `spawn`, `join`, `channel`, `->`, and `<-`.
- `Prelude/Panic` provides `Panic::Panic`, which is used heavily by the regression tests and small examples.

## Result Naming

`Prelude/Result.mdr` uses `Result::Ok` and `Result::Err` as the only public constructor spellings.

Migration note:

- Replace `Result::OK` with `Result::Ok`.
- Replace `Result::Error` with `Result::Err`.
- The legacy spellings are no longer part of the public prelude API.

## Typed IO and System APIs

`IO` models file-system failures as `Result<_, IOError>`. `System` uses `Option<Text>` for environment lookup, `Result<_, SystemError>` for fallible process and directory operations, and a `Platform` union for platform detection.

```midori-test name=prelude/typed_io_system path=.doc_example_prelude_typed_io_system.mdr module=PreludeTypedIOSystem
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

```midori-test name=prelude/datetime path=.doc_example_prelude_datetime.mdr module=PreludeDateTime
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

`TextUtil` groups the common string-style operations already available in the runtime, while `ArrayUtil` collects the ordinary array helpers. Arrays are immutable: `WithAppended`, `WithInserted`, `WithReplaced`, and `WithRemoved` return a new array with the requested change rather than mutating the original, and `Slice` and `Reverse` likewise return new arrays. Build an array up front with a comprehension or `List`, or grow one incrementally with `WithAppended` when the number of elements is small - calling it in a loop is quadratic.

```midori-test name=prelude/text_array_helpers path=.doc_example_prelude_text_array_helpers.mdr module=PreludeTextArrayHelpers
import
{
    "./MidoriPrelude/TextUtil.mdr",
    "./MidoriPrelude/ArrayUtil.mdr"
}

def words = TextUtil::Split(TextUtil::Trim("  alpha beta  "), " ");
def headline = TextUtil::Replace("midori docs", "docs", "prelude");

def numbers = [1, 2, 3];
def grown = ArrayUtil::WithAppended(numbers, 4);
def replaced = ArrayUtil::WithReplaced(grown, 0, 9);

def reversed = ArrayUtil::Reverse(replaced);
def window = ArrayUtil::Slice(replaced, 1, 3);
```

Public text helpers:

- `Length`, `Contains`, `Substring`
- `Split`, `Replace`, `Trim`, `Reverse`

Public array helpers:

- `Length`, `Contains`
- `Slice`, `Reverse`, `Copy`, `Concat`
- `WithAppended`, `WithReplaced`, `WithInserted`, `WithRemoved`

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
