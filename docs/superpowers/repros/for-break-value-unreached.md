# A `for` that never breaks yields an uninitialised slot

Found 2026-09-12 while deciding whether `break` should survive inside `for`.
It decided the question.

## Repro

```midori
def found   = for i in 0..1..10 { if i * i > 30 then break i else (); };  // 6, correct
def missing = for i in 0..1..3  { if i > 100    then break i else (); };  // 0, wrong
def crash   = for i in 0..1..3  { if i > 100    then break "x" else (); }; // panic
```

| break type | loop breaks | loop completes |
|---|---|---|
| `Int` | correct value | **`0`, exit 0 — silently wrong** |
| `Text` | correct value | **`panic[MemoryAccessViolation]`, exit 2** |

The completing path never writes the result slot, so the value read back is
whatever was there. For `Int` that reads as `0`; for `Text` a non-pointer is
dereferenced (fault address `0x47`).

## Why this is not simply a bug to fix

`for` is an expression and `break` carries a value, so `for`/`break` is
reachable as a *search*: iterate until found, yield what was found. That
construct has no answer for "not found". Making it sound means `for` yielding
`Option<T>`, which is a new built-in semantic and more machinery, against the
design goal of as few built-in things as possible.

**Resolution: `break` is deleted with `loop` and `continue`** rather than fixed.
`for` becomes genuinely effect-only, as spec §3 describes it, and a search is
written as a recursive helper or a filtered iterable, where the not-found case
has to be stated explicitly and cannot be read off an uninitialised slot.

Kept as the record of why, since "keep `break` for `for`" is otherwise the
reasonable-sounding call.
