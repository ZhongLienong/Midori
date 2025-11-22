# Midori Testing Guide

This document explains how to run tests, add new tests, and work with the Midori test suite.

## Quick Start

```bash
# 1. Build the Development configuration
cmake --build out/Development --config Development --target Midori
cmake --build out/Development --config Development --target MidoriStdLib

# 2. Run all tests
python scripts/run_tests.py

# 3. Run tests from a specific category
python scripts/run_tests.py --category closure

# 4. Run tests matching a pattern
python scripts/run_tests.py --pattern loop

# 5. Show detailed output for failures
python scripts/run_tests.py --verbose
```

## Test Organization

Tests are organized in `test/` directory by category:

```
test/
├── closure/              # Closure-related tests
├── expression/
│   ├── loop/
│   │   ├── success/      # Tests that should compile successfully
│   │   └── failure/      # Tests that should fail compilation
├── generics/             # Generic type tests
├── leetcode/             # Algorithm problems
├── module/               # Module system tests
├── performance/          # Performance benchmarks
├── pipe/                 # Pipe operator tests
├── sicp/                 # SICP examples
├── struct/               # Struct tests
└── union/                # Union type tests
```

### Test Types

**Success Tests** (default):
- Should compile and run without errors
- Exit code must be 0
- Can have optional `.expected` file for output verification

**Failure Tests** (in `failure/` directories):
- Should fail to compile
- Exit code must be non-zero
- Used to test error detection

## Running Tests

### Run All Tests
```bash
python scripts/run_tests.py
```

### Run Specific Test
```bash
# Run a single test by name (auto-shows detailed output)
python scripts/run_tests.py --test simple
python scripts/run_tests.py --test closure/simple.mdr
python scripts/run_tests.py --test test/closure/simple.mdr
```

### Run Specific Category
```bash
python scripts/run_tests.py --category closure
python scripts/run_tests.py --category expression/loop
```

### Run Tests Matching Pattern
```bash
python scripts/run_tests.py --pattern recursive
python scripts/run_tests.py --pattern "nested"
```

### Use Different Build
```bash
# Use Debug build (shows AST, bytecode, full diagnostics)
python scripts/run_tests.py --build Debug

# Use Release build (minimal output)
python scripts/run_tests.py --build Release
```

### Verbose Output
```bash
# Show output for failed tests
python scripts/run_tests.py --verbose

# Note: When running 3 or fewer tests (e.g., using --test),
# detailed output is automatically enabled
```

## Adding New Tests

### Using the Helper Script

```bash
# Create a new success test
python scripts/new_test.py closure/my_new_test

# Create a failure test (should not compile)
python scripts/new_test.py expression/loop/failure/invalid_break --should-fail

# With description
python scripts/new_test.py module/import_test --description "Test circular imports"
```

This will:
1. Create the test file with a template
2. Show you next steps
3. Suggest how to run the test

### Manual Test Creation

1. Create a `.mdr` file in the appropriate category directory
2. Write your test code
3. (Optional) Create a `.expected` file with expected output

**Example test** (`test/closure/my_test.mdr`):
```midori
// Test: Closure captures variables correctly
defun makeCounter() : Unit -> Int =>
{
    def count = 0;
    || => { count = count + 1; count }
};

def counter = makeCounter();
counter(); // Should return 1
counter(); // Should return 2
```

**Expected output** (`test/closure/my_test.expected`):
```
1
2
```

### Failure Test Example

For tests that should fail compilation (`test/expression/loop/failure/break_outside_loop.mdr`):
```midori
// Test: break outside loop should cause error
defun main() : Unit =>
{
    break 42; // Error: break not inside loop
};
```

## Test Output Format

```
[OK] [SUCCESS] closure\simple.mdr (15ms)
[FAIL] [SHOULD-FAIL] expression\loop\failure\invalid_break.mdr (8ms)
```

- `[OK]` = Test passed
- `[FAIL]` = Test failed
- `[SUCCESS]` = Test should succeed
- `[SHOULD-FAIL]` = Test should fail to compile

## Understanding Test Results

### All Tests Passing
```
[SUCCESS] All tests passed!

Total:     45/45 passed
Success:   42/42 passed
Failure:   3/3 passed (should fail)
Duration:  1250ms
```

### Some Tests Failing
```
[FAILED] Some tests failed

Total:     42/45 passed
Success:   40/42 passed
Failure:   2/3 passed (should fail)
Duration:  1180ms

Failed tests:
  [X] closure\nested_complex.mdr
  [X] expression\loop\success\infinite_loop.mdr
  [X] expression\loop\failure\break_outside_loop.mdr
```

## Common Issues

### Tests can't find MidoriPrelude

**Problem**: Tests fail with "Could not open import file"

**Solution**: Tests need to import from the root `MidoriPrelude/` directory. Use:
```midori
import { "../MidoriPrelude/IO.mdr" }
```

### Missing MidoriStdLib.dll

**Problem**: "Failed to load the standard library"

**Solution**: Build the standard library for your configuration:
```bash
cmake --build out/Development --config Development --target MidoriStdLib
```

### Tests run but produce no output

**Problem**: Test compiles but doesn't run

**Solution**: Make sure you call the main function or expression:
```midori
defun main() : Unit => { /* ... */ };
main();  // Don't forget to call it!
```

## Continuous Integration

For CI/CD, use:
```bash
# Build everything
cmake --build out/Development --config Development

# Run tests with exit code
python scripts/run_tests.py
# Exit code 0 = all passed
# Exit code 1 = some failed
```

## Advanced Usage

### Creating Output Verification Tests

1. Write your test
2. Run it to generate output:
   ```bash
   out/Development/Development/Midori.exe test/my_test.mdr > test/my_test.expected
   ```
3. Edit `.expected` file to match what you want
4. Run test again - it will now verify output matches

### Testing Specific Features

```bash
# Test only failure cases
find test -path "*/failure/*.mdr" -exec basename {} \; | \
  xargs -I{} python scripts/run_tests.py --pattern {}

# Test only success cases
python scripts/run_tests.py --category closure
python scripts/run_tests.py --category expression/loop/success
```

### Performance Testing

```bash
# Run performance tests
python scripts/run_tests.py --category performance

# Use Release build for accurate timing
python scripts/run_tests.py --category performance --build Release
```

## Test Best Practices

1. **One concept per test**: Each test should test one specific feature
2. **Clear names**: Use descriptive filenames (e.g., `closure_captures_by_reference.mdr`)
3. **Add comments**: Explain what the test is testing
4. **Keep it simple**: Tests should be easy to understand
5. **Use expected output**: For tests with output, create `.expected` files
6. **Organize properly**: Put tests in the right category and success/failure subdirectory

## Scripts Reference

### `scripts/run_tests.py`
Main test runner with filtering and reporting

**Arguments**:
- `--build`: Build configuration (Debug/Development/Release)
- `--category`: Run specific category
- `--pattern`: Filter by filename pattern
- `--verbose`: Show detailed output

### `scripts/new_test.py`
Helper to create new test files

**Arguments**:
- `path`: Test path (e.g., `closure/my_test`)
- `--should-fail`: Mark as failure test
- `--description`: Test description

## Example Workflows

### Adding a new closure feature

```bash
# 1. Create test
python scripts/new_test.py closure/closure_in_match_expression

# 2. Edit the test
code test/closure/closure_in_match_expression.mdr

# 3. Run just this test (auto-shows detailed output)
python scripts/run_tests.py --test closure_in_match_expression

# 4. If it passes, run all closure tests
python scripts/run_tests.py --category closure
```

### Debugging a failing test

```bash
# 1. Run specific test (auto-shows detailed output)
python scripts/run_tests.py --test my_failing_test

# 2. Run with Debug build for AST/bytecode dumps
python scripts/run_tests.py --test my_failing_test --build Debug

# 3. Or run manually for interactive debugging
out/Debug/Debug/Midori.exe test/category/my_failing_test.mdr
```

### Before committing

```bash
# Run full test suite
python scripts/run_tests.py

# If all pass, commit
git add test/
git commit -m "Add tests for new feature"
```
