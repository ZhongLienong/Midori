# Midori Scripts

This directory contains utility scripts for Midori development.

## Available Scripts

### `run_tests.py` - Main Test Runner

Run the Midori test suite with filtering and detailed reporting.

**Quick Examples**:
```bash
# Run all tests
python scripts/run_tests.py

# Run specific test (auto-shows detailed output)
python scripts/run_tests.py --test closure/simple
python scripts/run_tests.py --test simple.mdr

# Run specific category
python scripts/run_tests.py --category closure

# Run tests matching pattern
python scripts/run_tests.py --pattern recursive

# Use Debug build
python scripts/run_tests.py --build Debug --verbose

# Show help
python scripts/run_tests.py --help
```

**Features**:
- ✅ Colored output with status indicators
- ✅ Automatic detection of success/failure tests
- ✅ Expected output verification (.expected files)
- ✅ Auto-detailed output for single/few tests (≤3 tests)
- ✅ Run specific test by name or path
- ✅ Detailed timing information
- ✅ Summary statistics
- ✅ Proper exit codes for CI/CD

**Output Example**:
```
[OK] [SUCCESS] closure\simple.mdr (15ms)
[OK] [SHOULD-FAIL] expression\loop\failure\invalid_break.mdr (8ms)
[FAIL] [SUCCESS] module\import_test.mdr (20ms)
```

---

### `new_test.py` - Test Creation Helper

Create new test files with templates.

**Quick Examples**:
```bash
# Create success test
python scripts/new_test.py closure/my_new_test

# Create failure test
python scripts/new_test.py expression/failure/invalid_syntax --should-fail

# With description
python scripts/new_test.py module/circular_import \
  --description "Test detection of circular imports" \
  --should-fail
```

**Features**:
- ✅ Automatic directory creation
- ✅ Template generation
- ✅ Helpful next-step instructions

---

## Common Workflows

### Before Committing Code
```bash
# Run all tests
python scripts/run_tests.py

# If all pass, commit
git add .
git commit -m "Your message"
```

### Debugging a Feature
```bash
# Create test first
python scripts/new_test.py feature/my_test

# Edit test
code test/feature/my_test.mdr

# Run with Debug build for full diagnostics
python scripts/run_tests.py --pattern my_test --build Debug --verbose
```

### Running CI Tests
```bash
# Build
cmake --build out/Development --config Development

# Run tests (exits with code 1 if any fail)
python scripts/run_tests.py
```

---

## Test Directory Structure

```
test/
├── closure/              # Closure tests
├── expression/
│   ├── loop/
│   │   ├── success/      # Should pass
│   │   └── failure/      # Should fail compilation
├── generics/             # Generic types
├── module/               # Module system
├── performance/          # Benchmarks
└── ...
```

---

## Tips

1. **Use Development build for testing** - Provides good balance of speed and diagnostics
2. **Create .expected files** - For tests with output, create `.expected` file for verification
3. **Organize tests properly** - Put in correct category and success/failure folder
4. **Run tests frequently** - Catch regressions early
5. **Use --verbose** - When debugging test failures

---

## See Also

- [TESTING.md](../TESTING.md) - Comprehensive testing guide
- [CMakeLists.txt](../CMakeLists.txt) - Build system configuration with Debug/Development/Release settings
