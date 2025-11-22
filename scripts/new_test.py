#!/usr/bin/env python3
"""
Helper script to create new Midori tests.

Usage:
    python scripts/new_test.py closure/new_test
    python scripts/new_test.py expression/loop/success/my_test
    python scripts/new_test.py module/test_import --should-fail
"""

import argparse
import sys
from pathlib import Path

TEMPLATE_SUCCESS = '''// Test: {description}
// Expected: Should compile and run successfully

defun main() : Unit =>
{{
    // TODO: Add test code here
}};

main();
'''

TEMPLATE_FAILURE = '''// Test: {description}
// Expected: Should fail to compile

// TODO: Add code that should cause compilation error
'''

def create_test(test_path: str, should_fail: bool, description: str):
    """Create a new test file."""
    root_dir = Path(__file__).parent.parent
    test_file = root_dir / "test" / f"{test_path}.mdr"

    # Create parent directories
    test_file.parent.mkdir(parents=True, exist_ok=True)

    # Check if file already exists
    if test_file.exists():
        print(f"Error: Test file already exists: {test_file}")
        sys.exit(1)

    # Select template
    if should_fail:
        content = TEMPLATE_FAILURE.format(description=description)
    else:
        content = TEMPLATE_SUCCESS.format(description=description)

    # Write test file
    test_file.write_text(content, encoding='utf-8')

    print(f"[OK] Created test: {test_file.relative_to(root_dir)}")
    print(f"\nNext steps:")
    print(f"  1. Edit the test: {test_file}")
    print(f"  2. Run the test: python scripts/run_tests.py --pattern {test_file.stem}")

    # If it's a success test, suggest creating expected output
    if not should_fail:
        print(f"  3. (Optional) Create expected output file:")
        print(f"     out/Development/Development/Midori.exe {test_file} > {test_file.with_suffix('.expected')}")

def main():
    parser = argparse.ArgumentParser(description='Create a new Midori test')
    parser.add_argument('path', help='Test path (e.g., closure/my_test or expression/loop/success/test)')
    parser.add_argument('--should-fail', action='store_true',
                        help='Test should fail to compile')
    parser.add_argument('--description', default='',
                        help='Test description')

    args = parser.parse_args()

    # Remove .mdr extension if provided
    test_path = args.path.replace('.mdr', '')

    # Generate description if not provided
    if not args.description:
        test_name = Path(test_path).name.replace('_', ' ').title()
        args.description = f"{test_name} test"

    create_test(test_path, args.should_fail, args.description)

if __name__ == '__main__':
    main()
