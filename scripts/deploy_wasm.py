#!/usr/bin/env python3
"""
Deploy script for Midori WebAssembly

This script copies the built WASM files to the website's public folder.
"""

import shutil
import sys
from pathlib import Path
from typing import List


# Constants
PROJECT_ROOT = Path(__file__).parent.parent
BUILD_DIR = PROJECT_ROOT / 'build-wasm'
PRELUDE_DIR = PROJECT_ROOT / 'MidoriPrelude'
WEBSITE_PUBLIC = Path('C:/Users/jk381/OneDrive/Documents/GitHub/ZhongLienong.github.io/public')
FILES_TO_COPY = ['midori.js', 'midori.wasm']


def format_file_size(size_bytes: int) -> str:
	"""
	Format file size in human-readable format

	Args:
		size_bytes: File size in bytes

	Returns:
		str: Formatted size string (e.g., "1.23 MB" or "456.78 KB")
	"""
	size_kb = size_bytes / 1024
	size_mb = size_kb / 1024

	if size_mb >= 1:
		return f"{size_mb:.2f} MB"
	else:
		return f"{size_kb:.2f} KB"


def check_build_directory() -> bool:
	"""
	Check if build directory exists

	Returns:
		bool: True if directory exists, False otherwise
	"""
	if not BUILD_DIR.exists():
		print(f"Error: Build directory not found: {BUILD_DIR}", file=sys.stderr)
		print("\nPlease build the WASM module first:", file=sys.stderr)
		print("  python scripts/build_wasm.py", file=sys.stderr)
		return False
	return True


def check_build_artifacts() -> bool:
	"""
	Check if all required build artifacts exist

	Returns:
		bool: True if all files exist, False otherwise
	"""
	missing_files: List[str] = []

	for filename in FILES_TO_COPY:
		if not (BUILD_DIR / filename).exists():
			missing_files.append(filename)

	if missing_files:
		print("Error: Missing build artifacts:", file=sys.stderr)
		for filename in missing_files:
			print(f"  - {filename}", file=sys.stderr)
		print("\nPlease rebuild the WASM module:", file=sys.stderr)
		print("  python scripts/build_wasm.py", file=sys.stderr)
		return False

	return True


def check_website_directory() -> bool:
	"""
	Check if website public directory exists

	Returns:
		bool: True if directory exists, False otherwise
	"""
	if not WEBSITE_PUBLIC.exists():
		print(f"Error: Website public directory not found: {WEBSITE_PUBLIC}", file=sys.stderr)
		print("\nPlease update the website path in this script.", file=sys.stderr)
		return False
	return True


def copy_wasm_files() -> bool:
	"""
	Copy WASM files to website public directory

	Returns:
		bool: True if all files copied successfully, False otherwise
	"""
	print(f"Deploying WASM files to: {WEBSITE_PUBLIC}")
	print()

	for filename in FILES_TO_COPY:
		src = BUILD_DIR / filename
		dst = WEBSITE_PUBLIC / filename

		try:
			shutil.copy2(src, dst)
			file_size = src.stat().st_size
			size_str = format_file_size(file_size)
			print(f"Copied {filename} ({size_str})")
		except Exception as e:
			print(f"Failed to copy {filename}: {e}", file=sys.stderr)
			return False

	return True


def copy_prelude_directory() -> bool:
	"""
	Copy MidoriPrelude directory to website public directory

	Returns:
		bool: True if directory copied successfully, False otherwise
	"""
	print()
	print("Copying MidoriPrelude files...")
	prelude_dest = WEBSITE_PUBLIC / 'MidoriPrelude'

	try:
		if prelude_dest.exists():
			shutil.rmtree(prelude_dest)
		shutil.copytree(PRELUDE_DIR, prelude_dest)

		# Count and list copied files
		mdr_files = list(prelude_dest.glob('*.mdr'))
		print(f"Copied MidoriPrelude directory ({len(mdr_files)} .mdr files)")
		for mdr_file in mdr_files:
			print(f"  - {mdr_file.name}")

		return True
	except Exception as e:
		print(f"Failed to copy MidoriPrelude: {e}", file=sys.stderr)
		return False


def print_success_message() -> None:
	"""Print success message with instructions"""
	print()
	print("=" * 60)
	print("Deployment successful!")
	print("=" * 60)
	print()
	print("The WASM files have been deployed to your website.")
	print("Start your development server to test:")
	print()
	print("  cd ZhongLienong.github.io")
	print("  npm run dev")
	print()
	print("Then navigate to: http://localhost:3000/projects/midori")


def main() -> int:
	"""
	Main entry point for the deployment script

	Returns:
		int: Exit code (0 for success, 1 for failure)
	"""
	# Validation checks
	if not check_build_directory():
		return 1

	if not check_build_artifacts():
		return 1

	if not check_website_directory():
		return 1

	# Copy files
	if not copy_wasm_files():
		return 1

	# Copy MidoriPrelude
	if not copy_prelude_directory():
		return 1

	# Success message
	print_success_message()
	return 0


if __name__ == '__main__':
	sys.exit(main())
