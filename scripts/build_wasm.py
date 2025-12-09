#!/usr/bin/env python3
"""
Build script for Midori WebAssembly

This script automates the process of building Midori for WebAssembly
using Emscripten.
"""

import multiprocessing
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import List, Optional


# Constants
PROJECT_ROOT = Path(__file__).parent.parent
BUILD_DIR = PROJECT_ROOT / 'build-wasm'
OUTPUT_DIR = BUILD_DIR / 'out'


def check_emscripten() -> bool:
	"""
	Check if Emscripten is available in PATH

	Returns:
		bool: True if emcc is found, False otherwise
	"""
	if shutil.which('emcc') is None:
		print("Error: Emscripten is not activated.", file=sys.stderr)
		print("\nPlease activate Emscripten first:", file=sys.stderr)
		if sys.platform == 'win32':
			print("  cd path\\to\\emsdk", file=sys.stderr)
			print("  emsdk_env.bat", file=sys.stderr)
		else:
			print("  source path/to/emsdk/emsdk_env.sh", file=sys.stderr)
		return False
	return True


def run_command(cmd: List[str], cwd: Optional[Path] = None) -> bool:
	"""
	Run a command and print output in real-time

	Args:
		cmd: Command and arguments as a list
		cwd: Working directory for the command

	Returns:
		bool: True if command succeeded, False otherwise
	"""
	try:
		process = subprocess.Popen(
			cmd,
			cwd=cwd,
			stdout=subprocess.PIPE,
			stderr=subprocess.STDOUT,
			universal_newlines=True,
			shell=True if sys.platform == 'win32' else False
		)

		for line in process.stdout:
			print(line, end='')

		process.wait()

		if process.returncode != 0:
			print(f"\nCommand failed with exit code {process.returncode}", file=sys.stderr)
			return False
		return True
	except Exception as e:
		print(f"Error running command: {e}", file=sys.stderr)
		return False


def configure_cmake() -> bool:
	"""
	Configure CMake with Emscripten

	Returns:
		bool: True if configuration succeeded, False otherwise
	"""
	print("\nConfiguring CMake with Emscripten...")
	cmake_cmd = ['emcmake', 'cmake', '..', '-DCMAKE_BUILD_TYPE=Release']
	return run_command(cmake_cmd, cwd=BUILD_DIR)


def build_project() -> bool:
	"""
	Build the project with emmake

	Returns:
		bool: True if build succeeded, False otherwise
	"""
	print("\nBuilding...")

	if sys.platform == 'win32':
		build_cmd = ['emmake', 'cmake', '--build', '.', '--config', 'Release']
	else:
		# Use parallel builds on Unix-like systems
		jobs = multiprocessing.cpu_count()
		build_cmd = ['emmake', 'make', f'-j{jobs}']

	return run_command(build_cmd, cwd=BUILD_DIR)


def print_success_message() -> None:
	"""Print success message with output file locations"""
	print("\n" + "=" * 60)
	print("Build successful!")
	print("=" * 60)
	print("\nOutput files:")
	print(f"  - {OUTPUT_DIR / 'midori.js'}")
	print(f"  - {OUTPUT_DIR / 'midori.wasm'}")
	print("\nYou can now use these files in your web playground.")


def main() -> int:
	"""
	Main entry point for the build script

	Returns:
		int: Exit code (0 for success, 1 for failure)
	"""
	print("Building Midori for WebAssembly...")
	print(f"Project root: {PROJECT_ROOT}")

	# Check Emscripten
	if not check_emscripten():
		return 1

	# Create build directory
	print(f"\nCreating build directory: {BUILD_DIR}")
	BUILD_DIR.mkdir(exist_ok=True)

	# Configure with Emscripten
	if not configure_cmake():
		print("\nCMake configuration failed", file=sys.stderr)
		return 1

	# Build
	if not build_project():
		print("\nBuild failed", file=sys.stderr)
		return 1

	# Success message
	print_success_message()
	return 0


if __name__ == '__main__':
	sys.exit(main())
