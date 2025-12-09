#!/usr/bin/env python3
"""
Build and deploy script for Midori WebAssembly

This script builds the WASM module and deploys it to the website in one step.
"""

import subprocess
import sys
from pathlib import Path


# Constants
SCRIPT_DIR = Path(__file__).parent


def run_script(script_name: str) -> int:
	"""
	Run another Python script and return its exit code

	Args:
		script_name: Name of the script file to run

	Returns:
		int: Exit code from the script
	"""
	script_path = SCRIPT_DIR / script_name

	print(f"Running {script_name}...")
	print("=" * 60)
	print()

	result = subprocess.run([sys.executable, str(script_path)])

	print()
	print("=" * 60)
	print()

	return result.returncode


def print_header() -> None:
	"""Print script header"""
	print("Midori WebAssembly - Build and Deploy")
	print("=" * 60)
	print()


def print_success_message() -> None:
	"""Print final success message"""
	print()
	print("=" * 60)
	print("All done!")
	print("=" * 60)
	print()
	print("Your Midori playground is ready to use.")


def main() -> int:
	"""
	Main entry point for the build and deploy script

	Returns:
		int: Exit code (0 for success, 1 for failure)
	"""
	print_header()

	# Step 1: Build
	print("Step 1: Building WASM module")
	print()
	build_result = run_script('build_wasm.py')

	if build_result != 0:
		print("Build failed. Aborting deployment.", file=sys.stderr)
		return 1

	# Step 2: Deploy
	print("Step 2: Deploying to website")
	print()
	deploy_result = run_script('deploy_wasm.py')

	if deploy_result != 0:
		print("Deployment failed.", file=sys.stderr)
		return 1

	# Success message
	print_success_message()
	return 0


if __name__ == '__main__':
	sys.exit(main())
