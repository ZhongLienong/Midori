#!/usr/bin/env python3
"""
Build and deploy Midori WebAssembly

This script builds the WASM module using Emscripten and deploys it
to the website's public folder along with the MidoriPrelude files.
"""

import multiprocessing
import shutil
import subprocess
import sys
from pathlib import Path


PROJECT_ROOT = Path(__file__).parent.parent
BUILD_DIR = PROJECT_ROOT / 'build-wasm'
OUTPUT_DIR = BUILD_DIR / 'out'
PRELUDE_DIR = PROJECT_ROOT / 'MidoriPrelude'
WEBSITE_PUBLIC = Path('C:/Users/jk381/OneDrive/Documents/GitHub/ZhongLienong.github.io/public')
WASM_FILES = ['midori.js', 'midori.wasm']


def format_size(size_bytes: int) -> str:
	size_kb = size_bytes / 1024
	size_mb = size_kb / 1024
	return f"{size_mb:.2f} MB" if size_mb >= 1 else f"{size_kb:.2f} KB"


def run_command(cmd: list[str], cwd: Path | None = None) -> bool:
	try:
		process = subprocess.Popen(
			cmd,
			cwd=cwd,
			stdout=subprocess.PIPE,
			stderr=subprocess.STDOUT,
			universal_newlines=True,
			shell=(sys.platform == 'win32')
		)
		for line in process.stdout:
			print(line, end='')
		process.wait()
		return process.returncode == 0
	except Exception as e:
		print(f"Error: {e}", file=sys.stderr)
		return False


def check_emscripten() -> bool:
	if shutil.which('emcc') is None:
		print("Error: Emscripten not found.", file=sys.stderr)
		print("\nActivate Emscripten first:", file=sys.stderr)
		if sys.platform == 'win32':
			print("  cd path\\to\\emsdk && emsdk_env.bat", file=sys.stderr)
		else:
			print("  source path/to/emsdk/emsdk_env.sh", file=sys.stderr)
		return False
	return True


def clean_build() -> None:
	if BUILD_DIR.exists():
		print(f"Cleaning {BUILD_DIR}...")
		shutil.rmtree(BUILD_DIR)
	BUILD_DIR.mkdir(exist_ok=True)


def configure() -> bool:
	print("\nConfiguring with Emscripten...")
	return run_command(['emcmake', 'cmake', '..', '-DCMAKE_BUILD_TYPE=Release'], cwd=BUILD_DIR)


def build() -> bool:
	print("\nBuilding...")
	if sys.platform == 'win32':
		cmd = ['emmake', 'cmake', '--build', '.', '--config', 'Release']
	else:
		cmd = ['emmake', 'make', f'-j{multiprocessing.cpu_count()}']
	return run_command(cmd, cwd=BUILD_DIR)


def check_artifacts() -> bool:
	for f in WASM_FILES:
		if not (OUTPUT_DIR / f).exists():
			print(f"Error: Missing {f}", file=sys.stderr)
			return False
	return True


def deploy() -> bool:
	if not WEBSITE_PUBLIC.exists():
		print(f"Error: Website directory not found: {WEBSITE_PUBLIC}", file=sys.stderr)
		return False

	print(f"\nDeploying to {WEBSITE_PUBLIC}...")

	for filename in WASM_FILES:
		src = OUTPUT_DIR / filename
		dst = WEBSITE_PUBLIC / filename
		shutil.copy2(src, dst)
		print(f"  {filename} ({format_size(src.stat().st_size)})")

	prelude_dest = WEBSITE_PUBLIC / 'MidoriPrelude'
	if prelude_dest.exists():
		shutil.rmtree(prelude_dest)
	shutil.copytree(PRELUDE_DIR, prelude_dest)
	mdr_count = len(list(prelude_dest.glob('*.mdr')))
	print(f"  MidoriPrelude/ ({mdr_count} files)")

	return True


def main() -> int:
	print("Midori WebAssembly - Build and Deploy")
	print("=" * 50)

	if not check_emscripten():
		return 1

	clean_build()

	if not configure():
		print("\nConfiguration failed.", file=sys.stderr)
		return 1

	if not build():
		print("\nBuild failed.", file=sys.stderr)
		return 1

	if not check_artifacts():
		return 1

	if not deploy():
		print("\nDeployment failed.", file=sys.stderr)
		return 1

	print("\n" + "=" * 50)
	print("Done! Start dev server:")
	print("  cd ZhongLienong.github.io && npm run dev")
	return 0


if __name__ == '__main__':
	sys.exit(main())
