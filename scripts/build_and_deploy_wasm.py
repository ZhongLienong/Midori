#!/usr/bin/env python3
"""
Build and deploy Midori WebAssembly

This script builds the WASM module using Emscripten and deploys it
to the website's public folder along with the MidoriPrelude files.
"""

import multiprocessing
import os
import shutil
import stat
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


PROJECT_ROOT = Path(__file__).parent.parent
BUILD_DIR = PROJECT_ROOT / 'build-wasm'
OUTPUT_DIR = BUILD_DIR / 'out'
PRELUDE_DIR = PROJECT_ROOT / 'MidoriPrelude'
WEBSITE_PUBLIC = Path('C:/Users/jk381/OneDrive/Documents/GitHub/ZhongLienong.github.io/public')
WASM_FILES = ['midori.js', 'midori.wasm']


@dataclass(frozen=True)
class EmscriptenTools:
	env: dict[str, str]
	emcc: str
	emcmake: str
	emmake: str
	root: Path | None = None


def format_size(size_bytes: int) -> str:
	size_kb = size_bytes / 1024
	size_mb = size_kb / 1024
	return f"{size_mb:.2f} MB" if size_mb >= 1 else f"{size_kb:.2f} KB"


def run_command(cmd: list[str], cwd: Path | None = None, env: dict[str, str] | None = None) -> bool:
	try:
		process = subprocess.Popen(
			cmd,
			cwd=cwd,
			env=env,
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


def find_first_directory(directory: Path) -> Path | None:
	if not directory.exists():
		return None

	candidates = [path for path in directory.iterdir() if path.is_dir()]
	if not candidates:
		return None

	candidates.sort()
	return candidates[-1]


def build_emscripten_env(emsdk_root: Path) -> EmscriptenTools | None:
	emscripten_root = emsdk_root / 'upstream' / 'emscripten'
	emcc_path = emscripten_root / ('emcc.bat' if sys.platform == 'win32' else 'emcc')
	emcmake_path = emscripten_root / ('emcmake.bat' if sys.platform == 'win32' else 'emcmake')
	emmake_path = emscripten_root / ('emmake.bat' if sys.platform == 'win32' else 'emmake')

	if not emcc_path.exists() or not emcmake_path.exists() or not emmake_path.exists():
		return None

	env = os.environ.copy()
	path_entries = [str(emsdk_root), str(emscripten_root)]
	env['EMSDK'] = str(emsdk_root).replace('\\', '/')
	env['EM_CONFIG'] = str(emsdk_root / '.emscripten')
	env['EMSDK_QUIET'] = '1'

	node_dir = find_first_directory(emsdk_root / 'node')
	if node_dir is not None:
		node_exe = node_dir / 'bin' / ('node.exe' if sys.platform == 'win32' else 'node')
		if node_exe.exists():
			env['EMSDK_NODE'] = str(node_exe)

	python_dir = find_first_directory(emsdk_root / 'python')
	if python_dir is not None:
		python_exe = python_dir / ('python.exe' if sys.platform == 'win32' else 'python3')
		if python_exe.exists():
			env['EMSDK_PYTHON'] = str(python_exe)

	existing_path = env.get('PATH', '')
	env['PATH'] = os.pathsep.join(path_entries + ([existing_path] if existing_path else []))

	return EmscriptenTools(
		env=env,
		emcc=str(emcc_path),
		emcmake=str(emcmake_path),
		emmake=str(emmake_path),
		root=emsdk_root
	)


def discover_emscripten() -> EmscriptenTools | None:
	emcc_path = shutil.which('emcc')
	emcmake_path = shutil.which('emcmake')
	emmake_path = shutil.which('emmake')
	if emcc_path is not None and emcmake_path is not None and emmake_path is not None:
		return EmscriptenTools(
			env=os.environ.copy(),
			emcc=emcc_path,
			emcmake=emcmake_path,
			emmake=emmake_path
		)

	search_roots: list[Path] = []
	emsdk_env = os.environ.get('EMSDK')
	if emsdk_env:
		search_roots.append(Path(emsdk_env))

	if sys.platform == 'win32':
		search_roots.extend([
			Path('C:/Program Files/emsdk-main'),
			Path('C:/emsdk-main'),
			Path('C:/emsdk'),
			Path.home() / 'emsdk',
		])
	else:
		search_roots.extend([
			Path.home() / 'emsdk',
			Path('/opt/emsdk'),
		])

	seen_paths: set[Path] = set()
	for root in search_roots:
		resolved_root = root.expanduser()
		if resolved_root in seen_paths:
			continue
		seen_paths.add(resolved_root)

		tools = build_emscripten_env(resolved_root)
		if tools is not None:
			return tools

	return None


def check_emscripten() -> EmscriptenTools | None:
	tools = discover_emscripten()
	if tools is not None:
		if tools.root is not None:
			print(f"Using Emscripten from {tools.root}")
		return tools

	print("Error: Emscripten not found.", file=sys.stderr)
	print("\nActivate Emscripten first:", file=sys.stderr)
	if sys.platform == 'win32':
		print("  cd C:\\Program Files\\emsdk-main && emsdk_env.bat", file=sys.stderr)
	else:
		print("  source path/to/emsdk/emsdk_env.sh", file=sys.stderr)
	return None


def remove_readonly(func, path, _):
	os.chmod(path, stat.S_IWRITE)
	func(path)


def clean_build() -> None:
	if BUILD_DIR.exists():
		print(f"Cleaning {BUILD_DIR}...")
		shutil.rmtree(BUILD_DIR, onexc=remove_readonly)
	BUILD_DIR.mkdir(exist_ok=True)


def configure(tools: EmscriptenTools) -> bool:
	print("\nConfiguring with Emscripten...")
	return run_command(
		[tools.emcmake, 'cmake', '..', '-DCMAKE_BUILD_TYPE=Release', '-DMIDORI_WASM64=ON'],
		cwd=BUILD_DIR,
		env=tools.env
	)


def build(tools: EmscriptenTools) -> bool:
	print("\nBuilding...")
	if sys.platform == 'win32':
		cmd = [tools.emmake, 'cmake', '--build', '.', '--config', 'Release']
	else:
		cmd = [tools.emmake, 'make', f'-j{multiprocessing.cpu_count()}']
	return run_command(cmd, cwd=BUILD_DIR, env=tools.env)


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

	tools = check_emscripten()
	if tools is None:
		return 1

	clean_build()

	if not configure(tools):
		print("\nConfiguration failed.", file=sys.stderr)
		return 1

	if not build(tools):
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
