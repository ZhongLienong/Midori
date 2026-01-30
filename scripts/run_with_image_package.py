#!/usr/bin/env python3
"""Run a Midori program with a temporary MIDORI_PATH.

This avoids installing packages system-wide.
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_PROGRAM = REPO_ROOT / "misc" / "fractal_renderer.mdr"
DEFAULT_IMAGE_PACKAGE = Path(r"C:\Users\jk381\source\repos\ZhongLienong\Midori\reference_package")


def find_midori_exe(repo: Path) -> Path | None:
    candidates = [
        repo / "out" / "build" / "x64-release" / "out" / "Midori.exe",
        repo / "out" / "build" / "x64-development" / "out" / "Midori.exe",
        repo / "out" / "build" / "x64-debug" / "out" / "Midori.exe",
        repo / "build" / "out" / "Midori.exe",
        repo / "build" / "Release" / "Midori.exe",
        repo / "build" / "Midori.exe",
    ]
    for candidate in candidates:
        if candidate.is_file():
            return candidate.resolve()
    return None


def split_path_list(value: str) -> list[str]:
    if value.strip() == "":
        return []
    return [item.strip() for item in value.split(os.pathsep) if item.strip() != ""]


def append_unique_path(existing: str, to_append: Path) -> str:
    candidate = str(to_append.resolve())
    entries = split_path_list(existing)
    normalized = [os.path.normcase(os.path.normpath(item)) for item in entries]
    if os.path.normcase(os.path.normpath(candidate)) in normalized:
        return os.pathsep.join(entries)
    return os.pathsep.join(entries + [candidate])


def build_midori_path(image_package: Path, prelude_dir: Path) -> str:
    midori_path = os.environ.get("MIDORI_PATH", "")
    midori_path = append_unique_path(midori_path, image_package)
    midori_path = append_unique_path(midori_path, prelude_dir)
    return midori_path


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Run Midori with a temporary MIDORI_PATH.")
    parser.add_argument("program", nargs="?", default=str(DEFAULT_PROGRAM), help="Midori program to run.")
    parser.add_argument("--midori-exe", default="", help="Path to Midori.exe.")
    parser.add_argument("--image-package", default=str(DEFAULT_IMAGE_PACKAGE), help="Path to Image package root.")
    args = parser.parse_args(argv)

    midori_exe = Path(args.midori_exe).expanduser().resolve() if args.midori_exe else find_midori_exe(REPO_ROOT)
    if midori_exe is None or not midori_exe.is_file():
        print("Midori.exe not found. Build Midori or pass --midori-exe.", file=sys.stderr)
        return 1

    program_path = Path(args.program).expanduser().resolve()
    if not program_path.is_file():
        print(f"Program not found: {program_path}", file=sys.stderr)
        return 1

    image_package = Path(args.image_package).expanduser().resolve()
    if not (image_package / "package.midori").is_file():
        print(f"Image package not found: {image_package}", file=sys.stderr)
        return 1

    prelude_dir = REPO_ROOT / "MidoriPrelude"
    midori_path = build_midori_path(image_package, prelude_dir)

    env = os.environ.copy()
    env["MIDORI_PATH"] = midori_path

    return subprocess.call([str(midori_exe), str(program_path)], env=env)


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
