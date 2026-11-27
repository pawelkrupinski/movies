#!/usr/bin/env python3
# Purpose: copy tools and every repository module they import into a scratch tree, so a shell self-test can run them there
"""THE SANDBOX COPY for a shell self-test (scripts/lib/sandbox_closure.py; cli_harness.ToolCase.sandbox is the Python
tests' way): a tool copied into a scratch repository alone cannot import the scripts/lib adapters it takes, so a test
copies it with its import closure.

  sandbox_copy.py DEST TOOL...      each TOOL (a path from the repository root) and every repository module its Python
                                    imports, transitively, into DEST at the same relative paths; the paths, one a line

Run from anywhere: the repository is the one this script is in. Tested by sandbox_copy_test.py beside it."""
from __future__ import annotations

import os
import sys
from typing import Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, os.path.join(HERE, "lib"))
import sandbox_closure  # noqa: E402
from tool_files import Files  # noqa: E402


def main(argv: Sequence[str], files: Files) -> int:
    if len(argv) < 2:
        print("usage: sandbox_copy.py DEST TOOL...", file=sys.stderr)
        return 2
    dest, tools = argv[0], argv[1:]
    missing = [t for t in tools if not files.is_file(f"{ROOT}/{t}")]
    if missing:
        print(f"sandbox_copy: no such tool: {', '.join(missing)}", file=sys.stderr)
        return 2
    for path in sandbox_closure.copy(files, ROOT, dest, tools):
        print(path)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], Files.here()))
