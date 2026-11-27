#!/usr/bin/env python3
"""tool_python.py: a tool's Python child runs `-I -B` -- isolated, and writing no bytecode. -I implies -E, which drops
PYTHONDONTWRITEBYTECODE, so a child run `-I` alone writes the __pycache__ of every module of the tree it imports into
the checkout (identity-measure's scripts/ci/__pycache__, 3331008cd). The lint: no launch in scripts/ says `python3 -I`
without -B, and no Python tool spells "-I" itself -- it asks tool_python.isolated."""
# script-test-reads: scripts/*
from __future__ import annotations

import os
import re
import sys
import tempfile
import unittest
from typing import Iterator, List, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
SCRIPTS = os.path.dirname(HERE)
sys.path.insert(0, HERE)
import cli_harness  # noqa: E402

SKIPPED_DIRECTORIES = {"__pycache__", "node_modules", "target", ".venv"}
# `python3 -I`, or an interpreter picked into a variable (`"$STALL_PYTHON" -I`: stall-watch.sh's watchdog wrote
# scripts/lib/__pycache__/machine.*.pyc beside every test it watched, Tool self-tests #622)
BARE_SHELL = re.compile(r"(?:\bpython3|\$\{?[A-Z_]*PY[A-Z_]*\}?)\"? -I(?! -B)(?=[\s\"']|$)")
LIST_FLAG = re.compile(r"\"-I\"")
LIST_FLAG_WITH_B = re.compile(r"\"-I\",\s*\"-B\"")


def sources() -> Iterator[Tuple[str, List[str]]]:
    for top, directories, files in os.walk(SCRIPTS):
        directories[:] = sorted(d for d in directories if d not in SKIPPED_DIRECTORIES and not d.startswith("."))
        for name in sorted(files):
            path = os.path.join(top, name)
            try:
                with open(path, encoding="utf-8") as f:
                    yield os.path.relpath(path, os.path.dirname(SCRIPTS)), f.read().splitlines()
            except (UnicodeDecodeError, OSError):
                continue


def findings() -> List[str]:
    found: List[str] = []
    for path, lines in sources():
        if path == os.path.join("scripts", "lib", "tool_python.py") or path.endswith("tool_python_test.py"):
            continue
        is_test = path.endswith("_test.py")
        for number, line in enumerate(lines, 1):
            if BARE_SHELL.search(line):
                found.append(f"{path}:{number}: python3 -I without -B: {line.strip()}")
            elif path.endswith(".py") and LIST_FLAG.search(line):
                if not is_test:
                    found.append(f"{path}:{number}: a Python child spelt by hand, not tool_python.isolated: {line.strip()}")
                elif not LIST_FLAG_WITH_B.search(line):
                    found.append(f"{path}:{number}: \"-I\" without \"-B\": {line.strip()}")
    return found


class IsolatedTest(unittest.TestCase):
    def test_the_argv_is_isolated_and_writes_no_bytecode(self) -> None:
        import tool_python
        self.assertEqual(["python3", "-I", "-B", "x.py", "a"], tool_python.isolated("x.py", "a"))
        self.assertEqual([sys.executable, "-I", "-B", "-c", "pass"],
                         tool_python.isolated("-c", "pass", python=sys.executable))

    def test_a_child_importing_a_module_of_its_tree_leaves_no_pycache(self) -> None:
        import tool_python
        with tempfile.TemporaryDirectory() as d:
            with open(os.path.join(d, "sibling.py"), "w") as f:
                f.write("VALUE = 1\n")
            with open(os.path.join(d, "main.py"), "w") as f:
                f.write("import os, sys\nsys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))\n"
                        "import sibling\nprint(sibling.VALUE)\n")
            done = cli_harness.run_bounded(tool_python.isolated(os.path.join(d, "main.py"), python=sys.executable),
                                           env=cli_harness.isolated_env(os.environ), cwd=d, stdin=None)
            self.assertEqual((0, "1\n"), (done.code, done.stdout))
            self.assertFalse(os.path.exists(os.path.join(d, "__pycache__")))


class LintTest(unittest.TestCase):
    def test_no_launch_in_scripts_runs_python_isolated_without_dont_write_bytecode(self) -> None:
        self.assertEqual([], findings())

    def test_the_lint_sees_each_bare_form(self) -> None:
        self.assertTrue(BARE_SHELL.search('exec python3 -I "$(dirname "$0")/x.py"'))
        self.assertTrue(BARE_SHELL.search("python3 -I - <<'PY'"))
        self.assertTrue(BARE_SHELL.search("x=$(python3 -I -c 'print(1)')"))
        self.assertFalse(BARE_SHELL.search('exec python3 -I -B "$(dirname "$0")/x.py"'))
        self.assertFalse(BARE_SHELL.search("# under `python3 -I` too"))
        self.assertTrue(LIST_FLAG.search('["python3", "-I", script]'))
        self.assertFalse(LIST_FLAG_WITH_B.search('[sys.executable, "-I", TOOL]'))
        self.assertTrue(LIST_FLAG_WITH_B.search('[sys.executable, "-I", "-B", TOOL]'))


if __name__ == "__main__":
    unittest.main()
