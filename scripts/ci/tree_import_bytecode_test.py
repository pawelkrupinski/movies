#!/usr/bin/env python3
"""A tracked Python file that puts a directory of the tree on sys.path turns bytecode off first.

run-script-tests runs every test with PYTHONDONTWRITEBYTECODE=1, but `python3 -I` (and `sys.executable, "-I"`) implies
-E and ignores it: a tool run so that imports a module beside it wrote that module's __pycache__/ into the checkout,
and the runner's tree check failed its test as POLLUTE -- recording-diff-test.sh (Order independence #103), then
ratchet_diff_test.py (#105), one more each run. So the rule is held in the code, not per call site: the first
`sys.path.insert` / `sys.path.append` of every .py of the tree has `sys.dont_write_bytecode = True` on the line before it,
which no command line can undo.

Run: python3 scripts/ci/tree_import_bytecode_test.py
"""
import os
import re
import subprocess
import sys
import tempfile
import unittest

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SYS_PATH = re.compile(r"^\s*sys\.path\.(insert|append)\(")
GUARD = re.compile(r"^\s*sys\.dont_write_bytecode\s*=\s*True\b")


def unguarded(path, text):
    """`path:line` of the first sys.path change when the line before it is not the guard; None when guarded or none."""
    lines = text.splitlines()
    for i, line in enumerate(lines):
        if SYS_PATH.match(line):
            return None if i > 0 and GUARD.match(lines[i - 1]) else f"{path}:{i + 1}"
    return None


# Never the tree's own sources: VCS state, dependencies installed by a tool, build output, editor and bytecode caches.
NOT_SOURCES = {".git", "node_modules", "target", "__pycache__", ".venv", "venv", "build", ".gradle", ".bsp",
               ".metals", ".bloop", ".idea", ".claude"}


def tree_python_files(root):
    """Every .py under `root` outside NOT_SOURCES, as relative paths (read from disk, not git: the shared-machine lint
    allows a test no direct git, and scratch_git refuses the checkout itself)."""
    for directory, subdirectories, names in os.walk(root):
        subdirectories[:] = sorted(d for d in subdirectories if d not in NOT_SOURCES)
        for name in sorted(names):
            if name.endswith(".py"):
                yield os.path.relpath(os.path.join(directory, name), root)


class TreeImportBytecode(unittest.TestCase):
    def test_the_check_tells_a_guarded_file_from_a_bare_one(self):
        self.assertEqual("t.py:2", unguarded("t.py", "import sys\nsys.path.insert(0, 'x')\n"))
        self.assertIsNone(unguarded("t.py", "import sys\nsys.dont_write_bytecode = True\nsys.path.insert(0, 'x')\n"))
        self.assertIsNone(unguarded("t.py", "import sys\nprint(sys.path)\n"))

    def test_a_guarded_import_under_dash_I_leaves_no_pycache(self):
        with tempfile.TemporaryDirectory() as scratch:
            with open(os.path.join(scratch, "beside.py"), "w") as f:
                f.write("X = 1\n")
            tool = os.path.join(scratch, "tool.py")
            with open(tool, "w") as f:
                f.write("import os, sys\nsys.dont_write_bytecode = True\n"
                        "sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))\nimport beside\n")
            subprocess.run([sys.executable, "-I", tool], check=True, timeout=60)
            self.assertFalse(os.path.exists(os.path.join(scratch, "__pycache__")))

    def test_every_python_file_of_the_tree_guards_its_sys_path_change(self):
        bare = []
        for rel in tree_python_files(REPO):
            with open(os.path.join(REPO, rel), encoding="utf-8", errors="replace") as f:
                found = unguarded(rel, f.read())
            if found:
                bare.append(found)
        self.assertEqual([], bare, "put `sys.dont_write_bytecode = True` on the line before each")


if __name__ == "__main__":
    unittest.main()
