#!/usr/bin/env python3
"""scripts/ci/py_quality_ratchet.py's own CLI, black-box: `check` applies the migrated tools' strictness (a row past a limit
there is refused whatever the baseline says) and is an error without the list of them."""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "py_quality_ratchet.py")
BASELINE = "test/resources/py-quality-baseline.tsv"
LIMITS = "".join(f"limit\t{kind}\t10\n" for kind in ("cognitive", "cyclomatic", "inner-defs", "method-lines", "file-lines",
                                                  "copies", "clone-tokens", "fanout", "cycle", "pyright"))
OVER = LIMITS + "cognitive\tscripts/tool.py:work\t12\n"


class PyQualityRatchetCliTest(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        os.makedirs(os.path.join(self.dir.name, "test/resources"))
        self.write(BASELINE, OVER)  # the row is recorded: only strictness can refuse it
        self.write("over.tsv", OVER)

    def write(self, name: str, text: str) -> str:
        path = os.path.join(self.dir.name, name)
        with open(path, "w") as f:
            f.write(text)
        return path

    def check(self, *args: str) -> subprocess.CompletedProcess:
        return subprocess.run([sys.executable, "-I", "-B", TOOL, "check", "--current", "over.tsv", *args],
                              cwd=self.dir.name, capture_output=True, text=True)

    def test_a_recorded_row_in_a_migrated_tool_is_refused(self) -> None:
        listed = self.write("migrated.txt", "scripts/tool.py\n")
        done = self.check("--migrated", listed)
        self.assertEqual(1, done.returncode, done.stdout + done.stderr)
        self.assertIn("no row is admitted", done.stdout)

    def test_a_recorded_row_in_an_unmigrated_tool_passes(self) -> None:
        listed = self.write("migrated.txt", "scripts/other.py\n")
        done = self.check("--migrated", listed)
        self.assertEqual(0, done.returncode, done.stdout + done.stderr)

    def test_check_without_the_migrated_list_is_an_error_never_a_pass(self) -> None:
        done = self.check()
        self.assertEqual(2, done.returncode, done.stdout + done.stderr)
        self.assertIn("--migrated", done.stderr)

    def test_an_unreadable_migrated_list_is_an_error(self) -> None:
        self.assertEqual(2, self.check("--migrated", "no-such.txt").returncode)


if __name__ == "__main__":
    unittest.main()
