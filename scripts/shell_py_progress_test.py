#!/usr/bin/env python3
"""scripts/shell_py_progress.py, black-box over a scratch repository: a MIGRATE tool rewritten in Python, or left a thin
`exec python3` forwarder, is done; one deleted is retired; one still shell is left -- or queued when a --queued branch
migrated it; a renamed tool is read at its new path; and the shell and Python lines are counted, tools and tests apart,
outside src/ trees, at the start against now."""
from __future__ import annotations

import os
import re
import sys
import unittest

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

SHELL_TOOL = "#!/usr/bin/env bash\nset -euo pipefail\necho one\necho two\necho three\necho four\necho five\necho six\n"
FORWARDER = '#!/bin/sh\n# the tool is its .py\nexec python3 "$(dirname "$0")/thin.py" "$@"\n'
TOOLS = ("# rank\tpath\tpath now\n1\tscripts/rewritten\t\n2\tscripts/thin.sh\t\n3\tscripts/retired.sh\t\n"
         "4\tscripts/still.sh\t\n5\tscripts/queued.sh\t\n6\tscripts/old-name\tscripts/new-name\n")


class ShellPyProgressTest(cli_harness.ToolCase):
    TOOL = cli_harness.tool_path("scripts/shell_py_progress.py")

    def setUp(self) -> None:
        super().setUp()
        self.checkout = self.repo("repo", "-b", "main")
        self.write("tools.tsv", TOOLS)
        for name in ("rewritten", "thin.sh", "retired.sh", "still.sh", "queued.sh", "old-name"):
            self.write(f"repo/scripts/{name}", SHELL_TOOL)
        self.write("repo/scripts/still-test.sh", "#!/bin/sh\necho test\n")
        self.write("repo/worker/src/main/tool.sh", SHELL_TOOL)
        self.start = self.commit("the start")
        self.write("repo/scripts/rewritten", "#!/usr/bin/env python3\nprint('one')\n")
        self.write("repo/scripts/thin.sh", FORWARDER)
        self.write("repo/scripts/thin.py", "print('thin')\n")
        self.write("repo/scripts/thin_test.py", "import unittest\n")
        os.remove(self.path("repo/scripts/retired.sh"))
        self.git(self.checkout, "mv", "scripts/old-name", "scripts/new-name")
        self.commit("now")
        self.git(self.checkout, "checkout", "-q", "-b", "queued")
        self.write("repo/scripts/queued.sh", FORWARDER)
        self.commit("a queued change")
        self.git(self.checkout, "checkout", "-q", "main")

    def commit(self, message: str) -> str:
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-q", "-m", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def progress(self, *args: str) -> cli_harness.Done:
        return self.run_tool("--repo", self.checkout, "--no-fetch", "--start", self.start, "--rev", "main",
                             "--tools", self.path("tools.tsv"), *args)

    def test_tools_done_queued_and_left_by_name(self) -> None:
        done = self.progress("--queued", "queued")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("MIGRATE tools: 3 done, 1 queued, 2 left, of 6", done.stdout)
        rows = [line.split() for line in done.stdout.splitlines() if line.startswith("  ") and "/" in line]
        self.assertEqual([["done", "1", "scripts/rewritten"], ["done", "2", "scripts/thin.sh"],
                          ["retired", "3", "scripts/retired.sh"], ["queued", "5", "scripts/queued.sh"],
                          ["left", "4", "scripts/still.sh"], ["left", "6", "scripts/new-name"]], rows)

    def test_without_the_queued_branch_its_tool_is_left(self) -> None:
        self.assertIn("MIGRATE tools: 3 done, 0 queued, 3 left, of 6", self.progress().stdout)

    def test_lines_are_counted_tools_and_tests_apart_outside_src_trees(self) -> None:
        rows = {}
        for line in self.progress().stdout.splitlines():
            found = re.match(r"  (sh|py) (tool|test) +(\d+) \( *(\d+)\) +(\d+) \( *(\d+)\) +([+-]\d+)$", line)
            if found:
                rows[found.group(1, 2)] = list(found.group(3, 4, 5, 6, 7))
        # start: six 8-line shell tools (worker/src/ is not counted); now: three of them and the 3-line forwarder
        self.assertEqual(["48", "6", "27", "4", "-21"], rows[("sh", "tool")])
        self.assertEqual(["2", "1", "2", "1", "+0"], rows[("sh", "test")])
        # the rewritten tool (2 lines) and thin.py (1); the Python test (1)
        self.assertEqual(["0", "0", "3", "2", "+3"], rows[("py", "tool")])
        self.assertEqual(["0", "0", "1", "1", "+1"], rows[("py", "test")])

    def test_an_unknown_option_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.run_tool("--bogus").code)


if __name__ == "__main__":
    unittest.main()
