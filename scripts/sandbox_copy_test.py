#!/usr/bin/env python3
"""sandbox_copy.py, black-box: a tool copied with its import closure runs from the scratch tree; a missing tool and a
missing argument are usage errors."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/sandbox_copy.py")


class SandboxCopyTest(cli_harness.ToolCase):
    TOOL = TOOL

    def test_a_tool_is_copied_with_what_it_imports_and_runs_from_there(self) -> None:
        dest = self.path("tree")
        done = self.run_tool(dest, "scripts/ci/verified_ref.py")
        self.assertEqual(0, done.code, done.output)
        for path in ("scripts/ci/verified_ref.py", "scripts/lib/tool_git.py", "scripts/lib/tool_process.py",
                     "scripts/lib/processes/machine.py"):  # a module of a library package, by its dotted import
            self.assertIn(path, done.stdout.split())
        self.assertNotIn("scripts/lib/harness/cli_harness.py", done.stdout.split())
        ran = self.run_tool(tool=os.path.join(dest, "scripts/ci/verified_ref.py"))
        self.assertIn("usage: verified-ref.sh", ran.output)

    def test_a_python_tool_without_a_py_name_is_read_for_its_imports_too(self) -> None:
        done = self.run_tool(self.path("tree"), "scripts/label-lookup")
        self.assertEqual(0, done.code, done.output)
        for path in ("scripts/label-lookup", "scripts/ci/identity_judgements.py", "scripts/lib/tool_process.py"):
            self.assertIn(path, done.stdout.split())

    def test_a_missing_tool_or_destination_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.run_tool(self.path("t"), "scripts/ci/nope.py").code)
        self.assertEqual(2, self.run_tool(self.path("t")).code)


if __name__ == "__main__":
    unittest.main()
