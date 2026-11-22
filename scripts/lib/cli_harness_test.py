#!/usr/bin/env python3
"""cli_harness.py: a tool run by path sees its fakes first on its PATH and none of the git config, GIT_* or GITHUB_*
around the test; each fake (Python or shell body) logs its argv; a run past its bound ends with its whole process group
(exit 124) and leaves nothing running; a scratch repository refuses to be the checkout; tool_path prefers the Python
version of a tool."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cli_harness  # noqa: E402


class CliHarnessTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.TOOL = self.write("tool.sh", "#!/bin/sh\nprintf 'args:%s\\n' \"$*\"\nenv | sort\n"
                                          "gh api x --flag 'a b' && sbt run\ncat\n")
        os.chmod(self.TOOL, 0o755)

    def test_the_tool_sees_its_fakes_their_calls_logged(self) -> None:
        self.fakes.program("gh", "out('gh said ' + argv[-1])\nexit(0)")
        self.fakes.script("sbt", 'echo "sbt said $*" >&2; exit 0')
        done = self.run_tool("one", "two", stdin="piped\n")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("args:one two", done.stdout)
        self.assertIn("gh said a b", done.stdout)
        self.assertIn("sbt said run", done.stderr)
        self.assertIn("piped", done.stdout)
        self.assertEqual([["api", "x", "--flag", "a b"]], self.fakes.calls("gh"))
        self.assertEqual([["api", "x", "--flag", "a b"], ["run"]], self.fakes.calls())

    def test_the_environment_around_the_test_does_not_reach_the_tool(self) -> None:
        os.environ["GITHUB_OUTPUT"] = "/leak"
        os.environ["GIT_DIR"] = "/leak"
        try:
            done = self.run_tool(env={"MINE": "1", "HOME": None})
        finally:
            del os.environ["GITHUB_OUTPUT"], os.environ["GIT_DIR"]
        self.assertNotIn("/leak", done.stdout)
        self.assertIn("MINE=1", done.stdout)
        self.assertNotIn("\nHOME=", done.stdout)
        self.assertIn("GIT_CONFIG_GLOBAL=" + os.devnull, done.stdout)
        self.assertIn("KINOWO_GOVERNOR=0", done.stdout)

    def test_a_hung_tool_is_ended_with_its_group_and_leaves_nothing_running(self) -> None:
        pids = self.path("hung.pids")
        hung = self.write("hung.sh", f"#!/bin/sh\n(echo $$ >> {pids}; exec sleep 86400) &\necho $! >> {pids}\nexec sleep 86400\n")
        os.chmod(hung, 0o755)
        done = self.run_tool(tool=hung, bound=1)
        self.assertEqual(124, done.code)
        self.assertIn("ended past its 1 s bound", done.stderr)
        self.assertEqual(0, cli_harness.still_running(pids))

    def test_a_scratch_repository_refuses_the_checkout(self) -> None:
        repo = self.repo("r")
        self.write("r/f", "x\n")
        self.git(repo, "add", "f")
        self.git(repo, "commit", "-qm", "one")
        self.assertEqual("Spec", self.git(repo, "log", "-1", "--format=%an"))
        with self.assertRaises(AssertionError):
            self.git(cli_harness.ROOT, "status")

    def test_tool_path_prefers_the_python_version(self) -> None:
        self.assertEqual(os.path.join(cli_harness.ROOT, "scripts/lib/cli_harness.py"),
                         cli_harness.tool_path("scripts/lib/cli_harness.py", "scripts/shell-spec.sh"))
        self.assertEqual(os.path.join(cli_harness.ROOT, "scripts/shell-spec.sh"),
                         cli_harness.tool_path("scripts/lib/no_such.py", "scripts/shell-spec.sh"))

    def test_wait_until_is_bounded(self) -> None:
        self.assertTrue(cli_harness.wait_until(lambda: True, 0.1))
        self.assertFalse(cli_harness.wait_until(lambda: False, 0.1))


if __name__ == "__main__":
    unittest.main()
