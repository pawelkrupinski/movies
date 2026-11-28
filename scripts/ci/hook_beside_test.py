#!/usr/bin/env python3
"""scripts/ci/hook-beside.sh, black-box (cli_harness): <check> runs beside the share's other checks, at once, each told
its part; either failing fails the job; with nothing to run beside, the command runs as it is. Ported from
hook-beside-test.sh case for case; the same cases hold the shell tool and its Python rewrite.
Run: python3 scripts/ci/hook_beside_test.py"""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from harness import cli_harness  # noqa: E402

# A fake hook: says which part it was given, and exits $RC_ONLY (the beside run) or $RC_REST (the rest's). Each part
# waits for the other's mark before it ends: only two runs AT ONCE get past it -- one after another, the first waits
# out its bound and says so. Proven so, never by elapsed wall time.
HOOK = """part="rest"; [ -n "${PREPUSH_ONLY:-}" ] && part="only"
echo "$part ONLY=${PREPUSH_ONLY:-} ELSEWHERE=${PREPUSH_ELSEWHERE:-} args=$*"
echo "$part TREE_CHECK=${RUN_SCRIPT_TESTS_TREE_CHECK:-unset}"
touch "$MARKS/$part"
other=rest; [ "$part" = rest ] && other=only
if [ -n "${WAIT_FOR_OTHER:-}" ]; then
    for _ in $(seq 1 200); do [ -e "$MARKS/$other" ] && break; sleep 0.05; done
    [ -e "$MARKS/$other" ] || echo "$part: NEVER SAW $other"
fi
[ "$part" = only ] && exit "${RC_ONLY:-0}"
exit "${RC_REST:-0}"
"""


class HookBesideTest(cli_harness.ToolCase):
    TOOL = cli_harness.tool_path("scripts/ci/hook_beside.py", "scripts/ci/hook-beside.sh")

    def setUp(self) -> None:
        super().setUp()
        self.hook = self.fakes.script("hook", HOOK, shell="/usr/bin/env bash")
        os.makedirs(self.path("marks"))
        self.tool_env = {"MARKS": self.path("marks"), "TMPDIR": self.path("tmp"), "RUN_SCRIPT_TESTS_TREE_CHECK": None}

    def beside(self, **env: Optional[str]) -> cli_harness.Done:
        return self.run_tool("scripttests", "--", self.hook, "--checks-only", "base..HEAD",
                             env={**self.tool_env, **env})

    def test_the_check_and_the_rest_run_at_once_each_told_its_part(self) -> None:
        done = self.beside(CHECKS="scripttests sbt actionlint", PREPUSH_ELSEWHERE="identity", WAIT_FOR_OTHER="1")
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("NEVER SAW", done.stdout)
        self.assertIn("\nonly ONLY=scripttests ", done.stdout)
        self.assertIn("\nrest ONLY= ELSEWHERE=identity scripttests args=--checks-only base..HEAD\n", done.stdout)

    def test_the_beside_runs_output_is_printed_whole_after_the_rests_and_kept_in_the_temp_dir(self) -> None:
        done = self.beside(CHECKS="scripttests sbt", WAIT_FOR_OTHER="1")
        parts = re.findall(r"^(rest|only) ", done.stdout, re.M)
        self.assertEqual(["rest", "rest", "only", "only"], parts)
        self.assertIn("only ONLY=scripttests", self.read("tmp/scripttests-beside.log"))
        self.assertIn("── scripttests, run beside the rest (exit 0):", done.stdout)

    def test_the_beside_run_keeps_the_tree_check_off_and_the_rests_is_untouched(self) -> None:
        done = self.beside(CHECKS="scripttests sbt")
        self.assertEqual(["only TREE_CHECK=0", "rest TREE_CHECK=unset"],
                         sorted(re.findall(r"^(?:only|rest) TREE_CHECK=\S+$", done.stdout, re.M)))

    def test_failures_and_the_stall_signal(self) -> None:
        for env, code in (({"RC_ONLY": "1"}, 1), ({"RC_REST": "1"}, 1), ({"RC_REST": "75", "RC_ONLY": "1"}, 1),
                          ({"RC_ONLY": "75"}, 75)):
            with self.subTest(env):
                self.assertEqual(code, self.beside(CHECKS="scripttests sbt", **env).code)

    def test_an_inherited_prepush_only_never_reaches_the_rests_part(self) -> None:
        done = self.beside(CHECKS="scripttests sbt", PREPUSH_ONLY="scripttests", WAIT_FOR_OTHER="1")
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("NEVER SAW", done.stdout)
        self.assertEqual(1, done.stdout.count("\nrest ONLY= ELSEWHERE=scripttests "))

    def test_no_check_named_in_the_share_runs_the_hook_once_as_it_is(self) -> None:
        done = self.beside(CHECKS="sbt actionlint", PREPUSH_ELSEWHERE="identity")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, done.stdout.count("rest ONLY= ELSEWHERE=identity args="))
        self.assertNotRegex(done.stdout, r"(?m)^only ")

    def test_the_check_alone_runs_the_hook_once_its_exit_the_jobs_and_its_tree_check_on(self) -> None:
        done = self.beside(CHECKS="scripttests", RC_REST="3")
        self.assertEqual(3, done.code, done.output)
        self.assertEqual(2, len(re.findall(r"^rest ", done.stdout, re.M)))
        self.assertIn("rest TREE_CHECK=unset", done.stdout)
        self.assertNotRegex(done.stdout, r"(?m)^only ")

    def test_a_command_line_without_the_separator_is_a_usage_error(self) -> None:
        done = self.run_tool("scripttests", "x", "y")
        self.assertEqual(2, done.code)
        self.assertIn("usage:", done.stderr)


if __name__ == "__main__":
    unittest.main()
