#!/usr/bin/env python3
"""scripts/ci/verified-ref.sh, black-box, over a scratch repository and its bare origin: a row's last green base, kept
as a marker under refs/verified/<name>; advanced only forward; and, for main.yml's old-ref compile, a tool change in a
push whose queued Main run was replaced still in the next run's range (2026-11-21: the push's `before` left it out).
Ported from verified-ref-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/verified_ref.py", "scripts/ci/verified-ref.sh")
REF = "refs/verified/old-ref-compile"


def slice_tool_patterns() -> str:
    """main.yml's old-ref gate patterns (the `slice-tools` step's `patterns: |` block), one a line."""
    with open(os.path.join(cli_harness.ROOT, ".github", "workflows", "main.yml")) as f:
        text = f.read()
    block = text[text.index("id: slice-tools"):]
    lines = block[block.index("patterns: |"):].split("\n")[1:]
    taken = []
    for line in lines:
        if not line.strip():
            break
        taken.append(line.strip())
    return "\n".join(taken) + "\n"


class VerifiedRefTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.origin, self.checkout = self.origin_and_checkout()
        self.a = self.commit("docs/a.md", "the base")
        self.git(self.checkout, "push", "-q", "origin", "main")

    def commit(self, path: str, message: str) -> str:
        full = os.path.join(self.checkout, path)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a") as f:
            f.write(message + "\n")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def vref(self, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, cwd=self.checkout)

    def base(self) -> str:
        done = self.vref("base", "old-ref-compile")
        self.assertEqual(0, done.code, done.output)
        return done.stdout.strip()

    def matches(self, since: str, head: str) -> str:
        """Whether main.yml's old-ref gate takes any path changed in since..head (the real patterns, matches.sh)."""
        patterns = self.write("patterns", slice_tool_patterns())
        changed = self.git(self.checkout, "diff", "--name-only", since, head) + "\n"
        done = subprocess.run(["bash", os.path.join(cli_harness.ROOT, ".github/actions/changed-paths/matches.sh"),
                               patterns], input=changed, capture_output=True, text=True, check=True)
        return done.stdout.strip()

    def test_no_marker_yet_is_no_base_said_by_nothing(self) -> None:
        done = self.vref("base", "old-ref-compile")
        self.assertEqual((0, ""), (done.code, done.stdout), done.output)

    def test_a_green_run_advances_the_marker_to_head_as_a_marker_commit_never_mains_own(self) -> None:
        done = self.vref("advance", "old-ref-compile")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"  {REF} -> {self.a[:9]}\n", done.stdout)
        self.assertEqual(self.a, self.base())
        self.assertEqual(self.a, self.git(self.origin, "cat-file", "blob", f"{REF}:verified"))
        self.assertEqual("", self.git(self.origin, "log", "-1", "--format=%P", REF))

    def test_a_replaced_runs_tool_change_stays_in_the_next_runs_range(self) -> None:
        self.vref("advance", "old-ref-compile")
        b = self.commit("worker/src/test/scala/scripts/IdentitySlice.scala", "a tool change")
        c = self.commit("docs/c.md", "a docs change")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.assertIn("worker/src/test/scala/scripts/IdentitySlice.scala", slice_tool_patterns().split("\n"))
        self.assertEqual(self.a, self.base())
        self.assertEqual("true", self.matches(self.base(), c))
        self.assertEqual("false", self.matches(b, c))

    def test_a_later_green_run_advances_past_both_and_an_older_one_never_moves_it_back(self) -> None:
        self.vref("advance", "old-ref-compile")
        b = self.commit("worker/src/test/scala/scripts/IdentitySlice.scala", "a tool change")
        c = self.commit("docs/c.md", "a docs change")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.assertEqual(0, self.vref("advance", "old-ref-compile").code)
        self.assertEqual(c, self.base())
        self.git(self.checkout, "checkout", "-q", b)
        done = self.vref("advance", "old-ref-compile")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"  {REF} is past {b[:9]} already (a newer run advanced it)\n", done.stdout)
        self.assertEqual(c, self.base())

    def test_a_ref_in_its_first_form_main_s_commit_itself_is_read_as_that_commit(self) -> None:
        self.git(self.checkout, "push", "-q", "origin", f"{self.a}:{REF}")
        self.assertEqual(self.a, self.base())

    def test_usage_is_refused_exit_2(self) -> None:
        for args in (("everything", "old-ref-compile"), ("base",), ("base", "x", "y")):
            with self.subTest(args=args):
                done = self.vref(*args)
                self.assertEqual(2, done.code, done.output)
                self.assertRegex(done.stderr, re.compile(r"usage: .* base\|advance <name>"))

    def test_an_origin_that_cannot_be_read_is_no_base_and_no_advance_exit_2(self) -> None:
        self.git(self.checkout, "remote", "set-url", "origin", self.path("no-such-origin"))
        done = self.vref("base", "old-ref-compile")
        self.assertEqual((2, ""), (done.code, done.stdout), done.output)
        self.assertIn(f"::error title={REF}::could not read {REF} (git ls-remote exit", done.stderr)
        done = self.vref("advance", "old-ref-compile")
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"::error title={REF}::could not advance {REF} to {self.a[:9]}: the next run would judge from a "
                      "stale base", done.stdout)


if __name__ == "__main__":
    unittest.main()
