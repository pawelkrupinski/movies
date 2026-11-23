#!/usr/bin/env python3
"""scripts/scalafix-organize, black-box, over a scratch repository and a fake sbt: it refuses a checkout with uncommitted
changes, rewrites in the kept scalafix tree (made once, seeded from the caller's build, then moved to HEAD and reused)
with SemanticDB on and scalafix asked of every configuration (only the Compile ones with --main), and lays the rewrite
over the checkout and its index; --check changes nothing and lists what would change; a source set already in order is
said; a failed scalafix shows its log. Ported from scalafix-organize-test.sh case for case, plus the lines it prints;
the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from scalafix_tree_fixture import CONFIGURATIONS, SEMANTICDB, ScalafixToolCase, cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/scalafix_organize.py", "scripts/scalafix-organize")
SOURCE = "base/src/main/scala/tools/Genres.scala"
# the fake sbt: a base/Compile scalafix sorts the source's imports (as OrganizeImports would); fails when asked
SBT = '''
if env.get("SBT_FAILS"):
    out("[error] scalafix tripped")
    exit(1)
for a in argv:
    if a.startswith("base/Compile/scalafix"):
        path = "base/src/main/scala/tools/Genres.scala"
        with open(path) as f:
            lines = f.read().splitlines()
        rest = [l for l in lines if not l.startswith("import")]
        with open(path, "w") as f:
            f.write("\\n".join(rest[:2] + sorted(l for l in lines if l.startswith("import")) + rest[2:]) + "\\n")
'''


class ScalafixOrganizeTest(ScalafixToolCase):
    TOOL = TOOL
    SOURCES = {SOURCE: "package tools\n\nimport scala.util.Try\nimport java.util.Locale\n\nobject Genres\n"}
    SBT = SBT

    def test_uncommitted_changes_are_refused(self) -> None:
        with open(os.path.join(self.checkout, SOURCE), "a") as f:
            f.write("// wip\n")
        done = self.scalafix()
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("scalafix-organize: the checkout has uncommitted changes (commit or stash them first: the "
                         f"rewrite is of HEAD):\n M {SOURCE}\n", done.stderr)
        self.assertEqual([], self.sbt_calls())

    def test_check_lists_what_would_change_and_changes_nothing(self) -> None:
        done = self.scalafix("--check")
        self.assertEqual(1, done.code, done.output)
        lines = done.stdout.splitlines()
        self.assertEqual(SOURCE, lines[0])
        self.assertRegex(lines[1], r"^scalafix-organize: 1 source\(s\) would change \(\d+ s\)$")
        self.assertEqual("", self.status())
        self.assertEqual("", self.git(self.tree, "status", "--porcelain"), "the tree is moved back to HEAD")
        calls = self.sbt_calls()
        self.assertEqual([SEMANTICDB + [f"{c}/scalafix " for c in CONFIGURATIONS]], [c["argv"] for c in calls])
        self.assertEqual([self.tree], [c["cwd"] for c in calls], "in the kept tree")

    def test_main_asks_only_the_compile_configurations(self) -> None:
        self.scalafix("--check", "--main")
        self.assertEqual([SEMANTICDB + [f"{c}/scalafix " for c in CONFIGURATIONS if c.endswith("/Compile")]],
                         [c["argv"] for c in self.sbt_calls()])

    def test_a_rewrite_is_laid_over_the_checkout_and_staged(self) -> None:
        done = self.scalafix()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"M  {SOURCE}", self.status())
        self.assertEqual("import java.util.Locale", [l for l in self.read(f"repo/{SOURCE}").splitlines()
                                                     if l.startswith("import")][0])
        lines = done.stdout.splitlines()
        self.assertRegex(lines[0], r"^ 1 file changed, ")
        self.assertRegex(lines[1], r"^scalafix-organize: laid over the checkout and its index \(\d+ s\)$")
        self.assertEqual(2, len(lines), done.stdout)
        self.assertEqual([], self.work_left("scalafix-organize"),
                         "the work directory is removed")

    def test_the_tree_is_made_once_seeded_then_reused_at_the_new_head(self) -> None:
        self.scalafix()
        self.git(self.checkout, "commit", "-qm", "organised")
        done = self.scalafix()
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"^scalafix-organize: every source's imports are in order \(\d+ s\)\n$")
        self.assertEqual(self.git(self.checkout, "rev-parse", "HEAD"), self.git(self.tree, "rev-parse", "HEAD"))
        self.assertEqual([{"route": "local", "seed": self.checkout}, {"route": "local", "seed": None}], self.sbt_env())

    def test_check_with_nothing_to_change(self) -> None:
        self.scalafix()
        self.git(self.checkout, "commit", "-qm", "organised")
        done = self.scalafix("--check")
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"^scalafix-organize: every source's imports are in order \(\d+ s\)\n$")

    def test_a_failed_scalafix_shows_its_log(self) -> None:
        done = self.scalafix(SBT_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("[error] scalafix tripped\n", done.stderr)
        self.assertTrue(done.stderr.endswith("scalafix-organize: scalafix failed (log above)\n"), done.stderr)
        self.assertEqual("", self.status())

    def test_an_unknown_argument_is_a_usage_error(self) -> None:
        done = self.scalafix("--fast")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("scalafix-organize: unknown argument --fast\nscalafix-organize: see scalafix-organize --help\n",
                         done.stderr)

    def test_help_prints_the_header(self) -> None:
        done = self.scalafix("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(re.search(r"^IMPORTS ORGANISED BY SCALAFIX", done.stdout, re.M), done.stdout)
        self.assertIn("\n  scripts/scalafix-organize [--check] [--main]\n", done.stdout)


if __name__ == "__main__":
    unittest.main()
