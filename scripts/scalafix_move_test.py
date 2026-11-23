#!/usr/bin/env python3
"""scripts/scalafix-move, black-box, over a scratch repository and a fake sbt: it finds the class's source, asks every
configuration (dependents first) for the MoveSymbol rewrite with the two names in the kept scalafix tree, moves the source
to the new package's directory, compiles every configuration, and only then lays the rewrite (a rename kept) over the
checkout and its index; a rewrite that does not compile leaves the checkout untouched; a source holding another class,
an unknown class, an existing target, a bad name and a dirty checkout refuse. Ported from scalafix-move-test.sh case
for case, plus the lines it prints and the work directory it leaves; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from scalafix_tree_fixture import CONFIGURATIONS, SEMANTICDB, ScalafixToolCase, cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/scalafix_move.py", "scripts/scalafix-move")
GENRES = "base/src/main/scala/tools/Genres.scala"
USES = "base/src/main/scala/tools/Uses.scala"
MOVED = "base/src/main/scala/tools/genres/Genres.scala"
RULE = ("--rules=file:scripts/scalafix-rules/MoveSymbol.scala --settings.MoveSymbol.from=tools.Genres"
        " --settings.MoveSymbol.to=tools.genres.Genres")
COMPILES = ["Test/compile", "worker/IntegrationTest/compile", "web/IntegrationTest/compile", "web/PageTest/compile"]
# the fake sbt: the MoveSymbol command does to these two sources what the rule would; a compile, or the rewrite, fails
# when asked
SBT = '''
def rewrite(path, old, new):
    with open(path) as f:
        text = f.read()
    with open(path, "w") as f:
        f.write(text.replace(old, new, 1))
for a in argv:
    if a.startswith("base/Compile/scalafix --rules=file:scripts/scalafix-rules/MoveSymbol.scala"):
        if env.get("REWRITE_FAILS"):
            out("[error] the rule tripped")
            exit(1)
        rewrite("base/src/main/scala/tools/Genres.scala", "package tools\\n", "package tools.genres\\n")
        rewrite("base/src/main/scala/tools/Uses.scala", "package tools\\n", "package tools\\n\\nimport tools.genres.Genres\\n")
    if a == "Test/compile" and env.get("COMPILE_FAILS"):
        out("[error] Not found: Genres")
        exit(1)
'''


class ScalafixMoveTest(ScalafixToolCase):
    TOOL = TOOL
    SOURCES = {GENRES: "package tools\n\nfinal case class Genres(names: Seq[String])\n\n"
                       "object Genres { def none: Genres = Genres(Nil) }\n",
               USES: "package tools\n\nobject Uses { def g: Genres = Genres.none }\n",
               "base/src/main/scala/tools/Pair.scala": "package tools\n\nclass Pair\nclass Other\n"}
    SBT = SBT

    def test_a_rewrite_that_does_not_compile_leaves_the_checkout_untouched(self) -> None:
        done = self.scalafix("tools.Genres", "tools.genres.Genres", COMPILE_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, done.stderr.count("Not found: Genres"), done.output)
        self.assertIn("scalafix-move: the moved code does not compile: the checkout is untouched", done.stderr)
        self.assertEqual("", self.status())
        self.assertEqual("", self.git(self.tree, "status", "--porcelain"), "the tree is reset to HEAD")

    def test_a_failed_rewrite_shows_its_log_and_leaves_the_checkout_untouched(self) -> None:
        done = self.scalafix("tools.Genres", "tools.genres.Genres", REWRITE_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("[error] the rule tripped", done.stderr)
        self.assertIn("scalafix-move: the rewrite failed", done.stderr)
        self.assertEqual("", self.status())

    def test_a_move_asks_every_configuration_for_the_rule_then_compiles(self) -> None:
        done = self.scalafix("tools.Genres", "tools.genres.Genres")
        self.assertEqual(0, done.code, done.output)
        calls = self.sbt_calls()
        self.assertEqual(2, len(calls), calls)
        self.assertEqual(SEMANTICDB + [f"{c}/scalafix {RULE}" for c in CONFIGURATIONS], calls[0]["argv"])
        self.assertEqual(SEMANTICDB + COMPILES, calls[1]["argv"])
        self.assertEqual([self.tree, self.tree], [c["cwd"] for c in calls], "both in the kept tree")
        self.assertEqual(["local", "local"], [e["route"] for e in self.sbt_env()])

    def test_a_move_stages_the_rename_and_the_rewrite_in_the_checkout(self) -> None:
        done = self.scalafix("tools.Genres", "tools.genres.Genres")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(f"M  {USES}\nR  {GENRES} -> {MOVED}", self.status())
        self.assertEqual("package tools.genres", self.read(f"repo/{MOVED}").splitlines()[0])
        self.assertIn("import tools.genres.Genres\n", self.read(f"repo/{USES}"))
        lines = done.stdout.splitlines()
        self.assertEqual(f"scalafix-move: tools.Genres -> tools.genres.Genres ({GENRES} -> {MOVED})", lines[0])
        self.assertRegex(lines[1], r"^ 2 files changed, ")
        self.assertRegex(lines[2], r"^scalafix-move: compiled and staged in the checkout \(\d+ s\); "
                                   r"now grep -rn 'Genres' for strings and docs$")
        self.assertEqual(3, len(lines), done.stdout)
        self.assertEqual([], self.work_left("scalafix-move"),
                         "the work directory is removed")

    def test_a_source_holding_another_class_is_refused(self) -> None:
        done = self.scalafix("tools.Pair", "tools.pairs.Pair")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("scalafix-move: base/src/main/scala/tools/Pair.scala defines more than Pair (and its companion):"
                      " split it first", done.stderr)
        self.assertEqual([], self.sbt_calls())

    def test_a_class_no_source_defines_is_refused(self) -> None:
        done = self.scalafix("tools.Nothing", "tools.x.Nothing")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("scalafix-move: no source of tools defines Nothing", done.stderr)

    def test_an_existing_target_is_refused(self) -> None:
        done = self.scalafix("tools.Genres", "tools.Uses")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("scalafix-move: base/src/main/scala/tools/Uses.scala exists already", done.stderr)
        self.assertEqual([], self.sbt_calls())

    def test_a_dirty_checkout_is_refused(self) -> None:
        with open(os.path.join(self.checkout, USES), "a") as f:
            f.write("// wip\n")
        done = self.scalafix("tools.Genres", "tools.genres.Genres")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("scalafix-move: the checkout has uncommitted changes", done.stderr)
        self.assertIn(f" M {USES}", done.stderr)
        self.assertEqual([], self.sbt_calls())

    def test_a_name_without_its_package_is_a_usage_error(self) -> None:
        done = self.scalafix("Genres", "tools.genres.Genres")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual("scalafix-move: 'Genres' is no full name (package.Name)\n"
                         "scalafix-move: see scalafix-move --help\n", done.stderr)

    def test_other_usage_errors(self) -> None:
        for args, why in ((("tools.A",), "scalafix-move <from.package.Name> <to.package.Name>"),
                          (("tools.A", "tools.A"), "from and to are the same name"),
                          (("tools.A", "tools.9b"), "'tools.9b' is no full name (package.Name)")):
            done = self.scalafix(*args)
            self.assertEqual((2, f"scalafix-move: {why}\nscalafix-move: see scalafix-move --help\n"),
                             (done.code, done.stderr), args)

    def test_help_prints_the_header(self) -> None:
        done = self.scalafix("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(re.search(r"^A CLASS MOVED TO ANOTHER PACKAGE", done.stdout, re.M), done.stdout)
        self.assertIn("\n  scripts/scalafix-move <from.package.Name> <to.package.Name>\n", done.stdout)


if __name__ == "__main__":
    unittest.main()
