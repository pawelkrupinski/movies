#!/usr/bin/env python3
"""ratchet_diff.py (scripts/ratchet-diff): every ratchet row changed between two refs, read from the committed
baselines at each -- marked fell / rose / new / removed / moved (a source git renames keeps its rows, a class moved to
another package keeps its fan-out row) -- with each metric's totals and rows still over the limit; the head side from
measured rows instead (--measure's) shows a fall the baselines never recorded; --tsv gives the same rows."""

from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
TOOL = os.path.join(HERE, "ratchet_diff.py")
WRAPPER = os.path.join(os.path.dirname(HERE), "ratchet-diff")
SCRATCH_GIT = os.path.join(os.path.dirname(os.path.dirname(HERE)), "scripts/scratch-git.sh")

COMPLEXITY = "test/resources/complexity-baseline.tsv"
LENGTH = "test/resources/length-baseline.tsv"
DUPLICATION = "test/resources/duplication-baseline.tsv"
COUPLING = "test/resources/coupling-baseline.tsv"
JS = "test/resources/js-quality-baseline.tsv"
JS_LIMITS = "limit\tcognitive\t10\nlimit\tdistance\t0.2\n"
OLD_SRC = "web/src/main/scala/services/Old.scala"
NEW_SRC = "web/src/main/scala/services/moved/Old.scala"
UNTOUCHED_SRC = "web/src/main/scala/services/Untouched.scala"
SOURCE = "".join(f"  def line{i}(x: Int): Int = x * {i} + {i * i} // a body git can follow\n" for i in range(30))


def complexity(*rows: str) -> str:
    return "\n".join(["# the complexity baseline", "limit\tcognitive\t15", "limit\tcyclomatic\t10", "limit\tinner-defs\t2", *rows]) + "\n"


def coupling(*rows: str) -> str:
    return "\n".join(["limit\tfanout\t9", "limit\tdistance\t20", "ce\tmodels\t3", *rows]) + "\n"


BASE_FILES = {
    COMPLEXITY: complexity(f"cognitive\t{OLD_SRC}:Old.run\t30", "cognitive\tweb/src/main/scala/A.scala:A.fall\t25",
                           "cyclomatic\tweb/src/main/scala/A.scala:A.gone\t14"),
    LENGTH: "limit\tmethod-lines\t50\nlimit\tfile-lines\t800\nmethod-lines\tweb/src/main/scala/A.scala:A.fall\t60\n",
    DUPLICATION: "limit\tcopies\t0\n",
    COUPLING: coupling("fanout\tservices.web.Hub\t20", "fanout\tservices.web.Shrunk\t15", "distance\tservices.web\t30"),
    OLD_SRC: "package services\n\nobject Old {\n" + SOURCE + "}\n",
    UNTOUCHED_SRC: "package services\n\nobject Untouched {\n  val table: Seq[Int] = Seq(1)\n}\n",
    JS: JS_LIMITS + "cognitive\tweb/src/main/assets/js/a.js:render\t15\ndistance\tinfra/version-dashboard/src\t0.85\n",
}
HEAD_FILES = {
    COMPLEXITY: complexity(f"cognitive\t{NEW_SRC}:Old.run\t30", "cognitive\tweb/src/main/scala/A.scala:A.fall\t18",
                           "inner-defs\tweb/src/main/scala/B.scala:B.fresh\t3"),
    LENGTH: BASE_FILES[LENGTH] + f"method-lines\t{UNTOUCHED_SRC}:Untouched.table\t70\n",
    COUPLING: coupling("fanout\tservices.movies.Hub\t20", "fanout\tservices.web.Shrunk\t12", "distance\tservices.web\t31",
                       "cycle-edge\tservices.a->services.b\t2"),
    OLD_SRC: None,
    NEW_SRC: "package services.moved\n\nobject Old {\n" + SOURCE + "}\n",
    JS: JS_LIMITS + "cognitive\tweb/src/main/assets/js/a.js:render\t12\ndistance\tinfra/version-dashboard/src\t0.9\n",
}


class RatchetDiffTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.repo = os.path.join(self.dir.name, "repo")
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
        self.env.update(GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_NOSYSTEM="1", GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@t",
                        GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@t")
        self.scratch("scratch_repo", "-q")
        self.base = self.commit(BASE_FILES, "base")
        self.head = self.commit(HEAD_FILES, "head")

    def tearDown(self):
        self.dir.cleanup()

    def scratch(self, helper, *args):
        return subprocess.run(["bash", "-c", '. "$1"; "$2" "$3" "${@:4}"', "_", SCRATCH_GIT, helper, self.repo, *args],
                              env=self.env, check=True, capture_output=True, text=True).stdout.strip()

    def commit(self, files, message):
        for path, text in files.items():
            full = os.path.join(self.repo, path)
            if text is None:
                os.remove(full)
                continue
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as f:
                f.write(text)
        self.scratch("scratch_git", "add", "-A")
        self.scratch("scratch_git", "commit", "-qm", message)
        return self.scratch("scratch_git", "rev-parse", "HEAD")

    def run_tool(self, *args):
        done = subprocess.run([sys.executable, "-I", "-B", TOOL, "--base", self.base, *args],
                              cwd=self.repo, env=self.env,
                              capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        return done.stdout

    def test_every_changed_row_is_marked(self):
        out = self.run_tool("--head", self.head, "--tsv")
        rows = {tuple(line.split("\t")[:2]): line.split("\t")[2:] for line in out.splitlines() if not line.startswith(("#", "total"))}
        self.assertEqual(["25", "18", "fell"], rows[("cognitive", "web/src/main/scala/A.scala:A.fall")])
        self.assertEqual(["14", "-", "removed (within the limit)"], rows[("cyclomatic", "web/src/main/scala/A.scala:A.gone")])
        self.assertEqual(["-", "3", "new"], rows[("inner-defs", "web/src/main/scala/B.scala:B.fresh")])
        self.assertEqual(["-", "70", "measured"], rows[("method-lines", f"{UNTOUCHED_SRC}:Untouched.table")],
                         "a new row of a source the range left unchanged was newly measured, not newly written")
        self.assertEqual(["30", "30", "moved"], rows[("cognitive", f"{OLD_SRC}:Old.run => {NEW_SRC}:Old.run")])
        self.assertEqual(["20", "20", "moved"], rows[("fanout", "services.web.Hub => services.movies.Hub")])
        self.assertEqual(["15", "12", "fell"], rows[("fanout", "services.web.Shrunk")])
        self.assertEqual(["30", "31", "rose"], rows[("distance", "services.web")])
        self.assertEqual(["-", "2", "new"], rows[("cycle-edge", "services.a->services.b")])
        self.assertNotIn(("method-lines", "web/src/main/scala/A.scala:A.fall"), rows, "an unchanged row is not listed")
        self.assertEqual(["15", "12", "fell"], rows[("js cognitive", "web/src/main/assets/js/a.js:render")])
        self.assertEqual(["0.85", "0.9", "rose"], rows[("js distance", "infra/version-dashboard/src")])

    def test_totals_count_rows_still_over_the_limit(self):
        out = self.run_tool("--head", self.head, "--tsv")
        totals = {line.split("\t")[1]: line.split("\t")[2:] for line in out.splitlines() if line.startswith("total\t")}
        # rows before, after, sum before, after, over the limit before, after, limit
        self.assertEqual(["2", "2", "55", "48", "2", "2", "15"], totals["cognitive"])
        self.assertEqual(["2", "2", "35", "32", "2", "2", "9"], totals["fanout"])
        self.assertEqual(["0", "1", "0", "2", "0", "1", "0"], totals["cycle-edge"])

    def test_markdown_is_a_pasteable_table(self):
        out = self.run_tool("--head", self.head)
        self.assertIn("| metric | row | before | after | change |", out)
        self.assertIn("| fanout | `services.web.Shrunk` | 15 | 12 | fell |", out)
        self.assertIn("| method-lines | 50 | 1 -> 2 | 60 -> 130 | 0 | 0 | 0 | 1 | 0 | 0 | 1 -> 2 |", out)
        self.assertIn("| cognitive | 15 | 2 -> 2 | 55 -> 48 | 1 | 0 | 0 | 0 | 0 | 1 | 2 -> 2 |", out)

    def test_measured_rows_show_a_fall_the_baselines_never_recorded(self):
        measured = os.path.join(self.dir.name, "coupling-rows.tsv")
        with open(measured, "w") as f:
            f.write(coupling("fanout\tservices.movies.Hub\t20", "fanout\tservices.web.Shrunk\t10", "distance\tservices.web\t31"))
        out = self.run_tool("--coupling-rows", measured, "--tsv")
        self.assertIn("fanout\tservices.web.Shrunk\t15\t10\tfell", out)
        self.assertNotIn("cycle-edge\tservices.a->services.b", out)

    def test_measured_js_rows_hold_only_the_production_ones(self):
        measured = os.path.join(self.dir.name, "over-limit.tsv")
        with open(measured, "w") as f:
            f.write(JS_LIMITS + "cognitive\tweb/src/main/assets/js/a.js:render\t11\n"
                    "cognitive\tpage-tests-playwright/tests/helpers.ts:firstVisibleCardIn\t11\n")
        out = self.run_tool("--js-rows", measured, "--tsv")
        self.assertIn("js cognitive\tweb/src/main/assets/js/a.js:render\t15\t11\tfell", out)
        self.assertNotIn("page-tests-playwright", out)

    def test_the_wrapper_takes_a_range(self):
        done = subprocess.run(["bash", WRAPPER, "--tsv", f"{self.base}..{self.head}"], cwd=self.repo, env=self.env,
                              capture_output=True, text=True)
        self.assertEqual(0, done.returncode, done.stderr)
        self.assertIn("fanout\tservices.web.Shrunk\t15\t12\tfell", done.stdout)


if __name__ == "__main__":
    unittest.main()
