#!/usr/bin/env python3
"""touched_methods.py: a diff's production lines met with method spans (a changed line, a pure deletion, a line in no
method, a test source left out), and the two reports over the touched methods -- statement coverage from scoverage XML
(a statement counted once over several reports, ignored ones left out, the methods below the bar first) and mutants from
Stryker's JSON (killed, survived, not covered, a timeout killed, a mutant outside the touched methods counted apart)."""

import json
import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import touched_methods  # noqa: E402

CLEAN_ENV = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
GENRES = "scrapers/src/main/scala/services/cinemas/Genres.scala"


def git(repo, *args):
    return subprocess.run(["git", "-C", repo, "-c", "user.name=t", "-c", "user.email=t@example.com", *args],
                          check=True, capture_output=True, text=True, env=CLEAN_ENV).stdout.strip()


def write(repo, path, text):
    full = os.path.join(repo, path)
    os.makedirs(os.path.dirname(full), exist_ok=True)
    with open(full, "w", encoding="utf-8") as f:
        f.write(text)


SOURCE = """package services.cinemas

object Genres {
  def parse(text: String): Seq[String] =
    text.split(',').map(_.trim).toSeq

  def label(genre: String): String = {
    val lower = genre.toLowerCase
    lower.capitalize
  }

  val Known = Set("drama")
}
"""

SPANS = [(GENRES, GENRES + ":Genres.parse", 4, 5), (GENRES, GENRES + ":Genres.label", 7, 10)]


class ChangedLinesTest(unittest.TestCase):
    def test_a_hunk_marks_its_new_lines_and_a_deletion_the_lines_beside_it(self):
        diff = ("diff --git a/x.scala b/x.scala\n--- a/x.scala\n+++ b/x.scala\n"
                "@@ -4,2 +4,3 @@\n-a\n-b\n+a\n+b\n+c\n@@ -9,2 +10,0 @@\n-gone\n-gone\n")
        self.assertEqual(touched_methods.changed_lines(diff), {"x.scala": {4, 5, 6, 10, 11}})

    def test_a_deleted_file_marks_nothing(self):
        diff = "diff --git a/x.scala b/x.scala\n--- a/x.scala\n+++ /dev/null\n@@ -1,3 +0,0 @@\n-a\n"
        self.assertEqual(touched_methods.changed_lines(diff), {})

    def test_only_a_deployed_modules_main_scala_is_production(self):
        self.assertTrue(touched_methods.is_production(GENRES))
        self.assertFalse(touched_methods.is_production("scrapers/src/test/scala/services/cinemas/GenresSpec.scala"))
        self.assertFalse(touched_methods.is_production("testkit/src/main/scala/tools/X.scala"))
        self.assertFalse(touched_methods.is_production("scrapers/src/main/resources/x.tsv"))


class TouchedTest(unittest.TestCase):
    def test_a_line_in_a_method_touches_it_and_one_in_no_method_is_said_apart(self):
        hit, loose = touched_methods.touched(SPANS, {GENRES: {8, 12}})
        self.assertEqual([s[1] for s in hit], [GENRES + ":Genres.label"])
        self.assertEqual(loose, {GENRES: [12]})

    def test_a_range_over_a_repository(self):
        with tempfile.TemporaryDirectory() as repo:
            git(repo, "init", "-q", "-b", "main")
            write(repo, GENRES, SOURCE)
            write(repo, "scrapers/src/test/scala/services/cinemas/GenresSpec.scala", "class GenresSpec\n")
            git(repo, "add", "-A")
            git(repo, "commit", "-qm", "base")
            base = git(repo, "rev-parse", "HEAD")
            write(repo, GENRES, SOURCE.replace("_.trim", "_.trim.toLowerCase"))
            write(repo, "scrapers/src/test/scala/services/cinemas/GenresSpec.scala", "class GenresSpec { }\n")
            git(repo, "commit", "-qam", "head")
            self.assertEqual(touched_methods.changed_paths(repo, base, "HEAD"), [GENRES])
            lines = touched_methods.range_lines(repo, base, "HEAD")
            self.assertEqual(lines, {GENRES: {5}})
            self.assertEqual([s[1] for s in touched_methods.touched(SPANS, lines)[0]], [GENRES + ":Genres.parse"])


def scoverage(statements):
    rows = "".join(f'<statement source="{s}" line="{line}" start="{start}" end="{start + 5}" invocation-count="{count}" '
                   f'ignored="{ignored}"></statement>' for s, line, start, count, ignored in statements)
    return f"<scoverage><packages><package><classes><class><methods><method><statements>{rows}</statements></method>" \
           f"</methods></class></classes></package></packages></scoverage>"


class CoverageTest(unittest.TestCase):
    def test_each_method_counts_its_statements_once_and_the_uncovered_come_first(self):
        with tempfile.TemporaryDirectory() as d:
            root = "/repo"
            first, second = os.path.join(d, "a.xml"), os.path.join(d, "b.xml")
            with open(first, "w") as f:
                f.write(scoverage([(f"{root}/{GENRES}", 5, 100, 0, "false"), (f"{root}/{GENRES}", 5, 110, 3, "false"),
                                   (f"{root}/{GENRES}", 8, 200, 0, "false"), (f"{root}/{GENRES}", 9, 210, 0, "true"),
                                   (f"{root}/{GENRES}", 12, 300, 9, "false")]))
            with open(second, "w") as f:   # the same statement seen run by another module's specs
                f.write(scoverage([(f"{root}/{GENRES}", 5, 100, 1, "false")]))
            counts = touched_methods.coverage_rows(SPANS, [first, second], [root])
            self.assertEqual(counts, {GENRES + ":Genres.parse": [2, 2], GENRES + ":Genres.label": [1, 0]})
            report = touched_methods.coverage_report(SPANS, counts, 80.0)
            self.assertIn("1 below 80%", report[0])
            below = report.index("## below the bar (write pinning tests first)")
            covered = report.index("## covered")
            self.assertIn("Genres.label", report[below + 1])
            self.assertIn("0.0% (0/1)", report[below + 1])
            self.assertIn("100.0% (2/2)", report[covered + 1])

    def test_a_method_no_statement_was_recorded_for_is_below_the_bar(self):
        report = touched_methods.coverage_report(SPANS[:1], {}, 80.0)
        self.assertIn("no statements on record", report[3])


class MutationTest(unittest.TestCase):
    def report(self, d, mutants):
        path = os.path.join(d, "report.json")
        with open(path, "w") as f:
            json.dump({"projectRoot": "/repo/scrapers", "files": {"src/main/scala/services/cinemas/Genres.scala": {
                "mutants": [{"mutatorName": m, "replacement": r, "status": s, "location": {"start": {"line": line}}}
                            for line, m, r, s in mutants]}}}, f)
        return path

    def test_each_method_counts_its_mutants_and_names_every_survivor(self):
        with tempfile.TemporaryDirectory() as d:
            path = self.report(d, [(5, "StringLiteral", '""', "Killed"), (5, "MethodExpression", "_.trim", "Survived"),
                                   (8, "MethodExpression", "toUpperCase", "NoCoverage"), (9, "X", "y", "Timeout"),
                                   (9, "X", "z", "CompileError"), (12, "StringLiteral", '""', "Survived")])
            by_method, outside = touched_methods.mutation_rows(SPANS, [path], ["/repo"])
            self.assertEqual(outside, 1)
            parse, label = by_method[GENRES + ":Genres.parse"], by_method[GENRES + ":Genres.label"]
            self.assertEqual((parse["Killed"], parse["Survived"], parse["NoCoverage"]), (1, 1, 0))
            self.assertEqual((label["Killed"], label["NoCoverage"], label["Other"]), (1, 1, 1))
            report = touched_methods.mutation_report(SPANS, by_method, outside)
            self.assertIn("4 mutants: 2 killed, 1 survived, 1 not covered (50.0% killed)", report[0])
            self.assertIn("1 more in the touched files outside the touched methods", report[0])
            self.assertIn(f"- {GENRES}:5 [Survived] MethodExpression: `_.trim`  (Genres.parse)", report)
            self.assertIn(f"- {GENRES}:8 [NoCoverage] MethodExpression: `toUpperCase`  (Genres.label)", report)

    def test_a_run_with_every_mutant_killed_says_so(self):
        with tempfile.TemporaryDirectory() as d:
            path = self.report(d, [(5, "StringLiteral", '""', "Killed")])
            report = touched_methods.mutation_report(SPANS, *touched_methods.mutation_rows(SPANS, [path], ["/repo"]))
            self.assertIn("## no surviving mutant", report)


if __name__ == "__main__":
    unittest.main()
