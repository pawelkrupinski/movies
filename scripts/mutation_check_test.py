#!/usr/bin/env python3
"""scripts/mutation-check, black-box (cli_harness), over a scratch repository and the stub sbt
(scripts/touched-tree-stub.sh; touched_tree_fixture.py): it measures the head in a worktree of its own (removed
afterwards, the caller's checkout untouched), with the plugins laid over it; mutates only the touched method's source
and runs only the reaching specs, from the repository root, without -Werror; reports each touched method's killed and
surviving mutants, the survivors by line; a range touching no production method, a module no spec reaches, too many
specs and a failed Stryker run each say so; the usage errors exit 2; an INT or TERM still removes the worktree.
Run: python3 scripts/mutation_check_test.py"""
from __future__ import annotations

import os
import signal
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from touched_tree_fixture import SOURCE, TouchedTreeCase  # noqa: E402


class MutationCheckTest(TouchedTreeCase):
    NAME = "mutation-check"
    PYTHON = "mutation_check.py"
    EDITS = (("_.trim", "_.trim.toLowerCase"),)

    def setUp(self) -> None:
        super().setUp()
        self.write("specs.txt", "scrapers services.cinemas.GenresSpec\n")
        self.write("mutants", '5 Killed MethodExpression _.trim\n5 Survived MethodExpression toUpperCase\n'
                              '7 Survived StringLiteral ""\n')
        self.mutants = {"STUB_MUTANTS": self.path("mutants")}

    def test_a_change_counts_the_touched_methods_mutants(self) -> None:
        done = self.run_in_repo("main~2..main~1", "--out", self.path("keep"), env=self.mutants)
        self.assertEqual(0, done.code, done.output)
        lines = done.output.splitlines()
        self.assertEqual(1, lines.count(f"| `{SOURCE}:Genres.parse` | 1 | 1 | 0 |"), done.output)
        self.assertIn("1 more in the touched files outside the touched methods", done.output)
        self.assertEqual(1, lines.count(f"- {SOURCE}:5 [Survived] MethodExpression: `toUpperCase`  (Genres.parse)"),
                         done.output)
        self.assertIn("mutation-check: scrapers -- 1 source(s) mutated, 1 spec(s) run", lines)
        for kept in ("report.md", "report-scrapers.json"):
            self.assertTrue(os.path.isfile(self.path("keep", kept)), kept)

    def test_stryker_mutates_only_the_touched_source_and_runs_only_the_reaching_specs(self) -> None:
        done = self.run_in_repo("main~2..main~1", env=self.mutants)
        self.assertEqual(0, done.code, done.output)
        stryker = [line for line in self.stub_calls() if line.endswith("|scrapers/stryker")]
        self.assertEqual(1, len(stryker), self.stub_calls())
        words = stryker[0].split("|")
        self.assertIn(f'set scrapers/strykerMutate := Seq("{SOURCE}")', words)
        self.assertIn('set scrapers/strykerTestFilter := Seq("services.cinemas.GenresSpec")', words)
        self.assertIn("set scrapers/strykerBaseDir := (LocalRootProject / baseDirectory).value", words)
        self.assertIn('set scrapers/scalacOptions ~= (_.filterNot(_ == "-Werror"))', words)
        self.assertIn("set scrapers/strykerConcurrency := 2", words)
        tree = words[0]
        self.assertEqual(self.path("trees", "mutation-check"), tree.rsplit(".", 1)[0])
        self.assertEqual(1, sum(line.startswith(f"{tree}|touchedSpecs") for line in self.stub_calls()))
        self.assertFalse(os.path.isdir(tree))
        self.assertEqual(1, len(self.git(self.scratch, "worktree", "list").splitlines()))
        self.assertEqual("", self.git(self.scratch, "status", "--porcelain"))
        self.assertEqual([], os.listdir(self.path("tmp")))

    def test_concurrency_reaches_stryker(self) -> None:
        done = self.run_in_repo("main~2..main~1", "--concurrency", "5", env=self.mutants)
        self.assertEqual(0, done.code, done.output)
        stryker = [line for line in self.stub_calls() if line.endswith("|scrapers/stryker")]
        self.assertIn("set scrapers/strykerConcurrency := 5", stryker[0].split("|"))

    def test_a_range_touching_no_production_method(self) -> None:
        done = self.run_in_repo("main~1..main")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("mutation-check: the range touches no production method", done.output)

    def test_no_spec_reaches_the_module(self) -> None:
        self.write("specs.txt", "")
        done = self.run_in_repo("main~2..main~1", env=self.mutants)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("scrapers: no spec of scrapers reaches the touched methods", done.output)
        self.assertEqual([], [line for line in self.stub_calls() if line.endswith("stryker")])

    def test_more_reaching_specs_than_max_specs(self) -> None:
        self.write("specs.txt", "scrapers a.ASpec\nscrapers b.BSpec\n")
        done = self.run_in_repo("main~2..main~1", "--max-specs", "1", env=self.mutants)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("mutation-check: scrapers: 2 specs reach the touched methods, over --max-specs 1", done.output)
        self.assertEqual([], self.trees_left())

    def test_a_failed_stryker_run(self) -> None:
        done = self.run_in_repo("main~2..main~1", env={**self.mutants, "STUB_FAIL_ON": "stryker"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("mutation-check: scrapers: Stryker failed (exit 1; log: ", done.output)
        self.assertIn("[error] ", done.stderr)
        self.assertEqual([], self.trees_left())

    def test_usage_errors_exit_2(self) -> None:
        for args in (("main~2",), (), ("main~2..main~1", "--max-specs", "many"),
                     ("main~2..main~1", "--concurrency", "x"), ("main~2..main~1", "--out"), ("-x",),
                     ("main~2..main~1", "main~1..main"), ("main~2..",), ("main~2..nothere",)):
            done = self.run_in_repo(*args)
            self.assertEqual(2, done.code, (args, done.output))
            self.assertIn("mutation-check: see mutation-check --help", done.stderr, args)
        self.assertEqual([], self.trees_left())

    def test_help_prints_the_header(self) -> None:
        self.assert_help("MUTATION TESTING OF THE METHODS A CHANGE TOUCHES (Stryker4s; a report, never a gate): "
                         "whether the specs that reach")

    def test_term_cleans_up(self) -> None:
        self.assert_signal_cleans_up(signal.SIGTERM, "/stryker", "main~2..main~1")

    def test_int_cleans_up(self) -> None:
        self.assert_signal_cleans_up(signal.SIGINT, "MethodSpans", "main~2..main~1")


if __name__ == "__main__":
    unittest.main()
