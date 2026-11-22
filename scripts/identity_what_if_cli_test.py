#!/usr/bin/env python3
# script-test-reads: scripts/identity-what-if scripts/identity_what_if*.py scripts/identity-tools.sh scripts/identity-what-if-roots.tsv scripts/pure-move-check scripts/pure_move_check.py scripts/identity_stamp_carry.py
"""scripts/identity-what-if, black-box: --bisect over kept sides (the first commit whose take of the listing moved, or
the corpus when none did), then THE MAIN PATH over stub JVMs -- an inert change starts no head JVM and compiles no head,
the base's whole side runs in a one-shot JVM, a head that resolves compiles beside the base's resolve, every family is
re-resolved by default and the verdict recorded whole and published, a pure move resolves nothing, head = base must move
nothing, a ref is laid out once and a step before the resolve failing says which. The mirror's cases are in
identity_what_if_cli_mirror_test.py. Ported from identity-what-if-test.sh; unchanged across the swap to Python."""
from __future__ import annotations

import json
import os
import sys
import unittest

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from identity_what_if_cli_fixture import GENERATION, WhatIfCase  # noqa: E402

AT = "2026-11-05T20:10:00Z"
SINCE = "2026-11-04T20:10:00Z"


class BisectTest(WhatIfCase):
    def test_names_the_first_commit_whose_take_of_the_listing_moved(self) -> None:
        for commit, take in (("c0", "none"), ("c1", "none"), ("c2", "tmdb:5"), ("c3", "tmdb:5")):
            self.side(commit, AT, take, take)
        done = self.bisect("c0", "c3", AT)
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"FIRST MOVED BY {self.c['c2'][:7]}", done.stdout)
        self.assertRegex(done.stdout, r"FIRST MOVED BY [0-9a-f]+ moves the take")
        self.assertRegex(done.stdout, r"\n +before: +model none, final none\n +after: +model tmdb:5, final tmdb:5\n")

    def test_when_no_commit_moves_it_the_corpus_did(self) -> None:
        for commit in ("c0", "c1", "c2", "c3"):
            self.side(commit, AT, "none", "none")
        self.side("c3", SINCE, "tmdb:5", "tmdb:5")
        done = self.bisect("c0", "c3", AT, "--since", SINCE)
        self.assertEqual(0, done.code, done.output)
        self.assertIn("CORPUS: the corpus moved it between", done.stdout)


class MainPathTest(WhatIfCase):
    def test_an_inert_change_resolves_the_base_once_and_starts_no_head(self) -> None:
        done = self.what_if("c0", "c1", "2026-11-06T10:00:00Z")
        self.assertEqual("sbt-start base|sbt-end base|java-start base|java-end base", self.calls(), done.output)
        self.assertIn("CLEAN", done.stdout)
        self.assertIn("0 take(s) moved", done.stdout)
        # ...and its verdict is PUBLISHED: one asset of the run, holding the recorded line that names it
        published = os.listdir(self.path("release"))
        line = self.verdict_lines("c1", "c0")[-1]
        self.assertEqual([line.split("\t")[6]], published)
        self.assertIn(line, json.load(open(self.path("release", published[0])))["lines"])

    def test_a_head_that_resolves_compiles_beside_the_base_and_resolves_every_family(self) -> None:
        done = self.what_if("c0", "c4", "2026-11-06T11:00:00Z", "--no-serve", env={"STUB_AWAIT_COMPILE": self.c["c4"]})
        calls = self.calls()
        self.assertLess(calls.find("sbt-start head"), calls.find("java-end base"), calls + done.output)
        self.assertGreaterEqual(calls.find("sbt-start head"), 0, calls + done.output)
        self.assertEqual(["java-start head", "java-end head"], [c for c in calls.split("|") if c.startswith("java-")
                                                                 and c.endswith(" head")])
        # THE STAMP CARRY reads the verdict's moved takes and mode: whole by default, the takes it moved counted
        self.assertEqual(1, done.stderr.count("selected: ALL (--all)"), done.output)
        fields = self.verdict_lines("c4", "c0")[-1].split("\t")
        self.assertEqual(["done", "0", "whole,final"], [fields[1], fields[4], fields[5]])

    def test_selection_keeps_the_narrow_resolve(self) -> None:
        done = self.what_if("c0", "c4", "2026-11-06T11:30:00Z", "--no-serve", "--selection")
        self.assertEqual(0, done.stderr.count("selected: ALL (--all)"), done.output)
        self.assertEqual("whole", self.verdict_lines("c4", "c0")[-1].split("\t")[5].split(",")[0])

    def test_a_pure_move_resolves_nothing_and_records_a_pure_move_verdict(self) -> None:
        done = self.what_if("c4", "c5", "2026-11-06T12:00:00Z")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["PURE MOVE: what-if skipped (scripts/pure-move-check)"],
                         [line for line in done.stdout.splitlines() if "PURE MOVE" in line])
        self.assertEqual("", self.calls())
        self.assertEqual([["uk", "pure-move"]], [line.split("\t")[:2] for line in self.verdict_lines("c5", "c4")])

    def test_a_range_that_moves_nothing_says_so_and_resolves(self) -> None:
        done = self.what_if("c0", "c1", "2026-11-06T10:00:00Z")
        self.assertIn("not a pure move (scripts/pure-move-check exit 1", done.stderr)
        self.assertIn("CLEAN", done.stdout)

    def test_every_side_is_asked_with_the_mirror_generation_it_must_read(self) -> None:
        done = self.what_if("c0", "c4", "2026-11-06T16:00:00Z")
        self.assertIn(f"--mirror-generation {GENERATION}", self.read("calls.args"), done.output)

    def test_head_equal_to_base_taking_alike_is_clean(self) -> None:
        done = self.what_if("c0", "c0", "2026-11-06T19:00:00Z", "--all", "--no-serve")
        self.assertEqual(0, done.code, done.output)
        self.assertIn("CLEAN", done.stdout)

    def test_head_equal_to_base_taking_otherwise_is_a_harness_error(self) -> None:
        done = self.what_if("c0", "c0", "2026-11-06T20:00:00Z", "--all", "--no-serve", env={"STUB_HEAD_TAKE": "tmdb:6"})
        self.assertEqual(2, done.code, done.output)
        self.assertRegex(done.stderr, "HARNESS ERROR.*same code.*moved")
        self.assertFalse(os.path.exists(self.side_dir(self.c["c0"], "2026-11-06T20:00:00Z")))

    def test_a_ref_laid_out_before_is_not_laid_out_again(self) -> None:
        self.what_if("c0", "c4", "2026-11-06T20:20:00Z", "--no-serve")
        done = self.what_if("c0", "c4", "2026-11-06T20:30:00Z", "--no-serve")
        self.assertNotRegex(done.stderr, "laying out [0-9a-f]{40} ")

    def test_a_head_whose_tree_cannot_be_laid_out_names_the_step(self) -> None:
        self.what_if("c0", "c1", "2026-11-06T10:00:00Z")
        os.chmod(self.path("cache/refs"), 0o555)
        try:
            done = self.what_if("c0", "c6", "2026-11-06T21:00:00Z", "--no-serve")
        finally:
            os.chmod(self.path("cache/refs"), 0o755)
        self.assert_harness_error(f"HARNESS ERROR.*laying out the head {self.c['c6'][:12]}.* failed \\(exit [0-9]*\\)",
                                  done)

    def test_a_bisect_step_whose_tree_cannot_be_laid_out_names_the_commit(self) -> None:
        self.what_if("c0", "c1", "2026-11-06T10:00:00Z")
        self.side("c0", AT, "none", "none")
        os.chmod(self.path("cache/refs"), 0o555)
        try:
            done = self.bisect("c0", "c6", AT)
        finally:
            os.chmod(self.path("cache/refs"), 0o755)
        self.assert_harness_error(f"HARNESS ERROR.*laying out {self.c['c6'][:12]} for its whole side failed "
                                  "\\(exit [0-9]*\\)", done)


if __name__ == "__main__":
    unittest.main()
