#!/usr/bin/env python3
"""scripts/identity-measure, black-box: what spares a dispatch. REUSE (a change rebased onto a main that moved only
outside the paths that move a take is answered by its earlier run), the take paths covering what the takes check
reads, THE FOOTPRINT (only the countries a change can move, over one recording) and its laptop measure of a
title-rule change, and THE RESULT CACHE on the release. Ported from identity-measure-test.sh case for case (its other
cases: identity_measure_cli_test.py); the same cases hold the shell tool and its Python rewrite."""
from __future__ import annotations

import hashlib
import os
import re
import subprocess
import sys
import unittest
from typing import List

from identity_measure_cli_fixture import MeasureCase, cli_harness

ROOT = cli_harness.ROOT
PL = ("--countries", "pl")
EVERY = "footprint pl,de,uk,us,es"


def takes_paths() -> List[str]:
    """The takes check's identity paths, as it lists them."""
    done = subprocess.run([sys.executable, "-I", os.path.join(ROOT, "scripts/ci/identity_takes_check_paths.py")],
                          check=True, capture_output=True, text=True)
    return done.stdout.split()


class IdentityMeasureReuseTest(MeasureCase):

    def on_main(self, path: str, content: str) -> None:
        """A commit on origin's main changing `path`."""
        self.git(self.checkout, "checkout", "-q", "main")
        self.write(f"repo/{path}", content + "\n")
        self.git(self.checkout, "add", path)
        self.git(self.checkout, "commit", "-qm", f"main: {path}")
        self.git(self.checkout, "push", "-q", "origin", "main")

    def rebased_dispatches(self, rule: str, main_path: str = "", main_content: str = "x") -> bool:
        """The one-commit feature branch (re)made on main's tip -- after a commit of `main_path` on main, when named
        -- and measured again over the cache: whether that dispatched."""
        if main_path:
            self.on_main(main_path, main_content)
        self.git(self.checkout, "checkout", "-q", "-B", "feature", "main")
        self.write("repo/rule.txt", rule + "\n")
        self.git(self.checkout, "commit", "-qam", "feature: a resolver change")
        self.assertIn(self.measure("landable", *PL, keep_cache=True), (0, 1), self.out)
        return self.dispatched()

    def test_a_rebased_change_reuses_its_run_while_main_moved_no_take(self) -> None:
        self.assertTrue(self.rebased_dispatches("rule v2"), "a first measurement of a change dispatches")
        self.assertFalse(self.rebased_dispatches("rule v2", "docs.txt", "a doc"), self.out)
        self.assertEqual([], self.measure_branches())
        self.assertEqual(1, self.lines(r"REUSED measurement of run 777 \(base [0-9a-f]*, same change, main moved only "
                                       r"outside the paths that move a take\)"), self.out)
        self.assertEqual(1, self.lines("^LANDABLE"), self.out)
        self.assertEqual(0, self.measure("landable", "--fresh", *PL, keep_cache=True), self.out)
        self.assertTrue(self.dispatched(), "--fresh dispatches all the same")
        # main judges Lalka wrong since: the judgements move no take (collecting classifies with the head's)
        self.judge_lalka_wrong()
        self.git(self.checkout, "checkout", "-q", "main")
        self.git(self.checkout, "commit", "-qam", "main: Lalka judged wrong")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.assertFalse(self.rebased_dispatches("rule v2"), "main moved only its judgements")
        self.assertFalse(self.rebased_dispatches("rule v2", ".github/scripts/convergence-setup.sh", "a leg's plumbing"),
                         "main moved only a leg's CI plumbing (a convergence path that moves no take)")
        self.assertTrue(self.rebased_dispatches("rule v2", "engine/match.txt"), "main moved inside the paths")
        self.assertTrue(self.rebased_dispatches("rule v3"), "the change itself changed")
        # the patch-id ignores whitespace: the same id, but not the same head
        self.assertTrue(self.rebased_dispatches("rule  v3"), "a change differing only in whitespace")
        # what the takes check reads that no convergence path names still moves a take
        self.assertTrue(self.rebased_dispatches("rule  v3", "test/resources/fixtures/identity-unmatched/pl.inputs"))
        self.assertTrue(self.rebased_dispatches("rule  v3", "worker/src/it/scala/IdentityResolveDump.scala"))
        self.assertFalse(self.rebased_dispatches(
            "rule  v3", "test/resources/fixtures/identity-unmatched/expected-matches.tsv"), "the ratchet's output")

    def test_a_run_collected_with_run_answers_its_change_for_a_day(self) -> None:
        # its base from the artifact, its head the run's commit
        self.push_run_branch()
        self.measure("lost", "--run", "777", *PL, env={"HEAD_SHA": self.tip})
        self.assertEqual(1, self.measure("landable", self.tip, "--base", self.parent, *PL, keep_cache=True), self.out)
        self.assertFalse(self.dispatched())
        self.assertEqual(1, self.lines(r"REUSED measurement of run 777 \(base [0-9a-f]*, same change, the same base\)"),
                         self.out)
        # ...for 24 hours from when the run was made, not from when it was collected
        self.push_run_branch()
        self.measure("lost", "--run", "777", *PL, env={"HEAD_SHA": self.tip, "RUN_CREATED": "2026-01-01T00:00:00Z"})
        self.measure("landable", self.tip, "--base", self.parent, *PL, keep_cache=True)
        self.assertTrue(self.dispatched(), "a run made days before it was collected is not reused")

    def test_every_path_the_takes_check_reads_moves_a_take(self) -> None:
        patterns = self.write("take-paths", self.run_tool("--take-paths", cwd=ROOT).stdout)
        checks = ""
        for name in sorted(os.listdir(os.path.join(ROOT, "scripts/ci"))):
            if name.startswith("identity_takes_check") and name.endswith(".py"):
                with open(os.path.join(ROOT, "scripts/ci", name)) as f:
                    checks += f.read()
        specs = sorted({m.split(".")[-1] for m in re.findall(r"testOnly ([a-z.]*[A-Z][A-Za-z]*)", checks)})
        read = takes_paths() + [p for spec in specs for p in self.sources(spec)]
        read.append("test/resources/fixtures/identity-unmatched/pl.json.gz")
        uncovered = [p for p in read if not self.moves_a_take(p, patterns)]
        self.assertEqual([], uncovered)
        self.assertIn("identity/src/main", read)
        self.assertTrue(any("UnmatchedClustersRatchetSpec" in p for p in read), read)

    @staticmethod
    def sources(spec: str) -> List[str]:
        """Where `spec`'s source lies, under any module's src/."""
        found: List[str] = []
        for module in sorted(os.listdir(ROOT)):
            for here, _, files in os.walk(os.path.join(ROOT, module, "src")):
                found += [os.path.relpath(os.path.join(here, f), ROOT) for f in files if f == f"{spec}.scala"]
        return found

    @staticmethod
    def moves_a_take(path: str, patterns: str) -> bool:
        probe = path + "/x.scala" if os.path.isdir(os.path.join(ROOT, path)) else path
        done = subprocess.run([os.path.join(ROOT, ".github/actions/changed-paths/matches.sh"), patterns],
                              input=probe + "\n", capture_output=True, text=True)
        return done.stdout.strip() == "true"

    def change_on_main(self, path: str, content: str) -> str:
        """A commit on top of main changing only `path`."""
        self.git(self.checkout, "checkout", "-q", "--detach", "main")
        self.write(f"repo/{path}", content)
        return self.commit(f"footprint: {path}")

    def plan(self, head: str, *flags: str) -> List[str]:
        done = self.run_tool("--plan", "--head", head, "--base", self.git(self.checkout, "rev-parse", "main"), *flags,
                             cwd=self.checkout, env={"KINOWO_IDENTITY_CACHE": self.path("cache"),
                                                     "STUB": self.path("stub"),
                                                     "IDENTITY_MEASURE_RECORDINGS": "528:pl,de 999"})
        return done.stdout.rstrip("\n").split("\n")

    def test_the_footprint_and_the_recordings(self) -> None:
        os.makedirs(self.path("stub"))
        de_only = self.change_on_main("engine/services/cinemas/de/Kinoheld.scala", "x")
        self.assertEqual("footprint de", self.plan(de_only)[0])
        self.assertEqual(["measure pinned de", "then, when LANDABLE and 2+ countries moved: 528 pl,de;999 pl,de,uk,us,es"],
                         self.plan(de_only)[-2:])
        self.assertEqual(["measure pinned de", "measure 528 de", "measure 999 de"],
                         self.plan(de_only, "--all-recordings")[-3:])
        self.assertEqual(EVERY, self.plan(self.change_on_main("engine/core.scala", "x"))[0])
        uk_capture = self.change_on_main("test/resources/fixtures/identity-unmatched/uk.inputs", "x")
        self.assertEqual("footprint uk", self.plan(uk_capture)[0])
        # a title-words row reaches every country (TitleWords.tokens reads every language's words in every country)
        words = self.change_on_main("common/src/main/resources/title-words.tsv",
                                    "# header\nanniversary\ten\tanniversary\nanniversary\tes\taniversario\n")
        self.assertEqual(EVERY, self.plan(words)[0])
        rules = self.change_on_main("common/src/main/resources/title-rules.tsv",
                                    "# header\nde-strip\tquery\t\tde\t\tjunk\t\tx\t\t\t[]\n")
        self.assertEqual("footprint de", self.plan(rules)[0], "a title-rules row scoped to a country")
        self.assertEqual("footprint pl", self.plan(de_only, *PL)[0])
        self.assertEqual(EVERY, self.plan(de_only, "--all-countries")[0])
        self.assertEqual(EVERY, self.plan(self.change_on_main("docs/notes.md", "x"))[0], "no take-moving path")

    def test_the_footprint_measure_of_a_title_rule_change(self) -> None:
        self.write("repo/common/src/main/resources/title-rules.tsv", "rule a\n")
        base = self.commit("rules before")
        self.write("repo/common/src/main/resources/title-rules.tsv", "rule b\n")
        head = self.commit("a title-rule change")
        code = hashlib.sha256(self.raw_git("ls-tree", "-r", base, "--", *takes_paths())).hexdigest()[:24]
        trace_check = self.fakes.script("trace-check", '''
echo "[normalizer-trace] 12 reading(s) made again: ${TRACE_DIFFER:-0} read otherwise, 0 could not be made again"
[ "${TRACE_DIFFER:-0}" = 0 ] && exit 0
printf 'pl\\tsanitize\\tFestive Fun with Peppa: Cinema Experience\\tfestivefunwithpeppacinemaexperience\\tfestivefunwithpeppa\\n' > "$2"; exit 1''')
        traced = {"FOOTPRINT_ASSETS": f"takes-trace-pl-{code}-p573.gz 99", "IDENTITY_MEASURE_TRACE_CHECK": trace_check}

        def footprint(**env: str) -> int:
            return self.measure("landable", "--head", head, "--base", base, *PL, "--footprint", env={**traced, **env})
        self.assertEqual(0, footprint(), self.out)
        self.assertEqual(1, self.lines("^measure: footprint"), self.out)
        self.assertIn(f"footprint: pl's base readings: takes-trace-pl-{code}-p573.gz", self.out)
        self.assertEqual(1, self.lines("^LANDABLE \\(footprint\\): no take can move -- before landing, the full measure"))
        self.assertFalse(self.dispatched())
        # a title read otherwise is listed, and the full measure decides
        self.assertEqual(0, footprint(TRACE_DIFFER="1"), self.out)
        self.assertIn("Festive Fun with Peppa: Cinema Experience", self.out)
        self.assertEqual((1, True), (self.lines("^measure: full"), self.dispatched()))
        # no trace of the base's identity code over the recording measures in full
        footprint(FOOTPRINT_ASSETS="takes-trace-pl-000000000000000000000000-p573.gz 99")
        self.assertEqual((True, 1), ("no trace of pl at the base" in self.out, self.lines("^measure: full")))
        # a change outside the title rules says so and measures in full
        self.measure("landable", "--head", self.tip, *PL, "--footprint", env=traced)
        self.assertIn("footprint: rule.txt can move a take outside the title rules", self.out)
        self.assertEqual(1, self.lines("^measure: full"), self.out)

    def test_the_result_cache(self) -> None:
        os.makedirs(self.path("release"))
        cache = {"RELEASE_DIR": self.path("release")}
        lost = ("--base", self.parent, *PL)
        first = self.measure("lost", *lost, env=cache)
        verdict = [line for line in self.out.split("\n") if re.match("(LANDABLE|NOT LANDABLE)", line)]
        stored = [n for n in os.listdir(self.path("release")) if n.startswith("measure-result-") and n.endswith(".json")]
        self.assertEqual(1, len(stored), os.listdir(self.path("release")))
        # the same request again answers from it: the same verdict, no dispatch, saying so with the stored table
        self.assertEqual((first, False), (self.measure("lost", *lost, env=cache), self.dispatched()))
        self.assertIn("CACHED verdict of run 777", self.out)
        self.assertTrue(verdict and all(line in self.out.split("\n") for line in verdict), (verdict, self.out))
        self.assertEqual((first, True), (self.measure("lost", *lost, "--no-cache", env=cache), self.dispatched()))
        self.measure("lost", "--base", self.parent, "--countries", "pl,de", env=cache)
        self.assertTrue(self.dispatched(), "another set of countries is another key")
        self.measure("lost", *lost, env={**cache, "IDENTITY_MEASURE_CACHE_HOURS": "-1"})
        self.assertTrue(self.dispatched(), "a pinned-pair verdict past IDENTITY_MEASURE_CACHE_HOURS")
        self.measure("lost", *lost, "--json", env=cache)
        self.assertTrue(self.dispatched(), "--json neither reads nor stores it")


if __name__ == "__main__":
    unittest.main()
