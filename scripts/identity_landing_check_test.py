#!/usr/bin/env python3
# script-test-reads: scripts/identity-landing-check scripts/identity_landing_check*.py scripts/tool-conventions.sh scripts/ci/ci-row-scope.sh .github/actions/changed-paths/matches.sh .github/identity-drift-paths.txt scripts/lib/harness/cli_harness.py scripts/lib/tool_process.py
"""scripts/identity-landing-check, black-box, over a fake identity-measure, a fake identity-ci-run (each group's sbt log
from a fixture) and a fake identity-drift-ci: one verdict, LANDABLE only when the measure and every group pass, BLOCKED
naming the failing spec, its test and the hint for its usual cause, and no verdict when a group could not run. Ported
from identity-landing-check-test.sh case for case; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_landing_check.py", "scripts/identity-landing-check")
UNIT = ("testOnly services.identity.* services.titles.rules.* scripts.* tools.DocCommentPlacementSpec "
        "tools.HardClusterRatchetSpec")
# identity-ci-run: the group's sbt log is $LOGS/<group>.log (exit 1); $LOGS/<group>.unreachable its stderr (exit 2);
# else a passing log, exit 0 -- or 1 with $LOGS/<group>.tail-fails (the run's tail failed after every spec passed)
CI_RUN = r'''
import shutil
out = argv[argv.index("--out") + 1]
cmd = " ".join(argv[argv.index("--") + 1:])
logs = env["LOGS"]
group = ("unit" if cmd.startswith("testOnly") else "it" if cmd.startswith("worker/IntegrationTest")
         else "e2e-corpus" if "FilmSchedule" in cmd else "e2e-rest")
def at(suffix):
    return os.path.join(logs, group + suffix)
if os.path.exists(at(".unreachable")):
    err(open(at(".unreachable")).read(), end=""); exit(2)
os.makedirs(out, exist_ok=True)
if os.path.exists(at(".log")):
    shutil.copy(at(".log"), os.path.join(out, "sbt.log")); exit(1)
with open(os.path.join(out, "sbt.log"), "w") as f:
    f.write("[info] All tests passed.\n[success] Total time: 3 s\n")
exit(1 if os.path.exists(at(".tail-fails")) else 0)
'''
MEASURE = r'''
code = int(env.get("MEASURE") or 0)
if code == 0:
    out("LANDABLE: no right take lost or turned wrong, no new wrong take")
elif code == 1:
    out("NOT LANDABLE:\n  - pl model: 1 right-lost")
elif code == 3:
    err("identity-measure: --footprint-only: not measured in full")
else:
    err("identity-measure: could not fetch")
exit(code)
'''
# identity-drift-ci: DRIFT=1 a red row, 2 no verdict
DRIFT = r'''
code = int(env.get("DRIFT") or 0)
if code == 0:
    out("DRIFT CLEAN")
elif code == 1:
    out("DRIFT RED: takes-drift-2 (germany)")
else:
    err("identity-drift-ci: could not dispatch identity-takes.yml")
exit(code)
'''
UNIT_FAILURES = """[info] MatchedTakesLabelSpec:
[info] The model's matched takes
[info] - should take no film a judgement denies or contradicts, beyond those known-wrong in scope rows *** FAILED ***
[info]   uk Pavilions Teignmouth | Patrick Monahan -> tmdb:123 judged wrong (MatchedTakesLabelSpec.scala:41)
[info] IdentitySliceCasesSpec:
[info] IdentityRefitCaptureReplaySpec:
[info] - should take identity-judgements.tsv's right film for each listing of uk-relay-1 *** FAILED ***
[info]   java.lang.IllegalArgumentException: requirement failed: the slice uk-relay-1 lacks answers: GET https://api.themoviedb.org/3/search
[info] - should take identity-judgements.tsv's right film for each listing of uk-relay-2 *** FAILED ***
[info]   java.lang.IllegalArgumentException: requirement failed: the slice uk-relay-2 lacks answers: GET https://api.themoviedb.org/3/search
[info] - should put every Scaladoc directly on the member it documents *** FAILED ***
[info]   List("RelayStage.scala:101 is followed by another Scaladoc") was not empty (DocCommentPlacementSpec.scala:56)
[error] Failed tests:
[error] \tservices.identity.MatchedTakesLabelSpec
[error] \tservices.identity.IdentitySliceCasesSpec
[error] \tservices.identity.UnmatchedClustersRatchetSpec
[error] \ttools.DocCommentPlacementSpec
[error] (worker / Test / testOnly) sbt.TestsFailedException: Tests unsuccessful
[error] Total time: 950 s
"""
COMPILE_ERROR = ("[error] /w/worker/src/main/scala/X.scala:3:5: not found: value y\n"
                 "[error] (worker / Compile / compileIncremental) Compilation failed\n")


class IdentityLandingCheckTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        repo = self.repo_dir = self.repo("repo", "-b", "main")
        self.write("repo/worker/src/main/scala/Resolver.scala", "base\n")
        self.write("repo/test/resources/fixtures/identity-judgements.tsv", "country\n")
        self.git(repo, "add", "-A")
        self.git(repo, "commit", "-qm", "base")
        self.base = self.git(repo, "rev-parse", "HEAD")
        self.write("repo/worker/src/main/scala/Resolver.scala", "head\n")
        self.git(repo, "commit", "-qam", "a resolver change")
        self.code = self.git(repo, "rev-parse", "HEAD")
        self.write("repo/test/resources/fixtures/identity-judgements.tsv", "country\tjudged\n")
        self.git(repo, "commit", "-qam", "a judgement")
        self.judged = self.git(repo, "rev-parse", "HEAD")
        os.makedirs(self.path("logs"))
        self.ci_run = self.fakes.program("ci-run", CI_RUN)
        self.measure = self.fakes.program("measure", MEASURE)
        self.drift = self.fakes.program("drift", DRIFT)
        self.out = self.err = ""

    def landing(self, *args: str, **env: str) -> int:
        """The tool from the scratch repository: its exit; its stdout and stderr in self.out, self.err."""
        self.fakes.clear()
        done = self.run_tool(*args, cwd=self.repo_dir, env={
            "LOGS": self.path("logs"), "IDENTITY_LANDING_OUT": self.path("check"),
            "IDENTITY_LANDING_CI_RUN": self.ci_run, "IDENTITY_LANDING_MEASURE": self.measure,
            "IDENTITY_LANDING_DRIFT": self.drift, **env})
        self.out, self.err = done.stdout, done.stderr
        return done.code

    def changed(self, **env: str) -> int:
        return self.landing("--head", self.code, "--base", self.base, **env)

    def judgement(self, *args: str, **env: str) -> int:
        return self.landing("--head", self.judged, "--base", self.code, *args, **env)

    def commands(self) -> List[str]:
        """The sbt command of each identity-ci-run call."""
        return [" ".join(argv[argv.index("--") + 1:]) for argv in self.fakes.calls("ci-run")]

    def measured(self) -> str:
        calls = self.fakes.calls("measure")
        return " ".join(calls[0]) if calls else ""

    def drift_args(self) -> str:
        calls = self.fakes.calls("drift")
        return " ".join(calls[0]) if calls else ""

    def lines(self, prefix: str) -> int:
        return sum(1 for line in self.out.splitlines() if line.startswith(prefix))

    def test_every_group_passing_and_the_measure_landable_is_landable(self) -> None:
        self.assertEqual(0, self.changed(), self.out + self.err)
        self.assertEqual("LANDABLE", self.out.splitlines()[-1], "the verdict last")
        self.assertEqual(f"--head {self.code} --base {self.base} --footprint", self.measured(),
                         "the measure asked for the footprint when eligible (identity-measure's own fallback)")
        commands = self.commands()
        self.assertEqual(4, len(commands), "every group Main's rows run for a resolver change, e2e included")
        self.assertIn(UNIT, commands, "the unit group naming the identity specs Main runs")
        self.assertTrue(any("integration.HardClusterConvergenceIntegrationSpec" in c for c in commands),
                        "the hard-cluster convergence among them")

    def test_a_judgements_only_change_skips_the_e2e_rows(self) -> None:
        self.assertEqual((0, 2), (self.judgement(), len(self.commands())), self.out + self.err)

    def test_the_measure_flags(self) -> None:
        self.judgement("--full")
        self.assertEqual(f"--head {self.judged} --base {self.code} --full", self.measured(),
                         "--full asks the full measure, over the whole suite")
        self.judgement("--footprint-only")
        self.assertIn("--footprint-only", self.fakes.calls("measure")[0])
        self.judgement("--no-cache")
        self.assertIn("--no-cache", self.fakes.calls("measure")[0])
        self.judgement("--no-what-if", "live asks")
        self.assertIn("--no-what-if live asks", self.measured(), "--no-what-if reaches the measure with its reason")

    def test_failing_specs_are_blocked_each_named_with_its_hint(self) -> None:
        self.write("logs/unit.log", UNIT_FAILURES)
        self.assertEqual(1, self.judgement(), "the measure LANDABLE notwithstanding")
        self.assertIn("BLOCKED", self.out)
        self.assertIn("- MatchedTakesLabelSpec: should take no film a judgement denies or contradicts, beyond those "
                      "known-wrong in scope rows", self.out)
        self.assertIn("hint: judgement rows stale -> known-wrong scope rows", self.out)
        self.assertEqual((2, 0), (self.lines("  - IdentitySliceCasesSpec: should take"),
                                  self.out.count("IdentityRefitCaptureReplaySpec")),
                         "a failure under another suite's header (parallel output) is its own suite's")
        self.assertIn("hint: slice case asks unpinned questions -> re-pin from the recording", self.out)
        self.assertIn("- DocCommentPlacementSpec: should put every Scaladoc directly on the member it documents",
                      self.out, "a failure its location names, that suite's")
        self.assertIn("- UnmatchedClustersRatchetSpec: (the suite failed: see the log)", self.out,
                      "a suite the summary names without a test, too")

    def test_the_sbt_log_decides_not_the_runs_conclusion(self) -> None:
        self.write("logs/it.tail-fails", "")
        self.assertEqual(0, self.changed(), self.out + self.err)

    def test_a_commit_that_does_not_compile_is_blocked_saying_so(self) -> None:
        self.write("logs/it.log", COMPILE_ERROR)
        self.assertEqual(1, self.changed(), self.out + self.err)
        self.assertIn("- compile: /w/worker/src/main/scala/X.scala:3:5: not found: value y", self.out)

    def test_a_measure_not_landable_is_blocked_with_its_reasons(self) -> None:
        self.assertEqual(1, self.changed(MEASURE="1"))
        self.assertIn("- measure: NOT LANDABLE: pl model: 1 right-lost", self.out)

    def test_a_measure_the_footprint_could_not_answer_under_footprint_only_is_blocked(self) -> None:
        self.assertEqual(1, self.landing("--head", self.code, "--base", self.base, "--footprint-only", MEASURE="3"))

    def test_a_measure_that_could_not_be_made_is_no_verdict_saying_why(self) -> None:
        self.assertEqual(2, self.changed(MEASURE="2"))
        self.assertIn("NO VERDICT: could not check measure", self.out)
        self.assertIn("measure     could not measure: could not fetch", self.out)

    def test_a_group_that_could_not_run_is_no_verdict_saying_why(self) -> None:
        self.write("logs/e2e-corpus.unreachable", "identity-ci-run: could not push measure/u/abc-1: remote rejected "
                                                  "(unable to migrate objects to permanent storage)\n")
        self.assertEqual(2, self.changed(), self.out + self.err)
        self.assertIn("could not check e2e-corpus", self.out)
        self.assertIn("e2e-corpus  could not run: could not push measure/u/abc-1: remote rejected (unable to migrate "
                      "objects to permanent storage)", self.out)

    def test_no_measure_and_no_specs_together_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.landing("--no-measure", "--no-specs"))
        self.assertIn("identity-landing-check: --no-measure and --no-specs leave nothing to check", self.err)
        self.assertEqual(2, self.landing("--bogus"), "an unknown option")
        self.assertEqual(2, self.landing("--base"), "a flag without its value")
        self.assertEqual(0, self.landing("--help"))
        self.assertIn("WILL THIS LAND GREEN?", self.out)
        self.assertIn("Exit: 0 LANDABLE, 1 BLOCKED, 2 could not check", self.out)

    def test_a_resolver_change_runs_the_drift_group_held_against_mains(self) -> None:
        self.assertEqual(0, self.changed(), self.out + self.err)
        self.assertIn(f"--head {self.code} --against main --out ", self.drift_args())
        self.assertIn("  drift       DRIFT CLEAN", self.out, "its verdict a line of the check's")

    def test_a_red_drift_row_is_blocked_naming_it(self) -> None:
        self.assertEqual(1, self.changed(DRIFT="1"))
        self.assertIn("- drift: DRIFT RED: takes-drift-2 (germany)", self.out)

    def test_a_drift_run_that_could_not_be_made_is_no_verdict_saying_why(self) -> None:
        self.assertEqual(2, self.changed(DRIFT="2"))
        self.assertIn("drift       could not check: could not dispatch identity-takes.yml", self.out)

    def test_which_changes_run_the_drift_group(self) -> None:
        self.assertEqual((0, ""), (self.judgement(), self.drift_args()), "a judgements-only change touches no drift path")
        self.judgement("--drift")
        self.assertNotEqual("", self.drift_args(), "...unless --drift asks for it")
        self.landing("--head", self.code, "--base", self.base, "--full")
        self.assertEqual("", self.drift_args(), "--full runs the drift rows inside the whole suite, so not again")
        self.landing("--head", self.code, "--base", self.base, "--no-drift")
        self.assertEqual("", self.drift_args(), "--no-drift never")


if __name__ == "__main__":
    unittest.main()
