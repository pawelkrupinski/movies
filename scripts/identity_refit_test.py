#!/usr/bin/env python3
"""scripts/identity-refit, black-box: the refit's steps in order -- the spec, the measure on the classpath sbt exported,
and the checks only when the weights changed -- locally through sbt-warm and it-local.sh, in CI through plain sbt and
the workflow's Mongo; a shard measuring round 1's share alone and a run merging the shards; a dirty tree and the root
checkout refused; the usage. sbt, java, lscpu and sysctl are fakes and scripts/sbt-warm and scripts/it-local.sh
stand-ins, in a scratch repository holding a copy of the scripts and a worktree of it to run in. Ported from
identity-refit-test.sh case for case, with a failed measure besides; the same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_refit.py", "scripts/identity-refit")
WEIGHTS = "identity/src/main/resources/identity-weights.json"
SCHEDULES = "test/resources/fixtures/08-06-2026/expected-schedules.txt"
CLASSPATH = "/cp/a.jar:/cp/test-classes"
UNSET = ("CI", "KEEP", "SPEC_FAILS", "MEASURE_FAILS", "TMDB_API_KEY", "MONGODB_URI", "GITHUB_STEP_SUMMARY",
         "IDENTITY_REFIT_HEAP")
# sbt and sbt-warm: an `export` prints a classpath (in colour, as sbt does); SPEC_FAILS fails the refit's spec; the
# snapshot spec writes a moved film
SBT = r'''for a in "$@"; do
  case "$a" in
    *IdentityRefitSpec*) [ -z "$SPEC_FAILS" ] || { echo "[error] IdentityRefitSpec failed"; exit 1; }; echo "[info] Tests: succeeded 16" ;;
    export*) printf "\033[0m/cp/a.jar:/cp/test-classes\033[0m\n" ;;
    *FilmScheduleEndToEndSpec*) echo "film B" > test/resources/fixtures/08-06-2026/expected-schedules.txt ;;
  esac
done'''
# java: writes the report, with KEEP changes the weights, with MEASURE_FAILS fails
JAVA = '''while [ $# -gt 0 ]; do [ "$1" = "--report" ] && { mkdir -p "$(dirname "$2")"; echo "## Identity rule refit" > "$2"; }; shift; done
[ -z "$MEASURE_FAILS" ] || exit 3
[ -z "$KEEP" ] || echo '{"version":"v1-refit-1"}' > identity/src/main/resources/identity-weights.json
echo "kept 0: "'''
SPEC_AND_CLASSPATH = ["sbt-warm", "worker/testOnly scripts.IdentityRefitSpec", "export worker/Test/fullClasspath"]
CHECKS = [
    ["sbt-warm", "identity/testOnly services.identity.*",
     "worker/testOnly services.identity.* scripts.IdentityCalibrateSpec scripts.IdentityRefitSpec"],
    ["sbt-warm", "--stop", "--root"],
    ["it-local.sh", "worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec"],
    ["sbt-warm", "e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec"],
    ["sbt-warm", "e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec"],
    ["sbt-warm", "web/PageTest/testOnly views.PageSnapshotSpec"],
    ["sbt-warm", "web/PageTest/testOnly views.PageSnapshotSpec"]]


class IdentityRefitTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.copy_scripts("repo")
        shutil.copy2(TOOL, self.path("repo", "scripts", os.path.basename(TOOL)))
        for name in ("sbt-warm", "it-local.sh"):
            stand_in = self.write(f"repo/scripts/{name}", f'#!/bin/sh\nexec {name}-stand-in "$@"\n')
            os.chmod(stand_in, 0o755)
        self.write(f"repo/{WEIGHTS}", '{"version":"v1"}\n')
        self.write(f"repo/{SCHEDULES}", "film A\n")
        self.write("repo/.gitignore", "target/\n")
        self.repo_dir = self.path("repo")
        self.git(self.repo_dir, "init", "-q", "-b", "main")
        self.commit_tree(self.repo_dir, "init")
        self.tree = self.path("tree")
        self.git(self.repo_dir, "worktree", "add", "-q", "-b", "refit", self.tree)
        for name in ("sbt", "sbt-warm-stand-in"):
            self.fakes.script(name, SBT)
        self.fakes.script("it-local.sh-stand-in", "")
        self.fakes.script("java", JAVA)
        # the processor as a Linux runner names it: lscpu answers, sysctl knows no machdep key (and fails)
        self.fakes.script("lscpu", 'echo "Model name:            Test CPU 9000"')
        self.fakes.script("sysctl", "exit 255")

    def refit(self, *args: str, cwd: Optional[str] = None, **env: str) -> cli_harness.Done:
        environment: Dict[str, Optional[str]] = {name: None for name in UNSET}
        environment.update(env)
        self.fakes.clear()
        return self.run_tool(*args, env=environment, cwd=cwd or self.tree,
                             tool=os.path.join(cwd or self.tree, "scripts", os.path.basename(TOOL)))

    def steps(self) -> List[List[str]]:
        """Each sbt, sbt-warm, it-local.sh and java call in order: its name (a stand-in's as the script's), then argv."""
        with open(self.fakes.log) as f:
            entries = [json.loads(line) for line in f if line.strip()]
        return [[e["name"].replace("-stand-in", ""), *e["argv"]] for e in entries
                if e["name"] not in ("lscpu", "sysctl")]

    def measure(self) -> List[str]:
        [java] = [step for step in self.steps() if step[0] == "java"]
        self.assertEqual("-Xmx12g", java[1])
        return java[java.index("-cp"):]

    def test_help_prints_the_usage_naming_every_flag(self) -> None:
        done = self.refit("--help")
        self.assertEqual(0, done.code, done.output)
        self.assertRegex(done.stdout, r"--skip-checks.*--relearn.*--threads.*--report")

    def test_a_bad_command_line_is_a_usage_error(self) -> None:
        self.assertEqual(2, self.refit("--bogus").code)
        done = self.refit("--report")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("--report needs a value", done.stderr)
        self.assertIn("see identity-refit --help", done.stderr)
        done = self.refit("--shard", "1/4")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual([], self.steps())

    def test_the_root_checkout_is_refused_before_any_sbt(self) -> None:
        done = self.refit(cwd=self.repo_dir)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("root checkout", done.stderr)
        self.assertEqual([], self.steps())

    def test_a_dirty_tree_is_refused(self) -> None:
        self.write("tree/stray.txt", "stray\n")
        done = self.refit()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("the tree is dirty", done.stderr)
        self.assertEqual([], self.steps())

    def test_nothing_kept_done_after_the_measure(self) -> None:
        done = self.refit("--threads", "3", TMDB_API_KEY="")
        self.assertEqual(0, done.code, done.output)
        spec, java = self.steps()
        # the spec and the classpath in ONE sbt-warm call, then the measure on that classpath, applied, on the threads
        self.assertEqual(SPEC_AND_CLASSPATH, spec)
        self.assertEqual("java", java[0])
        self.assertEqual(["-cp", CLASSPATH, "scripts.IdentityRefit", "--apply", "--report", "target/identity-refit/report.md",
                          "--threads", "3"], self.measure())
        # naming the processor, though sysctl fails (a Linux runner)
        self.assertIn("processors (Test CPU 9000)", done.output)
        self.assertRegex(done.output, r"no TMDB_API_KEY.*unmeasured")
        # the measure's log beside the report
        self.assertIn("kept 0", self.read("tree/target/identity-refit/refit.log"))

    def test_a_shard_measures_round_1s_share_and_nothing_else(self) -> None:
        done = self.refit("--shard", "1/4", "--shard-out", "shards/s1.json", "--threads", "3", TMDB_API_KEY="k")
        self.assertEqual(0, done.code, done.output)
        classpath, _java = self.steps()
        # the classpath alone, no spec: the merging run tests the refit
        self.assertEqual(["sbt-warm", "export worker/Test/fullClasspath"], classpath)
        # the shard measured into its file: no report, no weights written
        self.assertEqual(["-cp", CLASSPATH, "scripts.IdentityRefit", "--shard", "1/4", "--shard-out", "shards/s1.json",
                          "--threads", "3"], self.measure())

    def test_merged_round_1_from_the_shards_files_the_rest_as_ever(self) -> None:
        done = self.refit("--threads", "3", "--merge", "shards", TMDB_API_KEY="k")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(SPEC_AND_CLASSPATH, self.steps()[0])
        self.assertEqual(["-cp", CLASSPATH, "scripts.IdentityRefit", "--apply", "--report", "target/identity-refit/report.md",
                          "--threads", "3", "--merge", "shards"], self.measure())

    def test_a_red_refit_spec_stops_it_before_the_measure(self) -> None:
        done = self.refit(SPEC_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("IdentityRefitSpec failed", done.stderr)
        self.assertEqual([SPEC_AND_CLASSPATH], self.steps())

    def test_a_failed_measure_fails_it_before_any_check(self) -> None:
        done = self.refit(KEEP="1", MEASURE_FAILS="1")
        self.assertEqual(3, done.code, done.output)
        self.assertEqual(2, len(self.steps()))

    def test_kept_locally_the_checks_run_in_order(self) -> None:
        done = self.refit("--relearn", "year.delta", "--report", "target/r.md", KEEP="1", TMDB_API_KEY="k")
        self.assertEqual(0, done.code, done.output)
        steps = self.steps()
        self.assertEqual(["--relearn", "year.delta"], self.measure()[-2:])
        self.assertIn("target/r.md", self.measure())
        # identity specs, the hard clusters on a mongod of its own (the warm server stopped first), the snapshots twice
        self.assertEqual(CHECKS, steps[2:])
        self.assertIn("+film B\n", self.read("tree/target/r.md"))
        # the weights left changed in the worktree
        self.assertEqual(WEIGHTS, self.git(self.tree, "diff", "--name-only", "--", WEIGHTS))

    def test_kept_skip_checks_measures_only(self) -> None:
        done = self.refit("--skip-checks", KEEP="1")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(2, len(self.steps()))

    def test_kept_in_ci_plain_sbt_and_the_workflows_mongo(self) -> None:
        summary = self.path("summary.md")
        done = self.refit("--report", "identity-refit-report.md", cwd=self.repo_dir, CI="true", KEEP="1",
                          MONGODB_URI="mongodb://ci", GITHUB_STEP_SUMMARY=summary)
        self.assertEqual(0, done.code, done.output)
        steps = self.steps()
        self.assertEqual([], [step for step in steps if step[0] in ("sbt-warm", "it-local.sh")])
        sbt = [step for step in steps if step[0] == "sbt"]
        self.assertEqual(7, len(sbt))
        self.assertTrue(all(step[1] == "-batch" for step in sbt), sbt)
        self.assertIn(["sbt", "-batch", "worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec"],
                      sbt)
        with open(summary) as f:
            self.assertIn("Identity rule refit", f.read())


if __name__ == "__main__":
    unittest.main()
