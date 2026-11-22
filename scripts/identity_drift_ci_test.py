#!/usr/bin/env python3
"""scripts/identity-drift-ci, black-box, against a scratch repository and a fake gh: --for-red reads a red run's failed
takes-drift rows and dispatches only their countries and seeds; the verdict is the requested rows' own (a red row is
DRIFT RED, exit 1; every row green DRIFT CLEAN, exit 0; a row that never ran NO VERDICT, exit 2); a partial fix is DRIFT
PROGRESS, one row worse refutes it; --against main holds every row to main's newest decided run (DRIFT NOT WORSE);
--profile, --countries, --seeds and --lane become the dispatch's inputs; github.com's nested job names are read; a red
that failed no drift row has nothing to judge (exit 0, no dispatch). Ported from identity-drift-ci-test.sh, case for
case and in its order (each step builds on the state the one before left). The same cases hold the shell tool and its
Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import shutil
import sys
import unittest
from typing import Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_drift_ci.py", "scripts/identity-drift-ci")
RECORDED_JOBS = os.path.join(cli_harness.ROOT, "scripts/ci/testdata/github-drift-run-41250027249-jobs.json")
# gh answering from $FAKE_STATE: runs.json (run list), jobs-<id>.json (run view --json jobs), status-<id> / number-<id>
# (run view --json status / number), --jq applied through jq as gh does; `workflow run` notes its arguments and lists
# the dispatched run as 900 (on the commit FAKE_TIP names); `run download` lays each row's artifact
GH = r'''
import shutil, subprocess
state = env["FAKE_STATE"]
joined = " " + " ".join(argv) + " "
jq = argv[argv.index("--jq") + 1] if "--jq" in argv else None
def answer(text):
    if jq:
        done = subprocess.run(["jq", "-r", jq], input=text, capture_output=True, text=True)
        sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
    sys.stdout.write(text); exit(0)
def read(name):
    with open(os.path.join(state, name)) as f:
        return f.read().strip()
verb = " ".join(argv[:2])
if verb == "workflow run":
    with open(os.path.join(state, "dispatches"), "a") as f:
        f.write(" ".join(argv) + "\n")
    with open(os.path.join(state, "runs-dispatched.json"), "w") as f:
        f.write(json.dumps([{"databaseId": 900, "number": 600, "headSha": env["FAKE_TIP"], "createdAt": "2999-01-01T00:00:00Z"}]))
    exit(0)
if verb == "run list":
    answer(read("runs-dispatched.json" if " --event " in joined else "runs.json"))
if verb == "run view":
    run = argv[2]
    if " jobs" in joined:
        answer(read(f"jobs-{run}.json"))
    if " number" in joined:
        answer(json.dumps({"number": int(read(f"number-{run}"))}))
    answer(json.dumps({"status": read(f"status-{run}")}))
if verb == "run download":
    target = argv[argv.index("--dir") + 1]
    source = os.path.join(state, f"artifacts-{argv[2]}")
    for name in sorted(os.listdir(source)) if os.path.isdir(source) else []:
        shutil.copytree(os.path.join(source, name), os.path.join(target, name), dirs_exist_ok=True)
    exit(0)
exit(1)
'''


class IdentityDriftCiTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.origin = self.repo("origin.git", "--bare", "-b", "main")
        self.checkout = self.repo("repo", "-b", "main")
        self.git(self.checkout, "remote", "add", "origin", self.origin)
        self.write("repo/.github/workflows/country-convergence-leg.yml", "drift-only: true\n")
        self.write("repo/.github/workflows/identity-takes.yml", "name: Identity takes\n")
        self.write("repo/scripts/ci/.keep", "")
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", "main")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.tip = self.git(self.checkout, "rev-parse", "HEAD")
        self.dispatched_tip = self.tip  # the commit the fake's dispatched run names, whatever is pushed later
        self.state = self.path("state")
        os.makedirs(self.state)
        self.gh = self.fakes.program("gh", GH)

    # ── the fixture ─────────────────────────────────────────────────────────────────────────

    def drift(self, name: str, *flags: str) -> int:
        """One run of the tool from the checkout: its exit; stdout in self.stdout[name], stderr in self.stderr[name]."""
        done = self.run_tool("--head", self.tip, "--out", self.path(f"out-{name}"), *flags, cwd=self.checkout, env={
            "KINOWO_GH": self.gh, "KINOWO_IDENTITY_CACHE": self.path("cache"), "IDENTITY_MEASURE_REMOTE": "origin",
            "HOME": self.work, "IDENTITY_DRIFT_FIRST_WAIT": "0", "DISPATCH_FIND_SECONDS": "5", "FAKE_STATE": self.state,
            "FAKE_TIP": self.dispatched_tip})
        self.out, self.err = done.stdout, done.stderr
        return done.code

    def last(self) -> str:
        return self.out.rstrip("\n").split("\n")[-1]

    def jobs(self, run: int, *rows: str) -> None:
        jobs = [dict(zip(("name", "status", "conclusion"), row.rsplit(":", 2))) for row in rows]
        self.write(f"state/jobs-{run}.json", json.dumps({"jobs": jobs}))

    def artifact(self, run: int, cc: str, row: int, divergences: int, flips: int, seed: Optional[int] = None) -> None:
        """The row's artifact as identity-drift-check leaves it -- a `seed` file since the rotating seeds; none, a run
        from before them (row N replayed seed N)."""
        directory = f"state/artifacts-{run}/identity-drift-{cc}-takes-drift-{row}"
        shutil.rmtree(self.path(directory), ignore_errors=True)
        if seed is not None:
            self.write(f"{directory}/seed", f"{seed}\n")
            self.write(f"{directory}/reproduce.txt", f"IdentityDrift {cc} --seed {seed}\n")
        self.write(f"{directory}/verdict.txt", f"{divergences} divergence(s) from a whole resolve and {flips} flip(s) on "
                                               "the way, beyond the allow-lists\n")
        self.write(f"{directory}/report.txt", f"## {cc}: 1 divergence(s), 0 allow-listed\n      Cinema|Film  a -> b\n")

    def runs(self, *runs: str) -> None:
        self.write("state/runs.json", "[" + ", ".join(runs) + "]")

    def dispatches(self) -> str:
        return self.read("state/dispatches") if os.path.exists(self.path("state/dispatches")) else ""

    def no_dispatches(self) -> None:
        if os.path.exists(self.path("state/dispatches")):
            os.remove(self.path("state/dispatches"))

    def dispatched(self, ending: str) -> bool:
        return any(line.endswith(ending) for line in self.dispatches().splitlines())

    # ── the cases, in the shell test's order ────────────────────────────────────────────────

    def test_for_red_against_main_and_the_dispatchs_inputs(self) -> None:
        self.runs('{"databaseId": 3726, "number": 2517, "status": "completed"}',
                  '{"databaseId": 3700, "number": 2500, "status": "completed"}')
        self.jobs(3726, "takes-drift-2 (germany):completed:failure", "takes-drift-1 (united-kingdom):completed:success",
                  "leg (germany):completed:failure")
        self.jobs(900, "takes-drift-2 (germany):completed:failure")
        self.write("state/status-900", "completed\n")
        self.artifact(3726, "de", 2, 0, 1, 37)
        self.artifact(900, "de", 2, 0, 1, 37)
        self.assertEqual(1, self.drift("red", "--for-red", "2517"), self.err)
        self.assertTrue(self.dispatched("-f drift-only=true -f takes-countries=de -f drift-seeds=,37"), self.dispatches())
        self.assertEqual("DRIFT RED: no better than run 2517: takes-drift-2 (germany): 1 -> 1 open", self.last())
        self.assertIn("\ntakes-drift-2 (germany) [seed 37]: failure -- 0 divergence(s) from a whole resolve and 1 flip(s)",
                      "\n" + self.out)
        self.assertIn("Cinema|Film", self.out)
        self.assertIn("\n    reproduce: IdentityDrift de --seed 37\n", self.out)
        self.assertEqual(1, len(self.git(self.origin, "for-each-ref", "refs/heads/measure").splitlines()))

        self.no_dispatches()
        self.jobs(900, "takes-drift-2 (germany):completed:success")
        self.assertEqual((0, "DRIFT CLEAN"), (self.drift("green", "--for-red", "3726"), self.last()))
        self.extra_row_and_partial_fixes()
        self.against_main()
        self.github_nested_names_and_nothing_to_judge()
        self.countries_seeds_and_skipped_rows()
        self.the_lane()

    def extra_row_and_partial_fixes(self) -> None:
        # AN EXTRA ROW (Germany's seed 581 on a takes-drift-3 of its own): no pin reaches it -- every row on its fixed seeds
        self.no_dispatches()
        self.jobs(3726, "takes-drift-3 (germany):completed:failure")
        self.artifact(3726, "de", 3, 0, 1, 581)
        self.jobs(900, "takes-drift-1 (germany):completed:success", "takes-drift-2 (germany):completed:success",
                  "takes-drift-3 (germany):completed:failure")
        self.artifact(900, "de", 3, 0, 1, 581)
        self.assertEqual(1, self.drift("extra", "--for-red", "3726"))
        self.assertTrue(self.dispatched("-f drift-only=true -f takes-countries=de"), self.dispatches())
        self.assertIn("takes-drift-3 (germany)", self.last())
        # A PARTIAL FIX (5df3295bd): fewer in one row, more in none, is progress; one row worse refutes it
        self.jobs(3726, "takes-drift-2 (germany):completed:failure", "takes-drift-1 (united-kingdom):completed:failure")
        self.artifact(3726, "de", 2, 0, 2)
        self.artifact(3726, "uk", 1, 1, 3)
        self.jobs(900, "takes-drift-2 (germany):completed:failure", "takes-drift-1 (united-kingdom):completed:failure",
                  "takes-drift-1 (germany):completed:success", "takes-drift-2 (united-kingdom):completed:success")
        self.artifact(900, "de", 2, 0, 2)
        self.artifact(900, "uk", 1, 0, 1)
        self.assertEqual(0, self.drift("partialfix", "--for-red", "3726"))
        self.assertEqual("DRIFT PROGRESS: takes-drift-1 (united-kingdom): 4 -> 1 open -- still red: takes-drift-2 (germany) "
                         "takes-drift-1 (united-kingdom)", self.last())
        self.artifact(900, "de", 2, 1, 2)
        self.assertEqual(1, self.drift("worse", "--for-red", "3726"))
        shutil.rmtree(self.path("state/artifacts-3726"))
        self.no_dispatches()
        self.write("state/number-3726", "2517\n")
        self.assertEqual(2, self.drift("nobase", "--for-red", "3726"))
        self.assertNotIn("drift-seeds", self.dispatches())

    def against_main(self) -> None:
        self.jobs(3726, "takes-drift-1 (germany):completed:success", "takes-drift-2 (germany):completed:failure",
                  "takes-drift-1 (united-kingdom):completed:success", "takes-drift-2 (united-kingdom):completed:success")
        self.artifact(3726, "de", 2, 0, 2)
        self.jobs(900, "takes-drift-1 (germany):completed:success", "takes-drift-2 (germany):completed:failure",
                  "takes-drift-1 (united-kingdom):completed:failure", "takes-drift-2 (united-kingdom):completed:success")
        self.artifact(900, "de", 2, 0, 2)
        self.artifact(900, "uk", 1, 0, 3)
        self.no_dispatches()
        self.assertEqual(1, self.drift("born", "--against", "main", "--countries", "de,uk"))
        self.assertTrue(self.dispatched("-f drift-seeds=1,2"), self.dispatches())
        self.assertEqual("DRIFT RED: worse than run 3726: takes-drift-1 (united-kingdom): 0 -> 3 open", self.last())
        self.jobs(900, "takes-drift-1 (germany):completed:success", "takes-drift-2 (germany):completed:failure",
                  "takes-drift-1 (united-kingdom):completed:success", "takes-drift-2 (united-kingdom):completed:success")
        self.assertEqual(0, self.drift("notworse", "--against", "main", "--countries", "de,uk"))
        self.assertEqual("DRIFT NOT WORSE than run 3726: takes-drift-2 (germany): 2 -> 2 open", self.last())
        self.no_dispatches()
        self.drift("profiled", "--profile", "--seeds", "1,2", "--countries", "de,uk")
        self.assertTrue(self.dispatched("-f drift-seeds=1,2 -f profile=true"), self.dispatches())
        self.jobs(900, "takes-drift-2 (germany):completed:failure")
        self.assertEqual(2, self.drift("short", "--against", "main", "--countries", "de,uk"))
        self.assertEqual(2, self.drift("both", "--for-red", "3726", "--against", "main"))

    def github_nested_names_and_nothing_to_judge(self) -> None:
        shutil.copy(RECORDED_JOBS, self.path("state/jobs-41250027249.json"))
        self.write("state/status-41250027249", "completed\n")
        self.write("state/number-41250027249", "2600\n")
        self.runs('{"databaseId": 41250027249, "number": 2600, "status": "completed"}')
        self.assertEqual(1, self.drift("ghrun", "--run", "41250027249"))
        self.assertEqual("DRIFT RED: takes-drift-1 (germany) takes-drift-2 (united-kingdom)", self.last())
        self.no_dispatches()
        self.drift("ghred", "--for-red", "41250027249")
        self.assertIn("red drift rows: takes-drift-1 (germany),takes-drift-2 (united-kingdom) -> countries de,uk", self.err)
        self.no_dispatches()
        self.jobs(3700, "convergence (poland):completed:failure", "takes-drift-1 (poland):completed:success")
        self.assertEqual((0, "DRIFT CLEAN (nothing to judge)"), (self.drift("none", "--for-red", "2500"), self.last()))
        self.assertFalse(os.path.exists(self.path("state/dispatches")))

    def countries_seeds_and_skipped_rows(self) -> None:
        self.jobs(900, "takes-drift-1 (poland):completed:success", "takes-drift-1 (spain):completed:cancelled")
        self.assertEqual(2, self.drift("partial", "--countries", "pl,es", "--seeds", "1"))
        self.assertTrue(self.dispatched("-f takes-countries=pl,es -f drift-seeds=1,"), self.dispatches())
        self.assertIn("takes-drift-1 (spain) (cancelled)", self.last())
        self.assertEqual(2, self.drift("badseeds", "--seeds", "x"))
        self.jobs(901, "takes-drift-1 (poland):completed:skipped", "takes-drift-2 (poland):completed:skipped")
        self.write("state/status-901", "completed\n")
        self.assertEqual((2, "NO VERDICT: run 901 has no takes-drift row that passed or failed"),
                         (self.drift("allskipped", "--run", "901"), self.last()))
        self.jobs(901, "takes-drift-1 (poland):completed:success", "takes-drift-1 (spain):completed:skipped")
        self.assertEqual((0, "DRIFT CLEAN"), (self.drift("someskipped", "--run", "901"), self.last()))

    def lane(self) -> str:
        return next((line.rsplit(" -f lane=", 1)[1] for line in self.dispatches().splitlines() if " -f lane=" in line), "")

    def the_lane(self) -> None:
        # THE LANE (scripts/ci/measure-lane.sh): passed only to a workflow that has the input
        self.write("repo/.github/workflows/identity-takes.yml",
                   "name: Identity takes\non:\n    workflow_dispatch:\n        inputs:\n            lane:\n")
        self.git(self.checkout, "commit", "-qam", "identity-takes: the lane input")
        self.git(self.checkout, "push", "-q", "origin", "main")
        older, self.tip = self.tip, self.git(self.checkout, "rev-parse", "HEAD")
        self.no_dispatches()
        self.drift("landing", "--seeds", "1,2", "--countries", "de,uk")
        self.assertEqual("landing", self.lane())
        self.runs('{"databaseId": 3726, "number": 2517, "status": "completed"}')
        self.jobs(3726, "takes-drift-2 (germany):completed:failure")
        self.artifact(3726, "de", 2, 0, 1, 37)
        self.jobs(900, "takes-drift-2 (germany):completed:success")
        self.no_dispatches()
        self.drift("redfix", "--for-red", "3726")
        self.assertEqual("red-fix", self.lane())
        self.no_dispatches()
        self.drift("optional", "--lane", "measure", "--seeds", "1,2", "--countries", "de,uk")
        self.assertEqual("", self.lane())
        self.no_dispatches()
        self.tip = older
        self.drift("old", "--seeds", "1,2", "--countries", "de,uk")
        self.assertNotIn("-f lane=", self.dispatches())
        self.assertEqual(2, self.drift("badlane", "--lane", "urgent"))


if __name__ == "__main__":
    unittest.main()
