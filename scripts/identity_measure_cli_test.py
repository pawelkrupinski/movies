#!/usr/bin/env python3
"""scripts/identity-measure, black-box, against a fake gh and scratch repositories: the commit goes out on a temporary
measure/ branch that is deleted whatever happens, the dispatch names the base and the takes-only rows, an old commit
is measured under main's CI, the what-if gate, --no-wait / --run / --timeout / --cancel-on-timeout, SIGTERM, --wait's
ETA and backoff, and the downloaded takes artifacts become a LANDABLE / NOT LANDABLE verdict. Ported from
identity-measure-test.sh case for case (its reuse, footprint and result-cache cases: identity_measure_cli_reuse_test.py);
the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import signal
import subprocess
import unittest

from identity_measure_cli_fixture import MeasureCase, cli_harness, user

PL = ("--countries", "pl")


class IdentityMeasureTest(MeasureCase):

    def test_a_commit_that_moved_nothing_is_landable(self) -> None:
        self.assertEqual(0, self.measure("landable", *PL), self.out)
        # pushed as itself on a measure/<user>/<short sha>-<random> branch, named after the commit
        self.assertEqual(self.tip, self.stub("pushed"))
        self.assertTrue(self.stub("branch").startswith(f"{user()}/{self.tip[:9]}-"), self.stub("branch"))
        # dispatching only the takes rows against the commit's parent (it is on main already)
        self.assertEqual(f"workflow run identity-takes.yml --ref measure/{self.stub('branch')} -f takes-base={self.parent}"
                         " -f takes-only=true -f takes-countries=pl", self.stub("dispatch"))
        self.assertEqual([], self.measure_branches())
        self.assertEqual(1, self.lines("^LANDABLE"), self.out)
        self.assertEqual(1, self.lines("^pl +model .*failure"), self.out)
        # where its time went: the wait for runners, from the first job's start, not the run's; each row and phase
        self.assertIn("waited for runners 1188s, then ran 1256s", self.out)
        self.assertIn("leg (poland) / takes (poland): 582s failure", self.out)
        self.assertIn("pl phases: head-resolve 546s (resolved), base-resolve 3s (reused)", self.out)

    def test_the_what_if_gate(self) -> None:
        # no what-if verdict for the head and base: refused before any push or dispatch
        self.assertEqual(2, self.measure("landable", *PL, gate=True), self.out)
        self.assertEqual(([], False), (self.measure_branches(), self.dispatched()))
        self.assertIn(f"scripts/identity-what-if --base {self.parent} --head {self.tip} pl", self.out)
        self.assertIn("--no-what-if", self.out)
        verdict = self.write(f"cache/what-if-verdicts/{self.tip}-{self.parent}.tsv",
                             "pl\tdone\t2026-11-06T10:00:00Z\t/x/takes-diff-pl.tsv\n")
        self.assertEqual(0, self.measure("landable", *PL, gate=True, keep_cache=True), self.out)
        self.assertIn(f"what-if done for {self.tip[:9]} against {self.parent[:9]}: pl done", self.out)
        # one against another base is not that one
        os.rename(verdict, self.path(f"cache/what-if-verdicts/{self.tip}-{self.old}.tsv"))
        self.assertEqual(2, self.measure("landable", *PL, "--fresh", "--no-cache", gate=True, keep_cache=True))
        self.assertEqual(0, self.measure("landable", *PL, "--no-what-if", "live asks being added", gate=True))
        self.assertIn("WHAT-IF SKIPPED (--no-what-if): live asks being added", self.out)

    def test_cancelled_runs_and_the_lane(self) -> None:
        # a run cancelled more than twice is dispatched again until it runs: four dispatches in all
        self.assertEqual(0, self.measure("landable", *PL, env={"CANCELS": "3"}), self.out)
        self.assertIn("dispatching (attempt 4)", self.out)
        # a measurement does not wait behind another run of the workflow: it has a lane of its own
        self.assertEqual(0, self.measure("landable", "--timeout", "0", *PL, env={"LANE_BUSY": "1"}), self.out)

    def test_a_right_take_lost_is_not_landable(self) -> None:
        self.assertEqual(1, self.measure("lost", "--base", self.parent, *PL), self.out)
        self.assertIn("pl model: 1 right-lost", self.out)
        self.assertIn("Helios | Lalka: tmdb:1 Lalka (2026) -> none", self.out)
        self.assertEqual([], self.measure_branches())

    def test_a_commit_from_before_takes_only_is_measured_under_mains_ci(self) -> None:
        self.assertEqual(0, self.measure("landable", self.old, "--base", self.old, *PL), self.out)
        pushed = self.stub("pushed")
        self.assertEqual(self.old, self.git(self.checkout, "rev-parse", f"{pushed}^"))
        self.assertEqual("takes-only: {} takes-countries: {}", self.git(
            self.checkout, "show", f"{pushed}:.github/workflows/country-convergence-leg.yml").split("\n")[0])
        self.assertEqual("new", self.git(self.checkout, "show", f"{pushed}:scripts/ci/identity-takes-check.sh"))
        with self.assertRaises(subprocess.CalledProcessError):  # and nothing else of main's
            self.git(self.checkout, "cat-file", "-e", f"{pushed}:rule.txt")

    def test_retries_flags_and_failures(self) -> None:
        # a run read that fails once is retried; --country, the other tools' spelling, is the same flag
        self.assertEqual(0, self.measure("landable", *PL, env={"VIEW_BLIPS": "1"}), self.out)
        self.assertEqual(0, self.measure("landable", "--country", "pl", env={"VIEW_BLIPS": "1"}), self.out)
        # --head names the commit, --recording the run every leg replays: the workflow's recording input
        self.assertEqual(0, self.measure("landable", "--head", self.tip, "--recording", "528", *PL), self.out)
        self.assertIn("-f takes-only=true -f recording=528 -f takes-countries=pl", self.stub("dispatch"))
        # ...never answered by an earlier measurement over the pinned pair
        self.measure("landable", *PL, keep_cache=True)
        self.assertEqual(0, self.measure("landable", "--recording", "528", *PL, keep_cache=True), self.out)
        self.assertTrue(self.dispatched())
        self.assertEqual(2, self.measure("landable", "--recording", "latest"), "a recording is a run id")
        self.assertEqual(2, self.measure("landable", "--countries", "pl,xx"), "a country no tool knows")
        self.assertEqual(0, self.measure("landable", *PL, env={"DOWNLOAD_BLIPS": "1"}), self.out)
        # a takes download that keeps failing is no measurement, not a run without takes
        self.assertEqual(2, self.measure("landable", *PL, env={"DOWNLOAD_FAILS": "1"}), self.out)
        self.assertEqual(1, self.lines("could not download run 777's identity-takes"), self.out)
        self.assertEqual(2, self.measure("landable", *PL, env={"VIEW_FAILS": "1"}), self.out)
        self.assertEqual([], self.measure_branches())

    def test_no_wait_then_run(self) -> None:
        self.assertEqual(0, self.measure("landable", "--no-wait", *PL), self.out)
        self.assertEqual(1, len(self.measure_branches()))
        self.assertIn("scripts/identity-measure --run 777", self.out)
        self.git(self.checkout, "push", "-q", "origin", "--delete", f"refs/heads/measure/{self.stub('branch')}")
        self.push_run_branch()
        tip = {"HEAD_SHA": self.tip}
        self.assertEqual(1, self.measure("lost", "--run", "777", *PL, env=tip), self.out)
        # a footprint-limited run over every country: the rest were never measured, not NOT LANDABLE for want of them
        self.assertEqual(0, self.measure("landable", "--run", "777", env=tip), self.out)
        self.assertEqual(4, self.lines(r"^(uk|de|us|es) .*not measured \(outside the footprint\)$"), self.out)
        self.assertEqual(([], False), (self.measure_branches(), self.dispatched()))
        # a run still in progress is waited for (bounded by --timeout), then collected
        self.push_run_branch()
        self.assertEqual(1, self.measure("lost", "--run", "777", *PL, env={**tip, "RUN_PENDING": "1"}), self.out)
        self.assertIn("waiting for run 777", self.out)
        self.push_run_branch()
        self.assertEqual(3, self.measure("lost", "--run", "777", "--no-wait", *PL, env={**tip, "RUN_PENDING": "1"}))
        self.assertEqual(3, self.measure("lost", "--run", "777", "--timeout", "0", *PL,
                                         env={**tip, "RUN_PENDING": "1", "IN_PROGRESS": "1"}), self.out)
        self.assertIn("gave no conclusion within --timeout 0 minutes", self.out)
        self.assertEqual(1, len(self.measure_branches()), "the branch of the run still going is kept")
        self.git(self.checkout, "push", "-q", "origin", "--delete", "refs/heads/measure/abc-1")

    def test_run_classifies_with_the_checkouts_judgements_under_a_lock(self) -> None:
        # Lalka judged wrong since the run, not yet committed: its loss is a cure
        self.judge_lalka_wrong()
        self.push_run_branch()
        self.assertEqual(0, self.measure("lost", "--run", "777", *PL, env={"HEAD_SHA": self.tip}), self.out)
        self.git(self.checkout, "checkout", "-q", "--", "test/resources/fixtures/identity-judgements.tsv")
        # classifying in a scratch directory of its own: another agent's --run of the run classifies with its own
        kept = os.listdir(self.path("cache", "measure", "777"))
        self.assertEqual([], [name for name in kept if name in ("rows.json", "identity-judgements.tsv")])
        # the shared cache's download of a run is made under a lock: another agent's live download is waited for
        os.makedirs(self.path("cache", "locks"), exist_ok=True)
        self.hold_lock(self.path("cache", "locks", "measure-777.flock"))
        if os.path.exists(self.path("cache", "measure", "777", "downloaded")):
            os.remove(self.path("cache", "measure", "777", "downloaded"))
        self.push_run_branch()
        self.assertEqual(2, self.measure("lost", "--run", "777", *PL, keep_cache=True,
                                         env={"HEAD_SHA": self.tip, "KINOWO_IDENTITY_LOCK_WAIT": "2"}), self.out)
        self.assertEqual([], [c for c in self.fakes.calls("gh") if c[:2] == ["run", "download"]])

    def test_the_branch_on_every_exit_path(self) -> None:
        self.assertEqual(2, self.measure("landable", *PL, env={"DISPATCH_FAILS": "1"}), "a dispatch refused")
        self.assertEqual([], self.measure_branches())
        # a run past --timeout is no measurement, but is not cancelled: exit 3, its branch kept for --run
        running = {"IN_PROGRESS": "1"}
        self.assertEqual(3, self.measure("landable", "--timeout", "0", *PL, env=running), self.out)
        self.assertEqual(("", 1), (self.stub("cancelled"), len(self.measure_branches())))
        self.assertIn("scripts/identity-measure --run 777", self.out)
        self.drop_measure_branches()
        cancel = ("--timeout", "0", "--cancel-on-timeout", *PL)
        self.assertEqual(2, self.measure("landable", *cancel, env=running), self.out)
        self.assertEqual(("cancelled", []), (self.stub("cancelled"), self.measure_branches()))
        # ...even collecting early, while its takes rows still run
        self.assertEqual(2, self.measure("landable", *cancel, env={**running, "IDENTITY_MEASURE_EARLY_COLLECT": "1"}))
        # a run whose takes rows have ended is collected without waiting for the run's tail
        ended = {**running, "TAKES_STATUS": "completed"}
        self.assertEqual(1, self.measure("lost", "--timeout", "0", *PL,
                                         env={**ended, "IDENTITY_MEASURE_EARLY_COLLECT": "1"}), self.out)
        self.assertEqual(1, self.lines("every takes row has ended"), self.out)
        # ...but not where this tool, not the run, deletes the branch (github.com)
        self.assertEqual(2, self.measure("lost", *cancel, env={**ended, "IDENTITY_MEASURE_EARLY_COLLECT": "0"}))
        self.assertEqual(("cancelled", []), (self.stub("cancelled"), self.measure_branches()))
        # a second signal while cleaning up does not stop the clean-up
        self.measure("landable", *cancel, env={**running, "SIGNAL_ON_CANCEL": "1"})
        self.assertEqual([], self.measure_branches())
        # a push that failed after origin took the branch still deletes it
        self.assertEqual(2, self.measure("landable", *PL, env={"PUSH_FAILS_AFTER": "1"}), self.out)
        self.assertEqual([], self.measure_branches())

    def test_sigterm_while_waiting_cancels_the_run_and_deletes_the_branch(self) -> None:
        os.makedirs(self.path("stub"))
        env = self.env({**self.stub_env("landable"), "IN_PROGRESS": "1", "IDENTITY_MEASURE_POLL": "1"})
        with open(self.path("out"), "w") as out:
            tool = subprocess.Popen([self.TOOL, "--no-what-if", "the measure spec", *PL], env=env, cwd=self.checkout,
                                    stdin=subprocess.DEVNULL, stdout=out, stderr=subprocess.STDOUT,
                                    start_new_session=True)
        self.addCleanup(lambda: tool.poll() is not None or os.killpg(tool.pid, signal.SIGKILL))
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("stub", "watched"))), self.read("out"))
        tool.send_signal(signal.SIGTERM)
        # bounded: a tool that defers the signal must fail this, not hang it
        try:
            status = tool.wait(20)
        except subprocess.TimeoutExpired:
            os.killpg(tool.pid, signal.SIGKILL)
            status = tool.wait()
        self.assertEqual(130, status, self.read("out"))
        self.assertEqual(("cancelled", []), (self.stub("cancelled"), self.measure_branches()))

    def test_wait_says_the_eta_then_backs_off(self) -> None:
        self.write("sleepbin/sleep", '#!/bin/sh\necho "$1" >> "$SLEEP_LOG"\n')
        os.chmod(self.path("sleepbin", "sleep"), 0o755)
        sleeping = {"PATH": self.path("sleepbin") + os.pathsep + os.environ.get("PATH", ""),
                    "SLEEP_LOG": self.path("slept")}

        def eta_run(conclusion: str, branch: str, ended: str) -> object:
            return {"status": "completed", "conclusion": conclusion, "headBranch": branch,
                    "createdAt": "2026-10-23T10:00:00Z", "updatedAt": f"2026-10-23T{ended}:00Z"}
        recent = json.dumps([eta_run("success", "measure/a", "10:30"), eta_run("failure", "measure/b", "10:50"),
                             eta_run("success", "main", "13:00")])
        self.assertEqual(0, self.measure("landable", *PL, "--wait", env={**sleeping, "ETA_RUNS": recent}), self.out)
        self.assertIn("run 777: ETA ~50 min (median of 2 recent runs)", self.out)
        self.slept()
        # then agentctl wait's backoff (60 s, x1.5, at most 5 min) between reads
        self.assertEqual((0, "60 90 135"), (self.measure("landable", *PL, "--wait", env={
            **sleeping, "VIEW_PENDING": "3"}), self.slept()))
        # where without --wait it reads every DISPATCH_POLL_SECONDS, and says no ETA
        self.assertEqual((0, "0 0 0"), (self.measure("landable", *PL, env={**sleeping, "VIEW_PENDING": "3"}),
                                        self.slept()))
        self.assertNotIn("ETA", self.out)
        self.assertEqual(0, self.measure("landable", *PL, "--wait", env=sleeping), self.out)
        self.assertIn("run 777: no ETA", self.out)
        self.assertEqual(2, self.measure("landable", *PL, "--wait", "--no-wait"), "--wait and --no-wait together")

    def slept(self) -> str:
        """What the stubbed `sleep` was asked since the last read, space-separated."""
        if not os.path.exists(self.path("slept")):
            return ""
        text = " ".join(self.read("slept").split())
        os.remove(self.path("slept"))
        return text


if __name__ == "__main__":
    unittest.main()
