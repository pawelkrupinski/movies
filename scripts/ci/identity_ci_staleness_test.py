#!/usr/bin/env python3
"""scripts/ci/identity-ci-staleness.sh, black-box, against a fake `gh` (answering `run list` per workflow from a JSON
file) and a scratch repository whose history decides convergence coverage: each check's fresh and stale verdicts, the
exit code and the overdue output. Ported from identity-ci-staleness-test.sh; the same cases hold the shell tool and its
Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import datetime
import json
import os
import re
import subprocess
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/identity_ci_staleness.py", "scripts/ci/identity-ci-staleness.sh")
NOW = 1792065600  # 2026-10-14T12:00:00Z
GH = r"""
[ -n "${STUB_GH_FAILS:-}" ] && exit 1
while [ $# -gt 0 ] && [ "$1" != --workflow ]; do shift; done
cat "$STUB_RUNS/$2.json" 2>/dev/null || echo '[]'
"""
CONVERGENCE = "identity-model-convergence.yml"
TAKES = "identity-takes.yml"
RECORDING = "record-scrape-fixtures.yml"


def at(hours: int) -> int:
    return NOW - hours * 3600


def iso(epoch: int) -> str:
    return datetime.datetime.fromtimestamp(epoch, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def run_json(sha: str, status: str, conclusion: str, created: int, completed: int) -> str:
    """A `gh run list` entry: created and completed that many hours before NOW."""
    return json.dumps({"headSha": sha, "status": status, "conclusion": conclusion, "createdAt": iso(at(created)),
                       "updatedAt": iso(at(completed)), "url": f"u/{created}"})


class IdentityCiStalenessTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", GH, shell="/usr/bin/env bash")
        self.runs_dir = self.path("runs")
        os.makedirs(self.runs_dir)
        self.paths_file = self.write("paths.txt", "# the paths\nworker/src/main/**\nbuild.sbt\n")
        self.repo_dir = self.repo("repo", "-b", "main")
        self.base = self.commit("README", 10)
        self.pipeline = self.commit("worker/src/main/A.scala", 4)
        self.docs = self.commit("docs/notes.md", 1)
        self.green_recording()

    def commit(self, file: str, hours_ago: int) -> str:
        """A commit of `file` (a changed line) made that many hours before NOW: its sha."""
        full = os.path.join(self.repo_dir, file)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "a") as f:
            f.write(f"{file} {hours_ago} {len(os.listdir(self.repo_dir))}\n")
        date = f"@{at(hours_ago)}"
        env = cli_harness.isolated_env(os.environ, {"GIT_COMMITTER_DATE": date, "GIT_AUTHOR_DATE": date})
        subprocess.run(["git", "-C", self.repo_dir, "add", "-A"], env=env, check=True)
        subprocess.run(["git", "-C", self.repo_dir, "commit", "-qm", file], env=env, check=True)
        return self.git(self.repo_dir, "rev-parse", "HEAD")

    def runs(self, workflow: str, *entries: str) -> None:
        self.write(f"runs/{workflow}.json", "[" + ",".join(entries) + "]")

    def green_recording(self) -> None:
        self.runs(RECORDING, run_json(self.base, "completed", "success", 11, 10))

    def tool_env(self, **extra: Optional[str]) -> dict[str, Optional[str]]:
        env: dict[str, Optional[str]] = {"NOW": str(NOW), "STUB_RUNS": self.runs_dir,
                                         "CONVERGENCE_PATHS_FILE": self.paths_file}
        env.update(extra)
        return env

    def verdicts(self, check: str, cwd: Optional[str] = None, **extra: Optional[str]) -> str:
        """"stale" or "ok" for that check, from a run of the tool (empty when it printed none)."""
        done = self.run_tool(env=self.tool_env(**extra), cwd=cwd or self.repo_dir)
        found: List[str] = []
        for line in done.stdout.splitlines():
            if re.match(rf"^(::error::|::warning::|ok  ){re.escape(check)}:", line):
                found.append("ok" if line.startswith("ok  ") else "stale")
        return " ".join(found)

    def exit_code(self, **extra: Optional[str]) -> int:
        return self.run_tool(env=self.tool_env(GITHUB_OUTPUT=self.path("output"), **extra), cwd=self.repo_dir).code

    def overdue(self) -> str:
        return self.read("output").splitlines()[-1]

    def test_convergence_red_a_green_newest_decided_run_is_fresh(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", 1, 0),
                  run_json(self.pipeline, "completed", "failure", 3, 2))
        self.assertEqual("ok", self.verdicts("convergence-red"))

    def test_convergence_red_since_the_first_failure_is_stale_though_the_newest_failure_is_fresh(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "failure", 1, 0),
                  run_json(self.pipeline, "completed", "failure", 8, 7), run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("stale", self.verdicts("convergence-red"))

    def test_convergence_red_for_two_hours_is_not_yet_stale(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "failure", 3, 2),
                  run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("ok", self.verdicts("convergence-red"))

    def test_cancelled_and_running_runs_decide_nothing(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "cancelled", 1, 0),
                  run_json(self.docs, "in_progress", "", 0, 0), run_json(self.pipeline, "completed", "success", 3, 2),
                  run_json(self.base, "completed", "failure", 9, 8))
        self.assertEqual("ok", self.verdicts("convergence-red"))

    def test_a_run_completed_after_now_does_not_count(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", -1, -1),
                  run_json(self.pipeline, "completed", "failure", 8, 7))
        self.assertEqual("stale", self.verdicts("convergence-red"))

    def test_a_pipeline_commit_with_a_run_only_at_its_parent_is_unrun(self) -> None:
        self.runs(CONVERGENCE, run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("stale", self.verdicts("convergence-unrun"))

    def test_a_run_at_a_descendant_covers_it_whatever_its_verdict(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "failure", 1, 0))
        self.assertEqual("ok", self.verdicts("convergence-unrun"))

    def test_a_queued_run_at_it_covers_it(self) -> None:
        self.runs(CONVERGENCE, run_json(self.pipeline, "queued", "", 0, 0))
        self.assertEqual("ok", self.verdicts("convergence-unrun"))

    def test_a_cancelled_run_at_it_does_not(self) -> None:
        self.runs(CONVERGENCE, run_json(self.pipeline, "completed", "cancelled", 3, 3),
                  run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("stale", self.verdicts("convergence-unrun"))

    def test_with_only_build_sbt_a_pipeline_path_no_commit_touches_it(self) -> None:
        self.runs(CONVERGENCE, run_json(self.base, "completed", "success", 9, 8))
        self.write("paths.txt", "build.sbt\n")
        self.assertEqual("ok", self.verdicts("convergence-unrun"))

    def test_an_uncovered_pipeline_commit_under_three_hours_old_is_not_yet_due_a_run(self) -> None:
        self.runs(CONVERGENCE, run_json(self.base, "completed", "success", 9, 8))
        self.write("paths.txt", "worker/src/main/**\n")
        self.commit("worker/src/main/B.scala", 0)
        self.assertEqual("ok", self.verdicts("convergence-unrun"))

    def test_a_history_that_cannot_be_read_is_stale_not_no_commit_touches_the_paths(self) -> None:
        self.runs(CONVERGENCE, run_json(self.base, "completed", "success", 9, 8))
        not_a_checkout = self.path("not-a-checkout")
        os.makedirs(not_a_checkout)
        self.assertEqual("stale", self.verdicts("convergence-unrun", cwd=not_a_checkout, GIT_CEILING_DIRECTORIES=self.work))

    def test_takes_red_is_watched_as_convergence_is(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", 1, 0))
        self.runs(TAKES, run_json(self.docs, "completed", "failure", 1, 0),
                  run_json(self.pipeline, "completed", "failure", 8, 7), run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("stale", self.verdicts("takes-red"))
        self.assertEqual("ok", self.verdicts("convergence-red"))
        self.assertEqual("ok", self.verdicts("takes-unrun"))

    def test_a_green_takes_run_is_fresh_and_one_at_the_parent_leaves_the_commit_unrun(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", 1, 0))
        self.runs(TAKES, run_json(self.base, "completed", "success", 9, 8))
        self.assertEqual("ok", self.verdicts("takes-red"))
        self.assertEqual("stale", self.verdicts("takes-unrun"))

    def test_recording_a_success_ten_hours_ago_is_fresh(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", 1, 0))
        self.assertEqual("ok", self.verdicts("recording-stale"))

    def test_recording_a_last_success_forty_hours_ago_is_stale_whatever_failed_since(self) -> None:
        self.runs(RECORDING, run_json(self.base, "completed", "failure", 2, 1),
                  run_json(self.base, "completed", "cancelled", 20, 19), run_json(self.base, "completed", "success", 41, 40))
        self.assertEqual("stale", self.verdicts("recording-stale"))

    def test_recording_no_success_at_all_is_stale(self) -> None:
        self.runs(RECORDING, run_json(self.base, "completed", "failure", 2, 1))
        self.assertEqual("stale", self.verdicts("recording-stale"))

    def test_all_fresh_exits_zero_with_nothing_overdue(self) -> None:
        self.runs(CONVERGENCE, run_json(self.docs, "completed", "success", 1, 0))
        self.runs(TAKES, run_json(self.docs, "completed", "success", 1, 0))
        self.assertEqual(0, self.exit_code())
        self.assertEqual("overdue=false", self.overdue())

    def test_a_stale_check_is_overdue_work_not_a_red_run(self) -> None:
        self.runs(RECORDING, run_json(self.base, "completed", "failure", 2, 1))
        self.assertEqual(0, self.exit_code())
        self.assertEqual("overdue=true", self.overdue())

    def test_a_gh_that_cannot_list_runs_exits_one_never_a_quiet_zero(self) -> None:
        self.assertEqual(1, self.exit_code(STUB_GH_FAILS="fail"))
        self.assertEqual("overdue=true", self.overdue())

    def test_convergence_runs_that_cannot_be_judged_are_stale_never_ok(self) -> None:
        unreadable = json.dumps({"headSha": "x", "status": "completed", "conclusion": "failure",
                                 "createdAt": "2026-10-01T00:00:00Z", "updatedAt": None, "url": "u"})
        self.runs(CONVERGENCE, unreadable)
        self.assertEqual("stale", self.verdicts("convergence-red"))

    def test_recording_runs_that_cannot_be_judged_are_stale_never_ok(self) -> None:
        unreadable = json.dumps({"headSha": "x", "status": "completed", "conclusion": "success",
                                 "createdAt": "2026-10-01T00:00:00Z", "updatedAt": None, "url": "u"})
        self.runs(RECORDING, unreadable)
        self.assertEqual("stale", self.verdicts("recording-stale"))


if __name__ == "__main__":
    unittest.main()
