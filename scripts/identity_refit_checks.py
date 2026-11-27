"""The checks a calibration change answers to, step 3 of scripts/identity-refit (identity_refit.py), run only when the
refit kept a change to identity-weights.json: the identity specs and the unmatched-cluster ratchet, the hard clusters
(a mongod of the run's own through scripts/it-local.sh, or the Mongo a workflow started: MONGODB_URI), then the
whole-corpus snapshots and the page snapshots regenerated -- deleted, written, run again to prove them stable -- and
the films the whole-corpus read model moved appended to the report. A red check ends the refit with its status."""
from __future__ import annotations

import datetime
import os
import re
import sys
from typing import Callable, List, Mapping, Optional, Sequence

import machine
import tool_git
import tool_process

FIXTURES = "test/resources/fixtures/08-06-2026"
SCHEDULES = f"{FIXTURES}/expected-schedules.txt"
PAGES = [f"{FIXTURES}/expected-index.html", f"{FIXTURES}/expected-wroclaw-index.html",
         f"{FIXTURES}/expected-warszawa-index.html"]
HARD_CLUSTERS = "worker/IntegrationTest/testOnly integration.HardClusterConvergenceIntegrationSpec"
MOVED_LINE = re.compile(r"^[-+][^-+]")
SbtRun = Callable[[Sequence[str]], int]


def log(line: str) -> None:
    now = datetime.datetime.fromtimestamp(machine.wall(), datetime.timezone.utc).strftime("%H:%M:%S")
    print(f"[identity-refit {now}] {line}", file=sys.stderr, flush=True)


def text(path: str) -> str:
    with open(path, errors="replace") as f:
        return f.read()


def append(path: str, content: str) -> None:
    with open(path, "a") as f:
        f.write(content)


class Checks:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], git: tool_git.Git, sbt_run: SbtRun) -> None:
        self.runner, self.env, self.git, self.sbt_run = runner, env, git, sbt_run

    def hard_clusters(self) -> int:
        if self.env.get("MONGODB_URI"):
            return self.sbt_run([HARD_CLUSTERS])
        # a mongod of the run's own; it-local.sh runs plain sbt, which must not share target/ with a live warm server
        if not self.env.get("CI"):
            stopped = self.runner.run(tool_process.Command(["scripts/sbt-warm", "--stop", "--root"], self.env))
            if stopped != 0:
                return stopped
        return self.runner.run(tool_process.Command(["scripts/it-local.sh", HARD_CLUSTERS], self.env))

    def regenerated(self, files: Sequence[str], spec: str) -> int:
        """`files` deleted and written again by `spec`, which then runs once more over them: they must be stable."""
        for path in files:
            if os.path.exists(path):
                os.remove(path)
        self.sbt_run([spec])
        return self.sbt_run([spec])

    def moved_films(self) -> str:
        lines = ["", "### Films the whole-corpus read model moved (expected-schedules.txt)", ""]
        if self.git.ok("diff", "--quiet", "--", SCHEDULES):
            lines.append("None.")
        else:
            diff = (self.git.out("diff", "-U0", "--no-color", "--", SCHEDULES) or "").splitlines()
            lines += ["```diff", *[line for line in diff if MOVED_LINE.search(line)][:400], "```"]
        return "\n".join(lines) + "\n"

    def steps(self, report: str) -> List[Callable[[], Optional[int]]]:
        return [
            lambda: log("identity specs and the unmatched-cluster ratchet"),
            lambda: self.sbt_run(["identity/testOnly services.identity.*",
                                  "worker/testOnly services.identity.* scripts.IdentityCalibrateSpec "
                                  "scripts.IdentityRefitSpec"]),
            lambda: log("hard clusters"),
            self.hard_clusters,
            lambda: log("regenerating the whole-corpus snapshots"),
            lambda: self.regenerated([SCHEDULES, f"{FIXTURES}/read-model-snapshot.json"],
                                     "e2e/Test/testOnly services.movies.FilmScheduleEndToEndSpec"),
            lambda: append(report, self.moved_films()),
            lambda: log("regenerating the page snapshots"),
            lambda: self.regenerated(PAGES, "web/PageTest/testOnly views.PageSnapshotSpec"),
        ]

    def run(self, report: str) -> int:
        for step in self.steps(report):
            status = step()
            if status:
                return status
        log(f"done: kept changes in identity/src/main/resources/identity-weights.json and the regenerated snapshots, "
            f"report {report}")
        return 0
