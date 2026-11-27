#!/usr/bin/env python3
"""Wait until THIS workflow run has uploaded artifact <name>, produced by another job of the run -- a country leg's order
row waits for the `scrape-fixtures-<cc>` its convergence row uploads (country-convergence-leg.yml).

  wait-for-run-artifact.sh <artifact name> <producing job name fragment> <timeout seconds>

Exits 0 once the artifact is listed, 1 the moment the producing job finishes without it (failed, cancelled or skipped:
it will never come) or when the timeout passes. THE TIMEOUT COUNTS FROM THE PRODUCER'S START: while every producing
job still waits for a runner the waiter waits too, saying so, for at most WAIT_QUEUED_SECONDS (default 1800) -- Record
scrape fixtures #181 (run 43122318896): the UK order row started at 23:47 and gave up at 00:07, "no artifact
scrape-fixtures-uk after 1080s", while its convergence row, which uploads it, was still queued for a runner; it started
at 00:12 and uploaded at 00:14. Needs GH_TOKEN with `actions: read`, GITHUB_REPOSITORY and GITHUB_RUN_ID; POLL_SECONDS
overrides the 60 s poll: on github.com GITHUB_TOKEN has 1,000 API requests an hour for the whole repository
(github-return-plan 2026-10-30, 3.4). The workflow runs it by the name scripts/ci/wait-for-run-artifact.sh; tested by
wait_for_run_artifact_test.py beside it."""
from __future__ import annotations

import os
import sys
import time
from typing import Callable, List, Mapping, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import machine  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

USAGE = "usage: wait-for-run-artifact.sh <artifact name> <producing job name fragment> <timeout seconds>"
UNSTARTED = ("queued ", "waiting ", "pending ", "requested ")
# `<status> <conclusion>` of every job whose name holds the fragment (read from the environment, never spliced into the
# filter); the producer has ended only once each of them has -- a matrix, or a fragment two names share, is still
# producing while one of its jobs runs.
JOBS = '.jobs[] | select(.name | contains(env.PRODUCER)) | "\\(.status) \\(.conclusion)"'


class Waiter:
    def __init__(self, gh: tool_github.GitHub, env: Mapping[str, str], name: str, producer: str,
                 clock: Callable[[], float], sleep: Callable[[float], None]) -> None:
        self.gh = gh
        self.jobs_gh = tool_github.GitHub(gh.runner, {**gh.env, "PRODUCER": producer}, gh.gh)
        self.name = name
        self.producer = producer
        self.run = f"repos/{env.get('GITHUB_REPOSITORY', '')}/actions/runs/{env.get('GITHUB_RUN_ID', '')}"
        self.begun = clock()
        self.clock = clock
        self.sleep = sleep
        self.poll = float(env.get("POLL_SECONDS") or "60")
        self.queued_limit = int(env.get("WAIT_QUEUED_SECONDS") or "1800")
        self.said = -1  # the last minute the queued wait was said, so a long wait says itself once a minute
        self.started: Optional[int] = None  # when a producing job was first seen running: the timeout counts from it

    def seconds(self) -> int:
        return int(self.clock() - self.begun)

    def listed(self) -> bool:
        """Whether the run lists the artifact; a failed listing is "not yet"."""
        names = self.gh.read(["api", f"{self.run}/artifacts?per_page=100", "--paginate", "--jq", ".artifacts[].name"],
                             timeout=120)
        return names is not None and self.name in names.split("\n")

    def jobs(self) -> List[str]:
        """The producing jobs' `<status> <conclusion>` lines; a failed listing says nothing of the producer."""
        lines = self.jobs_gh.read(["api", f"{self.run}/jobs?per_page=100", "--paginate", "--jq", JOBS], timeout=120)
        if lines is None:
            print("::warning::could not list this run's jobs; polling again", flush=True)
            return []
        return [line for line in lines.split("\n") if line]

    def ready(self) -> int:
        print(f"artifact {self.name} is ready after {self.seconds()}s", flush=True)
        return 0

    def queued(self) -> Optional[int]:
        """Not one producing job has started: the timeout has not begun. 1 past the queued bound, else None."""
        if self.seconds() >= self.queued_limit:
            print(f"::error::'{self.producer}' still waited for a runner after {self.seconds()}s, so {self.name} never "
                  "came", flush=True)
            return 1
        if self.seconds() // 60 != self.said:
            self.said = self.seconds() // 60
            print(f"'{self.producer}' is still queued for a runner after {self.seconds()}s: waiting for it to start",
                  flush=True)
        return None

    def wait(self, timeout: int) -> int:
        while True:
            code = self.step(timeout)
            if code is not None:
                return code
            self.sleep(self.poll)

    def step(self, timeout: int) -> Optional[int]:
        """One poll: the exit once the wait is decided, else None."""
        if self.listed():
            return self.ready()
        jobs = self.jobs()
        if jobs and all(line.startswith(UNSTARTED) for line in jobs):
            return self.queued()
        self.started = self.seconds() if self.started is None else self.started
        if jobs and all(line.startswith("completed ") for line in jobs):
            return self.finished(jobs)
        if self.seconds() - self.started >= timeout:
            print(f"::error::no artifact {self.name} after {timeout}s of '{self.producer}' running", flush=True)
            return 1
        return None

    def finished(self, jobs: List[str]) -> int:
        """Every producing job ended. It may have uploaded in the moment since the listing above: look once more."""
        if self.listed():
            return self.ready()
        ended = ",".join(line[len("completed "):] for line in jobs)
        print(f"::error::'{self.producer}' finished ({ended}) without uploading {self.name}", flush=True)
        return 1


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    if len(argv) < 3 or not all(argv[:3]):
        print(USAGE, file=sys.stderr)
        return 1
    gh = tool_github.GitHub(tool_process.SubprocessRunner(), env)
    return Waiter(gh, env, argv[0], argv[1], time.monotonic, time.sleep).wait(int(argv[2]))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
