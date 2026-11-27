"""WAITING FOR A RUN'S CONCLUSION, the one way the Python tools do it (scripts/lib; the retired scripts/ci/dispatch.sh's
wait_for_run, whose rules these are): the polls of scripts/ci_wait.py -- the one implementation, which scripts/ci-wait
runs as a command -- read in process, so a dispatched run is waited on by the same code as `ci-wait --run`, not by a second
process whose line is parsed back. A null or partial answer from gh is asked again, never taken for a verdict.

  wait_for_run(run, timeout, poll, gh, clock) -> the conclusion (success, failure, cancelled...), or None when the run
                                                 gave none within `timeout` seconds or could not be read (said)

A deadline already past still reads the run once. Self-contained: no agentctl, so CI and `run-script-tests --linux` run
it as it is."""
from __future__ import annotations

import os
import sys
from typing import Any, Callable, List, Optional, cast

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
import ci_wait  # noqa: E402

wait: Callable[..., int] = cast(Callable[..., int], getattr(ci_wait, "wait"))


def wait_for_run(run: str, timeout: int, poll: int, gh: Any = None, clock: Any = None) -> Optional[str]:
    timeout = max(timeout, 0)
    github = ci_wait.GitHub(gh or ci_wait.gh_runner(float(os.environ.get("CI_WAIT_GH_TIMEOUT", "60"))))
    lines: List[str] = []
    status = wait([ci_wait.Target(run_id=int(run), follow=False)], github, clock or ci_wait.Clock(), timeout,
                          poll, int(os.environ.get("CI_WAIT_MAX_MISSES", "10")), out=lines.append)
    if status in (ci_wait.EXIT_SUCCESS, ci_wait.EXIT_FAILURE, ci_wait.EXIT_CANCELLED):
        fields = lines[0].split("\t")
        return fields[2] if len(fields) > 2 else ""
    print(f"dispatch: run {run} gave no conclusion within {timeout}s (or could not be read)", file=sys.stderr)
    return None
