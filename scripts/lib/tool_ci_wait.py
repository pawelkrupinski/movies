"""WAITING FOR A RUN'S CONCLUSION, the one way the Python tools do it (scripts/lib; scripts/ci/dispatch.sh's
wait_for_run, whose rules these are): scripts/ci-wait reads the run, bounded, until it ends -- a null or partial answer
from gh asked again, never taken for a verdict -- and its line's third field is the conclusion.

  wait_for_run(runner, env, run, timeout, poll) -> the conclusion (success, failure, cancelled...), or None when the run
                                                  gave none within `timeout` seconds or could not be read (said)

A deadline already past still reads the run once."""
from __future__ import annotations

import os
import sys
from typing import Mapping, Optional

import tool_process

CI_WAIT = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "ci-wait")


def wait_for_run(runner: tool_process.Runner, env: Mapping[str, str], run: str, timeout: int,
                 poll: int) -> Optional[str]:
    timeout = max(timeout, 0)
    done = runner.capture(tool_process.Command(
        [CI_WAIT, "--run", run, "--no-follow", "--timeout", f"{timeout}s", "--interval", f"{poll}s"], env))
    sys.stderr.write(done.stderr)
    if done.code in (0, 1, 3):  # success, failure, cancelled: each a conclusion
        fields = done.stdout.rstrip("\n").split("\t")
        return fields[2] if len(fields) > 2 else ""
    print(f"dispatch: run {run} gave no conclusion within {timeout}s (or could not be read)", file=sys.stderr)
    return None
