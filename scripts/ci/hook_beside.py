#!/usr/bin/env python3
"""One check beside the rest (.github/workflows/precommit-check.yml's `check (rest)`)
ONE CHECK BESIDE THE REST (.github/workflows/precommit-check.yml's `check (rest)`): the pre-push hook runs its checks
one after another, and in the rest's job `scripttests` (143-186 s) and then `sbt` (96-385 s) ran serially in 5 of 5
sampled runs (2026-11-18) -- two checks that share nothing: the script self-tests are bash and Python, the sbt check
a JVM. So the job runs the hook twice at once: once for <check> alone (PREPUSH_ONLY) and once for every other check
of its share (PREPUSH_ELSEWHERE, plus <check>), each with its own temp dir; the job's verdict is both.

  hook-beside.sh <check> -- <hook command...>

$CHECKS names the share's checks (space-separated; precommit-share.sh checks): only when it names <check> AND another
check do the two runs happen; else the command runs as it is. <check>'s output is kept in
$TMPDIR/<check>-beside.log and printed whole after the rest's, so the two never interleave (the job's failure steps
read $TMPDIR's logs too). Exit: a run's failure (anything but 0 and 75) first, else the stall signal (75), else 0.

NO TREE CHECK IN A SHARED CHECKOUT (landing check 44984605717, 2026-11-22): run-script-tests' tree check reads what
a run left by comparing the checkout before and after it, and moves each new path ASIDE while it re-runs the
candidates -- sound only while nothing else writes the checkout. Beside it the rest's itrest fetched the GitOps
manifests into infra/kubernetes; the check moved them aside for minutes, and LauncherJvmFlagsSpec and the other
deploy specs failed "infra/kubernetes/web/base/all.yaml is missing". The beside run keeps it off; the hook's own
serial runs, locally and on Main's Tool self-tests, keep it on.
Tests: scripts/ci/hook_beside_test.py."""
from __future__ import annotations

import os
import sys
import threading
from typing import Dict, List, Mapping, Optional, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from seams import tool_files  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

STALL = 75


def split_line(argv: Sequence[str]) -> Optional[Tuple[str, List[str]]]:
    """(check, hook command) of `<check> -- <command...>`; None when the line is not that."""
    if len(argv) >= 3 and argv[1] == "--":
        return argv[0], list(argv[2:])
    return None


def runs_beside(share: str, check: str) -> bool:
    """Whether the share names the check AND another one."""
    checks = share.split()
    return check in checks and any(c != check for c in checks)


def beside_env(env: Mapping[str, str], check: str) -> Dict[str, str]:
    return {**env, "PREPUSH_ONLY": check, "RUN_SCRIPT_TESTS_TREE_CHECK": "0"}


def rest_env(env: Mapping[str, str], check: str) -> Dict[str, str]:
    """Never a PREPUSH_ONLY this script was handed: it would make the rest's run <check>'s again."""
    kept = {k: v for k, v in env.items() if k != "PREPUSH_ONLY"}
    elsewhere = env.get("PREPUSH_ELSEWHERE", "")
    return {**kept, "PREPUSH_ELSEWHERE": f"{elsewhere} {check}" if elsewhere else check}


def job_exit(rest: int, beside: int) -> int:
    """A run's failure (not 0, not the stall signal) first, else the stall signal, else 0."""
    failed = [rc for rc in (rest, beside) if rc not in (0, STALL)]
    stalled = [rc for rc in (rest, beside) if rc == STALL]
    return (failed or stalled or [0])[0]


def run_beside(runner: tool_process.Runner, files: tool_files.Files, env: Mapping[str, str], check: str,
               command: List[str], log: str) -> int:
    """The command run twice at once, the check's part into `log`; the job's exit."""
    files.make_directories(os.path.dirname(log))
    result: List[int] = []
    beside = threading.Thread(target=lambda: result.append(
        runner.run(tool_process.Command(command, beside_env(env, check), log=log))))
    beside.start()
    print(f"hook-beside: {check} runs beside the rest's checks (its output below, once both end)", flush=True)
    rest = runner.run(tool_process.Command(command, rest_env(env, check)))
    beside.join()
    print(f"── {check}, run beside the rest (exit {result[0]}):")
    sys.stdout.write(files.read_text_or(log, ""))
    sys.stdout.flush()
    return job_exit(rest, result[0])


def main(argv: List[str], env: Mapping[str, str], runner: tool_process.Runner, files: tool_files.Files) -> int:
    line = split_line(argv)
    if line is None:
        print("usage: hook-beside.sh <check> -- <hook command...>", file=sys.stderr)
        return 2
    check, command = line
    if not runs_beside(env.get("CHECKS", ""), check):
        return runner.run(tool_process.Command(command, env))
    log = os.path.join(env.get("TMPDIR") or "/tmp", f"{check}-beside.log")
    return run_beside(runner, files, env, check, command, log)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment(), tool_git.shared_runner(), tool_files.Files.here()))
