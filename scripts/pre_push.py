#!/usr/bin/env python3
# Purpose: The pre-push hook's plan modes: which checks, compiles, lints and tests a list of changed paths selects
"""THE PRE-PUSH HOOK'S SELECTION, in Python (scripts/hooks/pre-push hands every plan mode here, and asks its own
questions of it while it runs: the checks a push runs, the test configs it compiles, the lint specs, the unit tests).

  scripts/hooks/pre-push --plan < paths       the checks, one a line, in run order (and every other plan mode the
                                              hook's header lists: --compile-plan, --lint-plan, --unit-plan, ...)
  scripts/pre_push.py <hook> <mode> [args]    what the hook runs: <hook> is its path (its tables lie beside it)
  scripts/pre_push.py <hook> --run [args]     THE RUN: what the hook does for a push (git's stdin) or a dry run
                                              (--range / --checks-only / --fast-only / --verify-only A..B):
                                              scripts/lib/pre_push_run.py, the checks the hook's own shell functions
  scripts/pre_push.py <hook> --check <c> <state>   one light check (scripts/lib/pre_push_light.py) at this directory,
                                              after the run's state (JSON): what the run starts for each

Paths are read on stdin, one a line; the answer goes to stdout. The checkout is the hook's working tree's root (git's);
the hook's tables (lint-registry.tsv, local-tests.txt) and scripts/run-script-tests are read beside the hook, as the
shell hook read them. scripts/hooks/pre_push_test.py holds every mode, on this and on the shell copy
(KINOWO_PRE_PUSH_LEGACY=1 runs scripts/hooks/pre-push.sh-legacy whole)."""
from __future__ import annotations

import os
import sys
from typing import List, MutableMapping, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), "lib"))
import machine  # noqa: E402
import pre_push_modes as modes  # noqa: E402
import pre_push_light as light  # noqa: E402
import pre_push_run  # noqa: E402
from pre_push_repo import Repo, Terminal  # noqa: E402
from tool_files import Files, LocalDisk  # noqa: E402

# git exports these for the repository being pushed; the hook runs at the checkout's root, where git finds it alone
GIT_VARIABLES = pre_push_run.GIT_VARIABLES


def checkout(hook: str, env: MutableMapping[str, str]) -> modes.Checkout:
    """The checkout the hook was run in, its root found by git; SystemExit(1) outside one."""
    for name in GIT_VARIABLES:
        env.pop(name, None)
    code, root = machine.output(["git", "rev-parse", "--show-toplevel"], env=env)
    if code != 0 or not root.strip():
        raise SystemExit(1)
    root = root.strip()

    def run(argv: Sequence[str], text: str) -> Tuple[int, str]:
        return machine.output(argv, text, cwd=root, env=env)
    return modes.Checkout(Files(LocalDisk(), root), os.path.dirname(os.path.abspath(hook)), env, run)


def check(hook: str, name: str, state: str) -> int:
    """`--check <name> <state>`: one of the light checks (pre_push_light), at this directory, after the run's state."""
    env = machine.environment()
    files = Files(LocalDisk(), os.getcwd())
    found = light.Check(Repo(os.getcwd(), env, files), Terminal(sys.stdout, sys.stderr),
                        light.state_of(files.read_text(state)), hook)
    return light.CHECKS[name](found)


def main(argv: List[str]) -> int:
    if len(argv) == 4 and argv[1] == "--check" and argv[2] in light.CHECKS:
        return check(argv[0], argv[2], argv[3])
    if len(argv) >= 2 and argv[1] == "--run":
        return pre_push_run.main(argv[0], argv[2:], machine.environment(), sys.stdin, sys.stdout, sys.stderr)
    if len(argv) < 2 or not modes.is_mode(argv[1]):
        sys.stderr.write(__doc__ or "")
        return 64
    hook, mode, args = argv[0], argv[1], argv[2:]
    found = checkout(hook, machine.environment())
    code, out = modes.answer(found, mode, args, "" if mode in ("--unit-universe", "--failure-summary") else sys.stdin.read())
    sys.stdout.write(out)
    return code


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
