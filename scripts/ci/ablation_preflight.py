#!/usr/bin/env python3
"""IDENTITY RULE ABLATION'S PREFLIGHT (.github/workflows/identity-rule-ablation.yml, "Preflight the inputs"): what CAN be
known at t=0 fails here, before a row takes a runner -- a kind that is no rule kind, rounds that is no number, an `only`
rule of a kind not ablated (it would never be ablated), a recording neither the archive release nor this forge holds
(scripts/ci/recording-held.sh). Every fault is an `::error::` line, all said before the exit (1); 0 when none.

  env: KINDS, ROUNDS, ONLY, RECORDINGS (the dispatch's inputs; comma-separated lists), GITHUB_REPOSITORY, GH_TOKEN

The step's inline block until 2026-11-22 (the shell-to-Python migration's workflow blocks); its cases hold both
(ablation_preflight_test.py)."""
from __future__ import annotations

import os
import re
import sys
from typing import Callable, List, Mapping

HERE = os.path.dirname(os.path.abspath(__file__))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import tool_process  # noqa: E402

KINDS = ("title", "accept", "veto", "join", "apart")
WHOLE = re.compile(r"[0-9]+")


def fields(value: str) -> List[str]:
    """A comma-separated input as bash's `IFS=, read -r -a` splits it: its first line, a trailing comma ending the
    last field rather than starting an empty one."""
    parts = value.split("\n", 1)[0].split(",")
    return parts[:-1] if parts[-1] == "" else parts


def input_faults(env: Mapping[str, str]) -> List[str]:
    kinds = env.get("KINDS", "")
    faults = [f"kinds: '{k}' is not one of {', '.join(KINDS)}" for k in fields(kinds) if k not in KINDS]
    rounds = env.get("ROUNDS", "")
    if not WHOLE.fullmatch(rounds):
        faults.append(f"rounds: '{rounds}' is not a whole number")
    for rule in fields(env.get("ONLY", "")):
        # identity rules are `<kind>:<name>`; a title rule has no colon and needs `title` in kinds
        kind = rule.split(":", 1)[0] if ":" in rule else "title"
        if f",{kind}," not in f",{kinds},":
            faults.append(f"only: '{rule}' is a {kind} rule, and kinds is '{kinds}' -- it would never be ablated")
    return faults


def preflight(env: Mapping[str, str], held: Callable[[str], bool]) -> int:
    bad = 0
    for fault in input_faults(env):
        print(f"::error::{fault}", flush=True)
        bad = 1
    for run in fields(env.get("RECORDINGS", "")):
        if not held(run):  # where identity-fetch-recording.sh finds its corpus: the archive release, else its run
            bad = 1
    return bad


def main() -> int:
    """The composition point: the environment, and recording-held.sh through the real runner."""
    runner = tool_process.SubprocessRunner()
    env = dict(os.environ, GH_REPO=os.environ.get("GITHUB_REPOSITORY", ""))
    checker = os.path.join(HERE, "recording-held.sh")
    return preflight(os.environ, lambda run: runner.run(tool_process.Command([checker, run], env)) == 0)


if __name__ == "__main__":
    sys.exit(main())
