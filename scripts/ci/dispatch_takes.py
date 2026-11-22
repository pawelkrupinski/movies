#!/usr/bin/env python3
"""WHATEVER STARTS IDENTITY MODEL CONVERGENCE STARTS IDENTITY TAKES (the owner, 2026-11-08: "make whatever kicks off
convergence kick off both"). Convergence's preflight runs this first: it dispatches identity-takes.yml on the same ref,
handing it every input of the convergence dispatch that means something to a takes row, and `parent-run` -- the
convergence run's id, which the takes run carries in its title (`Identity takes (convergence run <id>)`), so a reader
finds one from the other (scripts/ci/convergence_reuse.py takes-run, scripts/ci/dispatch.sh find_takes_run).

One entry point, so no caller of convergence (Main's kick, convergence-daily, identity-measure --full, a hand dispatch)
can start one suite without the other. IdentityTakesWiringSpec holds the forwarded inputs to the two workflows' own.

Usage: dispatch_takes.py <ref> <convergence run id>  (scripts/ci/dispatch-takes.sh, the name the workflow runs)
env: the convergence dispatch's inputs, INPUT_<NAME> (INPUT_TAKES_BASE, ...); empty ones are not passed (the takes
workflow's default). GH_TOKEN / GH_REPO as gh reads them.
Exit: 0 dispatched; 1 GitHub refused it through the dispatcher's retries (the convergence run fails: half a gate).
Tested by scripts/ci/dispatch_takes_test.py against a fake gh."""
from __future__ import annotations

import os
import sys
from typing import List, Mapping

sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import tool_dispatch  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402

# The inputs both workflows take (identity-takes.yml's names), as the convergence dispatch set them.
FORWARDED = ("profile", "jvm-options", "runner", "takes-base", "recording", "takes-no-store", "takes-skip", "drift-twice",
             "reuse-run")
USAGE = "usage: dispatch-takes.sh <ref> <convergence run id>"


def inputs(parent: str, env: Mapping[str, str]) -> List[str]:
    args = ["-f", f"parent-run={parent}"]
    for name in FORWARDED:
        value = env.get("INPUT_" + name.upper().replace("-", "_"), "")
        args += ["-f", f"{name}={value}"] if value else []
    return args


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if len(argv) != 2 or not all(argv):
        print(USAGE, file=sys.stderr)
        return 1
    ref, parent = argv
    # a rate-limit reset is waited out only within the preflight's own budget
    env = {"DISPATCH_RATE_LIMIT_MAX_WAIT": "120", **env}
    gh = tool_github.GitHub(tool_process.SubprocessRunner(), env, env.get("DISPATCH_GH") or "gh")
    args = inputs(parent, env)
    if tool_dispatch.Dispatcher(gh, tool_dispatch.Settings.of(env)).dispatch("identity-takes.yml", ref, args):
        print(f"dispatched Identity takes on {ref} for convergence run {parent} ({' '.join(args)})")
        return 0
    print(f"::error title=no takes run::could not dispatch identity-takes.yml on {ref} -- this convergence run has no "
          "Identity takes run beside it, so it fails rather than gate half a verdict")
    return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
