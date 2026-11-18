#!/usr/bin/env python3
"""THE LANDING CHECK THAT ALREADY RAN THIS TREE (tool-self-tests.yml's plan, the owner, 2026-11-18): a push to main
whose very tree a landing check (.github/workflows/precommit-check.yml, agentctl's remote check) passed has had its
script self-tests and old refs run there, by the same hook, over the same files -- Main ran them all again, 165
runner-minutes in two days. This names that run, so Tool self-tests skips them; anything else -- a tree no check passed
(a push that landed some other way: from the laptop, a bot, a hand push), a check still going, failed or cancelled, the
fast-only batch check (compile and lints: no script tests), gh silent -- and they run.

Keyed on the TREE (`head_commit.tree_id`: what was checked is what landed, whatever commit carries it, whatever its
branch is called) and the RUN'S CONCLUSION (success, every job of it), never on a branch name: the fast-only mode is the
check's own record of what it ran, read from its ref only to leave such a run out.

Usage: landing_check_for_tree.py <tree sha> [--repo OWNER/NAME]   prints the run id and exits 0; exits 1 when there is
none (2 when gh cannot say): run the tests
"""

import argparse
import re
import subprocess
import sys

WORKFLOW = "precommit-check.yml"
# agentctl's batch check (scripts/ci/precommit-share.sh: check/fast-<id>, a red fix's check/red-fast-<id>) runs the
# fast-only set: the sbt check and the touched files' linters, never the script self-tests
FAST_ONLY = re.compile(r"^check/(red-)?fast-")


def runs(repo):
    """(run id, head branch, tree id, conclusion) of the workflow's newest completed successful runs."""
    out = subprocess.run(
        ["gh", "api", f"repos/{repo}/actions/workflows/{WORKFLOW}/runs?status=success&per_page=100",
         "--jq", '.workflow_runs[] | [.id, .head_branch, .head_commit.tree_id, .conclusion] | @tsv'],
        capture_output=True, text=True, timeout=60)
    if out.returncode != 0:
        raise RuntimeError(out.stderr.strip() or f"gh exited {out.returncode}")
    return [tuple(line.split("\t")) for line in out.stdout.splitlines() if line.count("\t") == 3]


def passed_for(tree, rows):
    """The first run that passed a full check of `tree`, or None."""
    for run_id, branch, tree_id, conclusion in rows:
        if tree_id == tree and conclusion == "success" and not FAST_ONLY.match(branch):
            return run_id
    return None


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("tree")
    ap.add_argument("--repo", default="{owner}/{repo}")
    a = ap.parse_args(argv)
    if not re.fullmatch(r"[0-9a-f]{40}", a.tree):
        print(f"landing_check_for_tree: {a.tree!r} is no tree sha", file=sys.stderr)
        return 2
    try:
        rows = runs(a.repo)
    except (RuntimeError, OSError, subprocess.TimeoutExpired) as e:
        print(f"landing_check_for_tree: gh cannot say ({e}): the tests run", file=sys.stderr)
        return 2
    run_id = passed_for(a.tree, rows)
    if run_id is None:
        return 1
    print(run_id)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
