#!/usr/bin/env python3
"""THE PROOF OF THE IDENTITY TEST-IMPACT SELECTION (scripts/ci/identity_impact.py, project/TestImpact.scala): that it
never leaves out a test that could fail. Two ways, each in a checkout of its own (--tree, default
~/.cache/kinowo-identity/impact-check, a detached worktree of this clone) through its own scripts/sbt-warm server:

  mutations   each MUTATION below, a change that fails some identity test, committed on the checkout's HEAD: the
              selection made of it, then EVERY test of the identity set run on it. SOUND when every class that failed
              is a selected one, and every selected country-scoped spec (tools.CountryScope) passes over the countries
              the selection left out; VACUOUS (said, not failed) when nothing failed.
  replay      the last -n commits of --ref (default origin/main) that touched an identity path, oldest first, each
              against its parent: the selection made at the commit (the tooling laid over it: identity-impact.sbt,
              project/TestImpact.scala, scripts/ci/identity_impact.py), then the identity set's tests the selection
              LEFT OUT run at the commit, and -- only where one fails -- at its parent. SOUND when no left-out test's
              outcome differs. Country scopes are reported; the commits' specs predate tools.CountryScope, so the
              mutations are the proof of those.

  hook        each MUTATION committed in the checkout -- detached, its own TMPDIR: a landing check's tree -- and the
              pre-push hook's own identity area run there with PREPUSH_IDENTITY_PLAN (the plan alone, nothing run):
              AGREES when the hook made a selection and it names exactly the classes this tool's own selection does;
              NO SELECTION when the hook fell back to every identity test (its why named: 2026-11-19's question,
              whether a check tree's selection silently fails), NO AREA when the hook runs no identity area for the
              change at all (is_identity_path, is_identity_reach_path), DISAGREES otherwise.

Usage: scripts/identity-impact-check mutations [--only NAME,...] | hook [--only NAME,...] | replay [-n 50] [--ref REF] [--from N]
Exit 0 sound, 1 a test the selection left out changed outcome (named; hook: a mutation not AGREES), 2 the check could
not be made.
Results: <tree>.results/<mode>.tsv, one row per mutation or commit; each step's logs beside it.
"""

from __future__ import annotations

import argparse
import os
import sys
from typing import List

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
sys.path[:0] = [HERE] + [os.path.join(HERE, sub) for sub in ("ci", "lib")]  # its modules, identity_impact, scripts/lib
from processes import machine  # noqa: E402
import identity_impact_check_tree as tree_module  # noqa: E402
import identity_impact_check_modes as modes  # noqa: E402
import tool_process  # noqa: E402

say = tree_module.say


def arguments(argv: List[str]) -> argparse.Namespace:
    ap = argparse.ArgumentParser(description=(__doc__ or "").split("\n")[0])
    ap.add_argument("mode", choices=["mutations", "hook", "replay"])
    ap.add_argument("--tree", default=os.path.expanduser("~/.cache/kinowo-identity/impact-check"))
    ap.add_argument("--only", default="")
    ap.add_argument("-n", type=int, default=50)
    ap.add_argument("--ref", default="origin/main")
    ap.add_argument("--from", dest="start", type=int, default=0, help="resume a replay at its N-th commit (0-based)")
    return ap.parse_args(argv)


def proof(c: tree_module.Checkout, a: argparse.Namespace, out: str) -> modes.Outcome:
    c.prepare(a.ref if a.mode == "replay" else c.head())
    os.makedirs(out, exist_ok=True)
    if a.mode == "replay":
        return modes.replay(c, out, a.n, a.ref, a.start)
    only = {name for name in str(a.only).split(",") if name}
    return (modes.mutations if a.mode == "mutations" else modes.hook)(c, out, only)


def results(outcome: modes.Outcome, table: str, appended: bool) -> None:
    """The rows into <tree>.results/<mode>.tsv (appended to a resumed replay's) and on stdout, each under its header."""
    with open(table, "a" if appended else "w", encoding="utf-8") as fh:
        if not appended:
            fh.write("\t".join(outcome.header) + "\n")
        fh.writelines("\t".join(r) + "\n" for r in outcome.rows)
    print("\t".join(outcome.header))
    for r in outcome.rows:
        print("\t".join(r))


def main(argv: List[str]) -> int:
    a = arguments(argv)
    out = a.tree + ".results"  # beside the checkout: a reset cleans its untracked files
    try:
        outcome = proof(tree_module.Checkout(tool_process.SubprocessRunner(), machine.environment(), REPO, a.tree), a, out)
    except (RuntimeError, OSError) as e:
        say(f"could not be made: {e}")
        return 2
    table = os.path.join(out, f"{a.mode}.tsv")
    results(outcome, table, bool(a.start))
    unsound = outcome.unsound
    say(f"{'UNSOUND: ' + ', '.join(unsound) if unsound else 'SOUND'} ({len(outcome.rows)} {a.mode}; {table})")
    return 1 if unsound else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
