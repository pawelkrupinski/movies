#!/usr/bin/env python3
"""THE FAMILY CHECK'S REACH (the owner, 2026-11-06, option 1): which families a change must walk again. The rest are
served from the check's cache, kept under rules that name no code, so a commit pays only for the families its code can
move -- and a full cold walk runs nightly (convergence-daily.yml).

The mapping is the what-if's (scripts/identity_what_if.py `attribute`): the changed code walked up its callers to the
rule roots (scripts/identity-what-if-roots.tsv), and the rule-id patterns those roots can move. A family is walked again
when any rule its walk fired -- in any subset, the check's verdict records them -- or its index entry fired matches one.
Anything the mapping cannot attribute walks EVERY family and says why: the agreement stage, code outside the identity
model, shared resolver code. And the check itself: a change to its own tools (the lattice walk, the final settle, the
family index, the drift harness it replays answers through) walks everything too.

  family_reach.py reach --repo DIR --base SHA --head SHA --out FILE
      writes the patterns, one a line (`*` alone: ALL; empty: none), prints `reach: ...` with why
  family_reach.py price --walks family-walks-<cc>.tsv --reach FILE
      the seconds the reached families' walks cost (the walks table of a cold run), and how many

Exit 0, 2 on usage or an unreadable input. Tested by family_reach_test.py.
"""

import argparse
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
import identity_what_if as what_if  # noqa: E402

# the check's own tools: a change to one walks every family (nothing about the code names which families it moves)
CHECK_TOOLS = (
    "worker/src/test/scala/services/identity/FamilyConvergence.scala",
    "worker/src/test/scala/services/identity/agreement/FamilyFinalReads.scala",
    "worker/src/test/scala/scripts/IdentityDrift.scala",
    "worker/src/test/scala/scripts/IdentitySlice.scala",
    "identity/src/main/scala/services/identity/FamilyIndex.scala",
    "identity/src/main/scala/services/identity/PopulationMonotonicity.scala",
    "scripts/ci/identity-family-check.sh",
    "scripts/ci/family_reach.py",
    "scripts/identity_what_if.py",
    "scripts/identity-what-if-roots.tsv",
)


def reach(repo, base, head):
    """(patterns, why): `['*']` for ALL, `[]` for none."""
    changes = what_if.changes_between(repo, base, head)
    tools = sorted({path for path, *_ in changes if path in CHECK_TOOLS})
    if tools:
        return ["*"], f"the family check's own tools changed ({' '.join(tools)})"
    patterns, why = what_if.attribute(changes, what_if.Sources(repo, base, head), what_if.read_roots(what_if.ROOTS))
    if patterns is None:
        return ["*"], why
    if not patterns:
        return [], "no change reaches a rule"
    return sorted(patterns), f"rules {' '.join(sorted(patterns))}"


def read_walks(path):
    with open(path, encoding="utf-8") as f:
        header = f.readline().rstrip("\n").split("\t")
        return [dict(zip(header, line.rstrip("\n").split("\t"))) for line in f if line.strip()]


def price(walks, patterns):
    """(families reached, their seconds, every family, every family's seconds)."""
    every = sum(float(w["seconds"].replace(",", ".")) for w in walks)
    if "*" in patterns:
        return len(walks), every, len(walks), every
    hit = [w for w in walks if what_if.matches(w["rules"].split() if w.get("rules") else [], patterns)]
    return len(hit), sum(float(w["seconds"].replace(",", ".")) for w in hit), len(walks), every


def main(argv=None):
    p = argparse.ArgumentParser(description="the family check's reach")
    sub = p.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("reach")
    r.add_argument("--repo", required=True)
    r.add_argument("--base", required=True)
    r.add_argument("--head", required=True)
    r.add_argument("--out", required=True)
    c = sub.add_parser("price")
    c.add_argument("--walks", required=True)
    c.add_argument("--reach", required=True)
    a = p.parse_args(argv)
    try:
        if a.cmd == "reach":
            patterns, why = reach(a.repo, a.base, a.head)
            with open(a.out, "w", encoding="utf-8") as f:
                f.writelines(x + "\n" for x in patterns)
            print(f"reach: {'ALL' if '*' in patterns else len(patterns)} ({why})")
        else:
            with open(a.reach, encoding="utf-8") as f:
                patterns = [x.strip() for x in f if x.strip()]
            n, seconds, total, every = price(read_walks(a.walks), patterns)
            print(f"price: {n} of {total} families, {seconds:.0f} of {every:.0f} s of walking")
    except (OSError, KeyError, ValueError, what_if.subprocess.CalledProcessError) as e:
        print(f"family_reach.py: {e}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
