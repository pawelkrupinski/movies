#!/usr/bin/env python3
"""THE LOCAL WHAT-IF's two judgements (scripts/identity-what-if runs the resolves): which families a change can reach,
and what its takes did.

SELECT -- which base families the head must re-resolve. The changed code is mapped to the rule ids it can reach, and the
families whose base trace names one of those ids are picked; anything the mapping cannot attribute picks EVERY family,
and says why:

  inert            no change to what a take reads: tests, fixtures, docs, scripts, CI, Markdown, comment-only lines,
                   imports
  attributed       a change inside identity/src/main/scala, walked up its callers (every identity source line naming the
                   changed definition, at the base and at the head) until each path ends in a RULE ROOT
                   (scripts/identity-what-if-roots.tsv: a definition, the rule-id patterns a change to it can move -- its
                   own accept / refusal ids and the ids of the rules it pre-empts); a path that reaches a definition
                   nobody calls without passing a root (the resolver's own machinery, scoring, families) attributes
                   nothing
  everything else  the agreement stage (identity/.../identity/agreement/, or a definition its code calls: the FINAL take
                   of every family is settled again), common/, scrapers/, geodata/, worker/src/main (the lookups, the
                   families' sites), resources (weights, title rules), the build: ALL

A family is picked when any member's trace (DecisionTrace.rulesOf, the stage's id) matches a pattern (`*` any run). The
selection is a static reading of the code, so the what-if's `--verify` also resolves every family at the head and
fails when a take outside the selection moved.

DIFF -- each listing's base and head take (the model's: the head's from its re-resolve where selected, the base's
elsewhere), bucketed by the judgements exactly as scripts/identity_measure.py buckets a CI measurement (identity
_takes_diff's pairing and moves, identity_measure.bucket_of), plus prod's served take beside each.

usage: identity_what_if.py select --repo DIR --base SHA --head SHA|. --takes base/takes.tsv --out keys.txt [--roots F]
       identity_what_if.py reaches --repo DIR --base SHA --head SHA|. [--roots F]
       identity_what_if.py diff --cc CC --base DIR --head DIR --judgements F [--verify] [--tsv OUT] [--json]
select exits 0 and prints `selected: N of M families (...)` or `selected: ALL (<why>)`; reaches prints `none` (no change
reaches a take: the head need not be compiled or resolved), `rules` or `all`, from the code alone, before any side resolves.
diff exits 0, or 1 when --verify finds a take the selection missed.
"""

import argparse
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "ci"), os.path.join(HERE, "lib")]
from seams.tool_files import Files  # noqa: E402
# the two halves' names, still this tool's for its callers (its tests, scripts/ci/family_reach.py)
from identity_what_if_attribution import (attribute as attribute, changes_between as changes_between, diff_path as diff_path,  # noqa: E402,F401
                                          holder_at as holder_at, matches as matches, read_roots as read_roots,
                                          select as select, Sources as Sources)
from identity_what_if_takes import diff as diff, read_takes, render as render, run_diff, side_take as side_take  # noqa: E402,F401

ROOTS = os.path.join(HERE, "identity-what-if-roots.tsv")


def reach(args):
    """The rule-id patterns the change can move (None: every family, with why; empty: none) -- the code alone decides
    them, never the base's takes."""
    changes = changes_between(args.repo, args.base, args.head)
    return attribute(changes, Sources(args.repo, args.base, args.head), read_roots(args.roots))


def run_reaches(args):
    """`none` when no change reaches a take (the head need not be compiled or resolved), `all`, or `rules`."""
    patterns, _ = reach(args)
    print("all" if patterns is None else "rules" if patterns else "none")
    return 0


def run_select(args):
    rows = read_takes(args.takes)
    patterns, why = reach(args)
    families = len({r["family"] for r in rows})
    if patterns is None:
        keys, line = [r["key"] for r in rows], f"selected: ALL {families} families ({why})"
    elif not patterns:
        keys, line = [], f"selected: 0 of {families} families (no change reaches the model's take)"
    else:
        keys, picked, _ = select(rows, patterns)
        line = f"selected: {picked} of {families} families, {len(keys)} listings (rules {' '.join(sorted(patterns))})"
    with open(args.out, "w", encoding="utf-8") as f:
        f.writelines(k + "\n" for k in keys)
    print(line)
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    s = sub.add_parser("select")
    s.add_argument("--repo", required=True)
    s.add_argument("--base", required=True)
    s.add_argument("--head", required=True)
    s.add_argument("--takes", required=True)
    s.add_argument("--out", required=True)
    s.add_argument("--roots", default=ROOTS)
    r = sub.add_parser("reaches")
    r.add_argument("--repo", required=True)
    r.add_argument("--base", required=True)
    r.add_argument("--head", required=True)
    r.add_argument("--roots", default=ROOTS)
    d = sub.add_parser("diff")
    d.add_argument("--cc", required=True)
    d.add_argument("--base", required=True)
    d.add_argument("--head", required=True)
    d.add_argument("--judgements", required=True)
    d.add_argument("--verify", action="store_true")
    d.add_argument("--tsv")
    d.add_argument("--json", action="store_true")
    t = sub.add_parser("take")
    t.add_argument("--side", required=True)
    t.add_argument("--listing")
    args = parser.parse_args(argv)
    if args.command == "take":
        print(side_take(args.side, args.listing))
        return 0
    if args.command == "reaches":
        return run_reaches(args)
    return run_select(args) if args.command == "select" else run_diff(args, Files.here())


if __name__ == "__main__":
    sys.exit(main())
