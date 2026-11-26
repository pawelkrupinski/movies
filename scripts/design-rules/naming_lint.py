#!/usr/bin/env python3
"""THE NAMING LINT (the owner's "names must match behaviour", 2026-11-19), over EVERY language: the production Scala's
top-level types, the Python tools' modules and classes, the production JS/TS's modules and classes -- read by the
general rule core's extractors (scripts/design-rules/extractors.py) and judged by its one naming implementation
(scripts/design-rules/naming.py). In the pre-push hook's quality, pyquality and jsquality checks.

  RULE 1, BLOCKS: two names of one package (a Scala package; a Python or JS directory) whose words are the same in
  another order (PruneSweep and SweepPrune, prune_sweep.py and sweep_prune.py): a reader cannot tell from the names
  which is which. Rename one after what makes it distinct. A pair being renamed may be allowed for now in
  ALLOWED_PERMUTATIONS (its owner and why); a row no clash matches any more is said, so it is struck.
  RULE 2, WARNS, new files only: a new type whose name ends in a noun of what it does to something else (Apart, Stages,
  Read(s), Reading(s), Builds, Rereads, Drafts, Traces, Denials, Plans) yet has at most one public def -- usually a
  function wearing a type's name: fold it into the class whose data it reads, or name the one thing it is. A warning,
  never a refusal (about 80% precise when measured on 2026-11-19).
The frozen naming rows -- a forbidden suffix (Impl, Manager, ...), an abbreviation -- are design_check.py's.

Usage:
  naming_lint.py check [--root DIR] [--range A..B ...] [--languages scala,py,js]   rule 1 over every production
      source of DIR (default .) in those languages (each pre-push check reads its own), rule 2 over the sources the
      ranges add; exit 1 on a rule-1 clash, 0 otherwise (warnings on stdout)
"""

import argparse
import os
import sys
from typing import Dict, FrozenSet, List, Set, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # the rule core beside it, under `python3 -I` too
import extractors  # noqa: E402
import naming  # noqa: E402
import tool_git  # noqa: E402
from model import Module  # noqa: E402

# (package, names): a clash allowed while it is renamed -- its owner and why. Struck once the rename lands.
ALLOWED_PERMUTATIONS: Dict[Tuple[str, FrozenSet[str]], str] = {
}


def modules(git: tool_git.Git, ranges: List[str], languages: Set[str]) -> List[Module]:
    return [m for found in extractors.by_language(git, languages, ranges).values() for m in found.values()]


def allowed_or_refused(found: List[Module]) -> List[str]:
    """Rule 1's refused clashes; the allowed ones and the stale allow rows said."""
    refused: List[str] = []
    seen: Set[Tuple[str, FrozenSet[str]]] = set()
    for (_, package), names in naming.permutations(found).items():
        key = (package, frozenset(names))
        said = f"{package}: " + ", ".join(f"{n} ({p})" for n, p in sorted(names.items()))
        if key in ALLOWED_PERMUTATIONS:
            seen.add(key)
            print(f"naming: {said} -- allowed: {ALLOWED_PERMUTATIONS[key]}")
        else:
            refused.append(said)
    for pkg, names in set(ALLOWED_PERMUTATIONS) - seen:
        print(f"naming: an ALLOWED_PERMUTATIONS row no clash matches any more ({pkg}: {', '.join(sorted(names))}):"
              " strike it in scripts/design-rules/naming_lint.py")
    return refused


def check(git: tool_git.Git, ranges: List[str], languages: Set[str]) -> int:
    found = modules(git, ranges, languages)
    refused = allowed_or_refused(found)
    for w in naming.action_names(m for m in found if m.new):
        print(f"naming WARNING (rule 2): a new type named for an action with at most one public def: {w} -- fold it"
              " into the class whose data it reads, or name the one thing it is")
    if not refused:
        print("naming: no two names of a package share their words in another order")
        return 0
    print("NAMING REFUSED (rule 1): two names of one package with the same words in another order -- rename one"
          " after what makes it distinct:")
    print("\n".join(f"  - {r}" for r in refused))
    return 1


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    c = sub.add_parser("check")
    c.add_argument("--root", default=".")
    c.add_argument("--range", action="append", default=[])
    c.add_argument("--languages", default=",".join(extractors.LANGUAGES), help="the languages read (scala,py,js)")
    args = parser.parse_args(argv)
    try:
        return check(tool_git.Git.here(os.path.abspath(args.root)), args.range, set(args.languages.split(",")))
    except (OSError, RuntimeError, tool_git.Failed) as e:
        print(f"naming_lint: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
