#!/usr/bin/env python3
# Purpose: The JS/TS quality ratchet (the pre-push jsquality check)
# THE JS/TS QUALITY RATCHET (the pre-push `jsquality` check; CLAUDE.md, "Code quality ratchets"): the production JS/TS's
# measures (scripts/js-quality-report --check, over scripts/js-quality/ratchet-areas.tsv: web/src/main/assets/js and
# infra/version-dashboard/src) may not grow past test/resources/js-quality-baseline.tsv -- the Scala ratchets' model
# (scripts/ci/quality_ratchet.py) in one baseline, each kind under the Scala ratchet's own trailer:
#
#   Complexity:   cognitive, cyclomatic, inner-defs     <file>:<function>[#n] (no line: an edit above never re-keys it)
#   Length:       method-lines <file>:<function>[#n], file-lines <file>
#   Duplication:  copies <file>|<file>... (the copies of every clone group spanning exactly those files), clone-tokens
#   Coupling:     fanout <file>, distance <directory>, cycle <member>|<member>...
#
# A row is `kind<TAB>key<TAB>value`, `limit<TAB>kind<TAB>value` a kind's limit; `#` lines are comments. A key with a
# baseline row may not rise past it; a key without one past its kind's limit fails (a NEW function, file, clone, cycle
# over a limit); the measure's limit may not rise past the baseline's. Only PRODUCTION rows are held: a row whose files
# all lie outside ratchet-areas.tsv's paths (a test, a page test, Playwright, a script) is reported, never gated. The
# baseline grows (a row added or raised, a limit raised) only in a commit changing it that carries the kind's trailer in
# its final trailer block (trailers.py, THE ATTRIBUTION RULE); a fall needs none, and `update` records it. No rename
# credit: a moved file's rows are new rows (record them with --update and the trailer).
#
#   js_quality_ratchet.py check --current <over-limit.tsv> [--range A..B ...]   exit 1 on a violation, 2 on an error
#   js_quality_ratchet.py update --current <over-limit.tsv>                     rewrite the baseline from the measure
from __future__ import annotations

import argparse
import os
import sys
from typing import Dict, List, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # the engine beside it, under `python3 -I` too
import baseline_ratchet  # noqa: E402

BASELINE = "test/resources/js-quality-baseline.tsv"
AREAS = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "js-quality", "ratchet-areas.tsv")
TRAILERS: Dict[str, Tuple[str, ...]] = {
    "Complexity": ("cognitive", "cyclomatic", "inner-defs"),
    "Length": ("method-lines", "file-lines"),
    "Duplication": ("copies", "clone-tokens"),
    "Coupling": ("fanout", "distance", "cycle"),
}
HEADER = """# THE JS/TS QUALITY BASELINE (scripts/js-quality-report --update writes it; scripts/ci/js_quality_ratchet.py holds
# every push touching production JS/TS to it). Every production function, file, clone set, module and directory
# (scripts/js-quality/ratchet-areas.tsv: web/src/main/assets/js, infra/version-dashboard/src) past a limit, frozen:
#   limit         each kind's limit: a key without a row stays at or below it
#   cognitive / cyclomatic / inner-defs   <file>:<function>[#n]                 Complexity:
#   method-lines <file>:<function>[#n], file-lines <file>                       Length:
#   copies <file>|<file>... (clone groups spanning exactly them), clone-tokens  Duplication:
#   fanout <file>, distance <directory>, cycle <member>|<member>...             Coupling:
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying its kind's trailer (CLAUDE.md, "Code quality ratchets"). Tests and scripts are reported, not held."""
HINT = ("Split the function or file, extract the clone, cut the dependency -- or record it: scripts/js-quality-report"
        " --update, committed with the kind's trailer (`Complexity:`, `Length:`, `Duplication:`, `Coupling: <why>`)"
        " in the commit message's final trailer block.")


def production_paths(areas_file: str = AREAS) -> Tuple[str, ...]:
    with open(areas_file) as f:
        lines = [line.split("\t") for line in f.read().splitlines() if line.strip() and not line.startswith("#")]
    return tuple(p.rstrip("/") + "/" for parts in lines for p in parts[2].split())


def spec(production: Tuple[str, ...]) -> baseline_ratchet.Spec:
    """The JS/TS ratchet: a key's file (or directory) is held when it lies under a production path."""
    return baseline_ratchet.Spec(
        title="JS/TS QUALITY", tool="js quality", baseline=BASELINE, header=HEADER, hint=HINT,
        update_command="scripts/js-quality-report --update",
        held_rows="production rows past a limit; tests and scripts reported, not held", trailers=TRAILERS,
        held=lambda p: p.startswith(production) or p + "/" in production)


# What scripts/ci/ratchet_diff.py reads of this ratchet: its kinds, its rows and which of them it holds
KINDS = tuple(kind for kinds in TRAILERS.values() for kind in kinds)
parse = baseline_ratchet.parse


def gated(key: str, production: Tuple[str, ...]) -> bool:
    return baseline_ratchet.gated(spec(production), key)


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description="The JS/TS quality ratchet (scripts/js-quality-report --check).")
    parser.add_argument("command", choices=("check", "update"))
    parser.add_argument("--current", required=True)
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--areas", default=AREAS)
    args = parser.parse_args(argv)
    try:
        production = production_paths(args.areas)
    except OSError as e:
        print(f"js_quality_ratchet: {e}", file=sys.stderr)
        return 2
    return baseline_ratchet.run(spec(production), args.command, args.current, args.range)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
