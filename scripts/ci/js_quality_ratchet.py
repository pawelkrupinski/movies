#!/usr/bin/env python3
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
import subprocess
import sys
from typing import Dict, List, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # trailers.py beside it, under `python3 -I` too
import trailers  # noqa: E402

BASELINE = "test/resources/js-quality-baseline.tsv"
AREAS = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "js-quality", "ratchet-areas.tsv")
TRAILERS: Dict[str, Tuple[str, ...]] = {
    "Complexity": ("cognitive", "cyclomatic", "inner-defs"),
    "Length": ("method-lines", "file-lines"),
    "Duplication": ("copies", "clone-tokens"),
    "Coupling": ("fanout", "distance", "cycle"),
}
KINDS = tuple(kind for kinds in TRAILERS.values() for kind in kinds)
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

Rows = Dict[Tuple[str, str], float]


def parse(text: str) -> Rows:
    rows: Rows = {}
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        parts = line.split("\t")
        try:
            if len(parts) != 3:
                raise ValueError
            rows[(parts[0], parts[1])] = float(parts[2])
        except ValueError:
            raise ValueError(f"line {n}: not `kind<TAB>key<TAB>value`: {line!r}") from None
    return rows


def show(value: float) -> str:
    return str(int(value)) if value == int(value) else f"{value:g}"


def production_paths(areas_file: str = AREAS) -> Tuple[str, ...]:
    with open(areas_file) as f:
        lines = [line.split("\t") for line in f.read().splitlines() if line.strip() and not line.startswith("#")]
    return tuple(p.rstrip("/") + "/" for parts in lines for p in parts[2].split())


def gated(key: str, production: Tuple[str, ...]) -> bool:
    """A row is held when one of its key's files (or directories) lies under a production path."""
    return any(p.startswith(production) or p + "/" in production for p in key.split("|"))


def of_kind(rows: Rows, kind: str, production: Tuple[str, ...]) -> Dict[str, float]:
    return {key: value for (k, key), value in rows.items() if k == kind and gated(key, production)}


def limit_of(rows: Rows, kind: str) -> float:
    if ("limit", kind) not in rows:
        raise ValueError(f"no `limit<TAB>{kind}<TAB>N` row")
    return rows[("limit", kind)]


def violations(current: Rows, baseline: Rows, production) -> List[str]:
    out = []
    for kind in KINDS:
        limit = limit_of(baseline, kind)
        if limit_of(current, kind) > limit:
            out.append(f"the measure's {kind} limit {show(limit_of(current, kind))} is past the baseline's {show(limit)}")
        base = of_kind(baseline, kind, production)
        for key, value in sorted(of_kind(current, kind, production).items()):
            if key in base:
                if value > base[key]:
                    out.append(f"{kind} {key}: {show(value)}, past its baseline {show(base[key])}")
            elif value > limit:
                out.append(f"{kind} {key}: {show(value)}, past the limit {show(limit)} (no baseline row)")
    return out


def growth(kinds: Tuple[str, ...], old: Rows, new: Rows, production) -> List[str]:
    """How the baseline grew in `kinds` from `old` to `new`: a limit raised, a row added past the old limit or raised."""
    out = []
    for kind in kinds:
        old_limit = old.get(("limit", kind))
        new_limit = limit_of(new, kind)
        if old_limit is not None and new_limit > old_limit:
            out.append(f"limit {kind} raised {show(old_limit)} -> {show(new_limit)}")
        allowed = old_limit if old_limit is not None else new_limit
        before = of_kind(old, kind, production)
        for key, value in sorted(of_kind(new, kind, production).items()):
            if value > before.get(key, allowed):
                was = show(before[key]) if key in before else f"<={show(allowed)}"
                out.append(f"{kind} {key} raised {was} -> {show(value)}")
    return out


def falls(current: Rows, baseline: Rows, production) -> List[str]:
    out = []
    for kind in KINDS:
        now = of_kind(current, kind, production)
        for key, value in sorted(of_kind(baseline, kind, production).items()):
            if now.get(key, 0) < value:
                out.append(f"{kind} {key} {show(value)} -> {show(now[key]) if key in now else 'within the limit'}")
    return out


def check(current: Rows, ranges: List[str], production) -> bool:
    with open(BASELINE) as f:
        baseline = parse(f.read())
    ok = True
    found = violations(current, baseline, production)
    if found:
        ok = False
        print(f"JS/TS QUALITY GREW past {BASELINE}:")
        print("\n".join(f"  - {v}" for v in found))
        print(f"  {HINT}")
    for rng in ranges:
        old_text = trailers.text_at(rng.split("..")[0], BASELINE)
        if not old_text:
            continue
        for trailer, kinds in TRAILERS.items():
            def rose(a: str, b: str, kinds=kinds) -> List[str]:
                return growth(kinds, parse(a), parse(b), production)
            if not growth(kinds, parse(old_text), baseline, production):
                continue
            unjustified = trailers.unjustified_rises(rng, BASELINE, trailer, rose)
            if unjustified:
                ok = False
                print(f"THE JS/TS QUALITY BASELINE GREW in {rng} ({', '.join(kinds)}) with no `{trailer}: <why>` trailer"
                      " on the commit changing it:")
                print("\n".join(f"  - {u}" for u in unjustified))
                for note in trailers.misplaced_notes(rng, trailer, BASELINE):
                    print(f"  {note}")
    down = falls(current, baseline, production)
    if down:
        print(f"js quality fell ({len(down)} rows below {BASELINE}, not yet recorded: scripts/js-quality-report --update"
              " records them, no trailer needed):")
        print("\n".join(f"  - {d}" for d in down[:20]))
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")
    if ok:
        held = sum(len(of_kind(current, kind, production)) for kind in KINDS)
        print(f"js quality: within {BASELINE} ({held} production rows past a limit; tests and scripts reported, not held)")
    return ok


def update(current: Rows, production) -> None:
    lines = [HEADER] + [f"limit\t{kind}\t{show(limit_of(current, kind))}" for kind in KINDS]
    for kind in KINDS:
        lines += [f"{kind}\t{key}\t{show(value)}" for key, value in sorted(of_kind(current, kind, production).items())]
    with open(BASELINE, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"js quality: wrote {BASELINE} -- a rise is committed with its kind's trailer")


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description="The JS/TS quality ratchet (scripts/js-quality-report --check).")
    parser.add_argument("command", choices=("check", "update"))
    parser.add_argument("--current", required=True)
    parser.add_argument("--range", action="append", default=[])
    parser.add_argument("--areas", default=AREAS)
    args = parser.parse_args(argv)
    try:
        with open(args.current) as f:
            current = parse(f.read())
        production = production_paths(args.areas)
        if args.command == "update":
            update(current, production)
            return 0
        return 0 if check(current, args.range, production) else 1
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"js_quality_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
