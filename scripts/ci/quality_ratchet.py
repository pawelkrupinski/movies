#!/usr/bin/env python3
# Purpose: The code-quality ratchets (the pre-push sbt check's quality step)
# THE CODE-QUALITY RATCHETS (the pre-push sbt check's quality step; CLAUDE.md, "Code quality ratchets"): the production
# code's measures (`quality/runMain quality.QualityReport`, scripts/quality-report) may not grow past their checked-in
# baselines -- the coupling ratchet's model (scripts/ci/coupling_ratchet.py) over other measures. Each ratchet holds
# some row kinds to its own baseline, under its own trailer:
#
#   complexity   cognitive / cyclomatic / inner-defs of a method    test/resources/complexity-baseline.tsv   Complexity:
#   length       method-lines of a method, file-lines of a source   test/resources/length-baseline.tsv       Length:
#   duplication  copies of the clone groups over a set of files     test/resources/duplication-baseline.tsv  Duplication:
#                (and clone-tokens, a limit only: the shortest clone counted)
#
# THE DATA TABLES (test/resources/quality-data-tables.tsv; quality.DataTables): the hand-written tables kept as Scala
# literals that the length ratchet does not hold. The measure writes `data-table<TAB>key<TAB>lines` for each declared
# table it read as data (a member val of an object of literals, constructors and collections: no def, branch, lambda or
# block) and `not-data-table<TAB>key<TAB>line` for one it did not (the line logic sits on; 0: no such member). Every
# declared table must be read as data, every row's why is linted (as the coupling exemptions': specific, no generic
# phrase) with its `added` date, and a row added lands only in a commit carrying `Length: <why>`.
#
# A row is `kind<TAB>key<TAB>value`, `limit<TAB>kind<TAB>value` a kind's limit; `#` lines are comments. The measure
# writes a row for each key past its kind's limit. A key with a baseline row may not rise past it; a key without one
# fails (a NEW method over a limit) -- unless it is a MOVE: a baseline row gone from the measure whose source git's
# rename detection (`git diff -M`, content similarity) followed to this key's source, the method or class name unchanged,
# at or above it (a source moved to another package re-keys its rows; the move carries the value over and may lower it,
# never raise it -- a higher moved row is a rise, and a same-named source git does not call a rename is no move). The
# measure's limit may not rise past the baseline's. The baseline itself may grow
# (a row added or raised, a limit raised) only in a commit changing it that carries the ratchet's trailer
# (`Complexity: <why>`, `Length: <why>`, `Duplication: <why>`) -- the commit whose own step raises it: in a batch push,
# one change's trailer never excuses another's rise (trailers.py, THE ATTRIBUTION RULE). A fall needs none: `update`
# writes the measured rows, every fall included.
#
#   quality_ratchet.py check --current <rows> [--ratchet NAME ...] [--range A..B ...]
#       violations: measured rows past a baseline (exit 1), and, for each range, a baseline grown between A and B by a
#       commit of the range not carrying its trailer (exit 1). Falls not yet recorded are said, and pass.
#   quality_ratchet.py update --current <rows> [--ratchet NAME ...]
#       each ratchet's baseline rewritten from the measured rows (a rise is then committed with its trailer).
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import Dict, List, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # baseline_ratchet.py and trailers.py beside it, under `python3 -I` too
import baseline_ratchet  # noqa: E402
import trailers  # noqa: E402
from trailers import text_at as file_at  # noqa: E402
from coupling_reasons import GENERIC_REASONS, ISO_DATE  # noqa: E402

Rows = Dict[Tuple[str, str], int]


@dataclass(frozen=True)
class Ratchet:
    name: str
    baseline: str
    trailer: str
    kinds: Tuple[str, ...]
    header: str
    hint: str


RATCHETS: Dict[str, Ratchet] = {r.name: r for r in (
    Ratchet("complexity", "test/resources/complexity-baseline.tsv", "Complexity", ("cognitive", "cyclomatic", "inner-defs"),
            """# THE COMPLEXITY BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every production method (*/src/main of a deployed module, quality.MethodMeasures) past a limit, frozen at its value:
#   limit       cognitive / cyclomatic / inner-defs: a method without a row stays at or below it
#   cognitive   <path>:<Owner.method>, its cognitive complexity (SonarSource's, nesting increments included)
#   cyclomatic  <path>:<Owner.method>, its cyclomatic complexity (McCabe)
#   inner-defs  <path>:<Owner.method>, the defs nested in it (CLAUDE.md: refactor past two inner methods)
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying `Complexity: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Split the method (extract the branches, name the inner defs as methods), or record it: scripts/quality-report"
            " --update, committed with a `Complexity: <why>` trailer."),
    Ratchet("duplication", "test/resources/duplication-baseline.tsv", "Duplication", ("copies", "clone-tokens"),
            """# THE DUPLICATION BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every clone group in the production sources (*/src/main of a deployed module; PMD CPD, `package`/`import` lines blanked),
# frozen:
#   limit   copies 0: a file set without a row holds no clone; clone-tokens: the shortest token run counted as a clone
#           (raising it hides clones, so it is held like any limit)
#   copies  <path>|<path>..., the files a clone group spans (sorted), and the copies of every group spanning exactly
#           them: a new group adds a row or raises one, a group gaining a copy too; an edit moving a clone's lines
#           leaves its row
# A row may only fall (--update records it, no trailer); a row added or raised lands only in a commit carrying
# `Duplication: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Extract the copied block (CLAUDE.md: extract at two uses; scripts/quality-report lists every group and its"
            " lines), or record it: scripts/quality-report --update, committed with a `Duplication: <why>` trailer."),
    Ratchet("length", "test/resources/length-baseline.tsv", "Length", ("method-lines", "file-lines"),
            """# THE LENGTH BASELINE (scripts/quality-report --update writes it; scripts/ci/quality_ratchet.py holds every push to it).
# Every production method and source (*/src/main of a deployed module) past its length limit, frozen at its length:
#   limit         method-lines / file-lines: a method or source without a row stays at or below it
#   method-lines  <path>:<Owner.method>, the lines from its `def` (or member `val`/`var`/given alias) to its body's end
#   file-lines    <path>, the source's lines
# A row may only fall (--update records it, no trailer); a row added or raised, or a limit raised, lands only in a
# commit carrying `Length: <why>` (CLAUDE.md, "Code quality ratchets").""",
            "Split the method or the source (a class, an object, a file of its own), or record it: scripts/quality-report"
            " --update, committed with a `Length: <why>` trailer."),
)}


DATA_TABLES = "test/resources/quality-data-tables.tsv"
DATA_TABLE_TRAILER = "Length"
TABLE_REASON_MIN_CHARS, TABLE_REASON_MIN_WORDS = 40, 6
TABLE_REASON_SHAPE = ("`<what the table holds>: <what reads it, and why it is hand-written>`, e.g. `per-host HTTP policies:"
                      " each host's pace and timeouts, with the incident behind each value beside it`")


def declared_tables(text: str) -> Tuple[Dict[str, List[str]], List[str]]:
    """The data-table file's rows, key -> columns, and what is wrong with each malformed one (its columns, its `added`
    date, its why)."""
    rows: Dict[str, List[str]] = {}
    problems = []
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        cols = line.split("\t")
        where = f"{DATA_TABLES}:{n}"
        if len(cols) != 4 or cols[0] != "table" or ":" not in cols[1]:
            problems.append(f"{where}: not `table<TAB><path>:<Owner.member><TAB>added<TAB>why`: {line!r}")
            continue
        if cols[1] in rows:
            problems.append(f"{where}: {cols[1]} declared twice")
        if not ISO_DATE.match(cols[2]):
            problems.append(f"{where}: no `added` date (the third column, YYYY-MM-DD: the day it is added)")
        problems += [f"{where}: {cols[1]}'s why: {p}" for p in table_reason_problems(cols[1], cols[3])]
        rows[cols[1]] = cols
    return rows, problems


def table_reason_problems(key: str, why: str) -> List[str]:
    """Why `why` is no SPECIFIC reason for a data table: it leads with what the table holds, says 40+ characters and 6+
    words besides the member's name, and no generic phrase."""
    text = why.strip()
    out = []
    held, sep, _ = text.partition(":")
    if not sep or not held.strip():
        out.append(f"lead with what the table holds: {TABLE_REASON_SHAPE}")
    member = key.rpartition(":")[2]
    rest = text
    for name in sorted({member, member.rsplit(".", 1)[-1]}, key=len, reverse=True):
        rest = rest.replace(name, " ")
    if len(rest.strip()) < TABLE_REASON_MIN_CHARS or len(rest.split()) < TABLE_REASON_MIN_WORDS:
        out.append(f"too short: {TABLE_REASON_MIN_CHARS}+ characters and {TABLE_REASON_MIN_WORDS}+ words besides the member's name")
    lowered = text.lower()
    out += [f"`{phrase}` is generic: say what the table holds and what reads it" for rx, phrase in GENERIC_REASONS
            if re.search(rx, lowered)]
    return out


def table_violations(current: Rows, declared: Dict[str, List[str]]) -> List[str]:
    """Each declared table the measure did not read as data, one sentence each."""
    data, not_data = of_kind(current, "data-table"), of_kind(current, "not-data-table")
    out = []
    for key in sorted(declared):
        if key in data:
            continue
        line = not_data.get(key, 0)
        out.append(f"{key}: line {line} holds a def, branch, lambda or block -- logic, not data" if line
                   else f"{key}: no member val of an object by that name")
    return out


def tables_added(old: str, new: str) -> List[str]:
    """The data tables `new` declares that `old` does not: each loosens the length ratchet."""
    before = declared_tables(old)[0]
    return [f"data table {key} added" for key in sorted(declared_tables(new)[0]) if key not in before]


def check_tables(current: Rows, ranges: List[str]) -> bool:
    """The data tables held: each row well formed, each table read as data, each row added under its trailer."""
    text = open(DATA_TABLES).read() if os.path.exists(DATA_TABLES) else ""
    declared, problems = declared_tables(text)
    problems += table_violations(current, declared)
    for rng in ranges:
        old = file_at(rng.split("..")[0], DATA_TABLES)
        added = tables_added(old, text)
        refused = trailers.unjustified_rises(rng, DATA_TABLES, DATA_TABLE_TRAILER, tables_added) if added else []
        if refused:
            problems.append(f"{'; '.join(added)} in {rng}, with no `{DATA_TABLE_TRAILER}: <why>` trailer on the commit"
                            f" adding it: {'; '.join(refused)}")
    if problems:
        print(f"THE DATA TABLES of {DATA_TABLES} do not hold:")
        for p in problems:
            print(f"  - {p}")
        print("  A table is a member val of an object built only of literals, names, constructors and collections: move"
              " the logic out of it (a val or def of its own beside it), or drop its row and split it.")
        return False
    print(f"data tables: {len(declared)} declared, every one read as data")
    return True


def parse(text: str) -> Rows:
    return baseline_ratchet.parse_rows(text, int)


def of_kind(rows: Rows, kind: str) -> Dict[str, int]:
    return {key: value for (k, key), value in rows.items() if k == kind}


def limit_of(rows: Rows, kind: str) -> int:
    limits = of_kind(rows, "limit")
    if kind not in limits:
        raise ValueError(f"no `limit<TAB>{kind}<TAB>N` row")
    return limits[kind]


# Renames, moved_key and moved_from -- git's rename detection followed to a key's source -- are baseline_ratchet's,
# which the Python and JS/TS ratchets share.
Renames, moved_key, moved_from = baseline_ratchet.Renames, baseline_ratchet.moved_key, baseline_ratchet.moved_from


def violations(ratchet: Ratchet, current: Rows, baseline: Rows, renames: Renames) -> List[str]:
    """The measured rows past the baseline, one sentence each."""
    out = []
    for kind in ratchet.kinds:
        limit = limit_of(baseline, kind)
        if limit_of(current, kind) > limit:
            out.append(f"the measure's {kind} limit {limit_of(current, kind)} is past the baseline's {limit}")
        base = of_kind(baseline, kind)
        for key, value in sorted(of_kind(current, kind).items()):
            if key in base:
                if value > base[key]:
                    out.append(f"{kind} {key}: {value}, past its baseline {base[key]}")
            elif value > limit and not moved_from(key, value, base, of_kind(current, kind), renames):
                out.append(f"{kind} {key}: {value}, past the limit {limit} (no baseline row)")
    return out


def growth(ratchet: Ratchet, old: Rows, new: Rows, renames: Renames) -> List[str]:
    """How the baseline grew from `old` to `new`: a limit raised, a row added past the old limit or raised."""
    out = []
    for kind in ratchet.kinds:
        old_limit = of_kind(old, "limit").get(kind)
        new_limit = limit_of(new, kind)
        if old_limit is not None and new_limit > old_limit:
            out.append(f"limit {kind} raised {old_limit} -> {new_limit}")
        allowed_new = old_limit if old_limit is not None else new_limit
        before = of_kind(old, kind)
        after = of_kind(new, kind)
        for key, value in sorted(after.items()):
            if value > before.get(key, allowed_new) and not (key not in before and moved_from(key, value, before, after, renames)):
                out.append(f"{kind} {key} raised {before.get(key, f'<={allowed_new}')} -> {value}")
    return out


def falls(ratchet: Ratchet, current: Rows, baseline: Rows) -> List[str]:
    """The baseline rows the measured code is below (or no longer past the limit): falls `update` would record."""
    out = []
    for kind in ratchet.kinds:
        now = of_kind(current, kind)
        for key, value in sorted(of_kind(baseline, kind).items()):
            if now.get(key, 0) < value:
                out.append(f"{kind} {key} {value} -> {now.get(key, 'within the limit')}")
    return out


def check_one(ratchet: Ratchet, current: Rows, ranges: List[str], renames: Renames) -> bool:
    with open(ratchet.baseline) as f:
        baseline = parse(f.read())
    failed = False
    found = violations(ratchet, current, baseline, renames)
    if found:
        failed = True
        print(f"{ratchet.name.upper()} GREW past {ratchet.baseline}:")
        for v in found:
            print(f"  - {v}")
        print(f"  {ratchet.hint}")
        print("  (A moved source keeps its rows only when git sees a rename -- `git mv`, the content mostly unchanged --"
              " and no value rises in the move.)")
    for rng in ranges:
        old = file_at(rng.split("..")[0], ratchet.baseline)
        grown = growth(ratchet, parse(old), baseline, renames) if old else []
        unjustified = trailers.unjustified_rises(rng, ratchet.baseline, ratchet.trailer, lambda a, b: growth(
            ratchet, parse(a), parse(b), renames)) if grown else []
        if unjustified:
            failed = True
            print(f"THE {ratchet.name.upper()} BASELINE GREW in {rng} with no `{ratchet.trailer}: <why>` trailer on the"
                  " commit changing it:")
            for g in grown:
                print(f"  - {g}")
            print(f"  Each rise needs the trailer on the commit making it, not on another commit of the push; without one:")
            for u in unjustified:
                print(f"  - {u}")
            trailers.print_misplaced(rng, ratchet.trailer, ratchet.baseline)
    down = falls(ratchet, current, baseline)
    if down:
        print(f"{ratchet.name} fell ({len(down)} rows below {ratchet.baseline}, not yet recorded: scripts/quality-report"
              " --update records them, no trailer needed):")
        for d in down[:20]:
            print(f"  - {d}")
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")
    if not failed:
        rows = sum(len(of_kind(current, kind)) for kind in ratchet.kinds)
        print(f"{ratchet.name}: within {ratchet.baseline} ({rows} rows past a limit)")
    return not failed


def update_one(ratchet: Ratchet, current: Rows) -> None:
    lines = [ratchet.header]
    lines += [f"limit\t{kind}\t{limit_of(current, kind)}" for kind in ratchet.kinds]
    for kind in ratchet.kinds:
        lines += [f"{kind}\t{key}\t{value}" for key, value in sorted(of_kind(current, kind).items())]
    with open(ratchet.baseline, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"{ratchet.name}: wrote {ratchet.baseline} -- a rise is committed with a `{ratchet.trailer}: <why>` trailer")


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    for name in ("check", "update"):
        c = sub.add_parser(name)
        c.add_argument("--current", required=True)
        c.add_argument("--ratchet", action="append", choices=sorted(RATCHETS), default=[])
        if name == "check":
            c.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    try:
        with open(args.current) as f:
            current = parse(f.read())
        chosen = [RATCHETS[name] for name in (args.ratchet or sorted(RATCHETS))]
        if args.command == "update":
            for ratchet in chosen:
                update_one(ratchet, current)
            return 0
        renames = Renames()
        results = [check_one(ratchet, current, args.range, renames) for ratchet in chosen]
        if RATCHETS["length"] in chosen:
            results.append(check_tables(current, args.range))
        return 0 if all(results) else 1
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"quality_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
