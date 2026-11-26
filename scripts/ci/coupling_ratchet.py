#!/usr/bin/env python3
# Purpose: The coupling ratchet (the pre-push sbt check's coupling step)
# THE COUPLING RATCHET (the pre-push sbt check's coupling step; CLAUDE.md, Design rules): the production code's coupling,
# measured from Zinc's class-dependency record and one source file's class files (`sbt couplingReport`,
# project/Coupling.scala), has NO package cycle --
# any `cycle-edge` row fails, measured or recorded in the baseline, and no `Coupling:` trailer excuses one -- and may
# not grow past the checked-in baseline (test/resources/coupling-baseline.tsv): no new cross-module edge kind, no
# package's Ce (the outside packages its classes use) past its row (or past the new-package limit), no class's fan-out
# past its row (or, without a row, to the limit), no package edge into a higher layer or, inside an ordered layer, into
# a higher rank (the layering, test/resources/coupling-layers.tsv: a `layer-violation` or `rank-violation` row fails
# whatever the baseline says, naming the direction allowed), no package without its layer or rank -- and the baseline
# itself may grow, and the layer table loosen (an `allow` row added, a package given another layer or rank: either lets
# an edge through that failed before), only in a commit changing that file carrying `Coupling: <why>` -- the commit whose
# own step grows or loosens it: in a batch push, one change's trailer never excuses another's (trailers.py, THE
# ATTRIBUTION RULE). A fall needs no trailer: `scripts/coupling-report --update` writes the measured rows, every fall
# included; a new package's row, or a row dropped, loosens nothing.
#
# THE LIMITS: a class's fan-out (`limit fanout`, its collaborators) and a package's distance D
# (`limit distance` in hundredths, packages of `limit distance-min-types` types or more) are FROZEN where they were past
# the limit when the limits came in -- a `fanout` / `distance` row may only fall -- and a class or package NEWLY past a
# limit fails: no `--update` and no trailer admits it, only an exemption row (test/resources/coupling-exemptions.tsv),
# and the exemption table may grow or loosen only in a commit changing it
# carrying `Coupling: <why>` -- and a fan-out or package row added (an agent's exemption of a new class, the owner
# 2026-11-19, or of one package's distance, 2026-11-20) only with a SPECIFIC why (its role and what it needs:
# reason_problems), its `added` date, and the adding commit's own `Coupling: exempt <Class|package> — <reason>` trailer
# (the exemption table's header, AN AGENT'S ROW ADDED).
# A frozen class that moves keeps its row under its new name (the same simple name, at or below the old value, the old
# name gone).
#
# A RENAME (the owner, 2026-11-19, "rename credit"; the quality ratchet's moves, quality_ratchet.py, the model): a frozen
# fan-out row carries over to the class's NEW name -- the old row gone, the new name's row at or below it -- when the
# range renamed it: git's rename detection (`git diff -M` over the range) renames the old name's source to the new
# name's, or scripts/pure-move-check calls a commit of the range touching the old source a PURE MOVE, the new name
# declared after it and the old no longer. A frozen distance row carries over to a package's new name the same way:
# every source of the old package git (or a PURE commit) renames lands in the new one, and its ceiling goes with it. No
# trailer is needed (the rename is git's to see, not the author's to claim). Anything else is no rename: a value past
# the old row, one old name going to several new ones, or another new class taking over the old one's code (a split
# posing as a rename: the split rule's, `Coupling: split of <X>`). A `split of <X> into <A>` claim of X's one rename is
# read as the rename.
#
# A measurement fix (`limit fanout-measure` raised: a count that missed real uses, or charged a class with
# another's; `limit distance-measure` raised: a package measured otherwise, 2 = a package of three or more segments with
# a layer-table row of its own measured apart from its parent, 2026-11-20) re-takes the rows at their true values, as a
# changed limit does, under its `Coupling:` trailer. A distance re-measure keeps every ceiling: a row it takes again
# above its ceiling leaves that package no split headroom until it falls back under.
#
# A SPLIT (the owner, 2026-11-18; the union rule, 2026-11-18): a frozen class X cut into pieces may freeze the pieces
# past the limit at their new values when a commit of the range carries `Coupling: split of <X>` (`... into <A>, <B>`
# names the pieces; without it they are the fanout rows the range adds) and ALL hold: X had a frozen row at the range's
# base; every piece takes over X's code (git sees its source file as a rename or copy of X's, or it defines 2+ members
# X's file no longer does); every piece's fan-out is strictly below X's frozen value; and the UNION of what the pieces
# and X's remainder (if X still exists) reference -- folded as the count folds it, their references to each other and to
# X internal to the split -- lies within X's frozen reference set at the base (test/resources/coupling-fanout-refs.tsv,
# which `--update` writes beside the baseline, as many entries as X's row): the
# split adds no dependency, though the pieces' counts may sum past X's when they share its types. The pieces' rows then
# fall only, like any frozen row. In that range the frozen distance row of a package holding X or a piece, or holding a
# class the split's references reach, may rise by DISTANCE_SPLIT_TOLERANCE hundredths at most (moving types shifts a
# package's abstractness; each piece using a package is one more outside class in its Ca), re-frozen at the new value;
# outside a valid split a distance row never rises.
#
# THE CEILING (the owner, 2026-11-19): that +0.02 is no ratchet upward. Each frozen distance row has a fixed ceiling, a
# `distance-cap` row: its value on 2026-11-19 + 0.02 (a row without one is read at its own value + DISTANCE_SPLIT_TOLERANCE).
# A split may raise D only while it stays at or under the ceiling; a fall lowers the ceiling with the row (min(ceiling,
# row + 0.02)), so it only tightens, and no trailer raises it (a changed distance limit aside, which re-takes the rows).
# `update` carries it -- over a conflicted baseline (agentctl's regen of a generated file both sides changed) the
# minimum of every side's. `--fanouts` (couplingReport's fourth output, every class's held
# fan-out and what it counts) gives each piece's and the remainder's value and references.
#
#   coupling_ratchet.py check --current <rows> --baseline <file> [--layers <file>] [--edges <file>] [--exemptions <file>]
#                             [--fanouts <file>] [--collaborators <file>] [--range A..B ...]
#       violations: a package cycle edge and the measured rows past the baseline (exit 1), and, for each range, a
#       baseline grown or a layer table loosened between A and B by a commit of the range not carrying `Coupling:`
#       (exit 1). Falls not yet recorded are said, and pass. `--edges` (couplingReport's third output, every
#       class reference with its package edge) names the class references behind each refused edge.
#   coupling_ratchet.py split-check --current <rows> --fanouts <file> --split <X> --into <A> [--into <B> ...] [--base REV]
#       whether a split of X into those pieces, in the WORKING TREE, would pass: the same split_credit and
#       frozen_growth `check` runs, over the range from REV (default: the merge base with origin/main) to a snapshot
#       commit of the working tree, the measured rows standing for the baseline `--update` would write. Says each
#       piece's fan-out against X's row, the union of their references against X's at REV, each distance row the split
#       may move, and the trailer to commit it under; exit 0 accepted, 1 refused (scripts/coupling-report --check-split).
#   coupling_ratchet.py update --current <rows> [--baseline <file>]
#       writes the baseline: the measured rows, each frozen distance row followed by its ceiling (THE CEILING) -- the
#       least of the file's own, of every side of it git holds while it is unmerged, and the measured row + 0.02.
#       What scripts/coupling-report --update writes.
#   coupling_ratchet.py new-exemptions [--since 7d] [--rev REV]
#       the exemption rows added since then on REV (default origin/main), each with its date, commit, why and trailer,
#       and whether it is still there: the weekly list the owner strikes from (scripts/coupling-report --new-exemptions).
#   coupling_ratchet.py refs --current <rows> --fanouts <file>
#       each frozen class's reference set (its `fanout` row's targets), `<class><TAB><target>` a line: what
#       scripts/coupling-report --update writes to test/resources/coupling-fanout-refs.tsv.
#
# THE DATA LIMITS (the owner, 2026-11-26: "protection against logic / complexity being hidden in data types"): a DATA type
# counts in no fan-out and no abstractness, so three limits keep logic from moving into one. (1) A data type holding
# more logic than a data type may (Coupling.Collaborators.withLogic: its non-trivial methods, its decisions) is a
# collaborator, measured, so no row. (2) `databag` <type>: the other types (any kind) a data type references past the
# `limit databag`; (3) `datafanout` <class>: the DATA types a collaborator class uses past the `limit datafanout`. Both are
# FROZEN kinds, like `fanout`: a row only falls, a new type or class past the limit fails, and -- unlike a fan-out --
# no exemption admits one (cut it: split the type, or hand its shapes on as one value).
#
# THE CLASSIFICATION (same date): test/resources/coupling-collaborators.tsv records every top-level type's verdict, `data` or
# `collaborator` (`update` writes it from couplingReport's collaborators and fanouts). A type measured as data that the
# file has as a collaborator is refused until the file records it, and the commit recording it carries `Coupling: <why it
# is data>` (the attribution rule): moving a type into the data bucket is never silent. A flip to a collaborator is said.
# A type absent from the file is new, and a stale row fails nothing.
#
# A row is `kind<TAB>key<TAB>value`; `#` lines are comments (project/Coupling.scala's BaselineHeader has the kinds).from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from typing import Callable, Dict, List, Optional, Set, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(HERE, "..", "lib")]  # baseline_ratchet.py and trailers.py beside it, tool_python in lib
import trailers  # noqa: E402
from coupling_exemption_history import new_exemptions, unadmitted_exemptions  # noqa: E402
from coupling_exemption_table import exemption_loosening, parse_exemptions  # noqa: E402
from coupling_growth import DISTANCE_CUT, falls, frozen_growth, growth, loosening  # noqa: E402
from coupling_reasons import REASON_SHAPE  # noqa: E402
from coupling_renames import RenameEvidence, rename_credit  # noqa: E402
from coupling_rows import (BASELINE_PATH, DISTANCE_SPLIT_TOLERANCE, EXEMPTIONS_PATH, FROZEN, LAYERS_PATH,  # noqa: E402
                           REFS_PATH, Edges, Fanouts, Rows, ceilings, layer_units, of_kind, package_of, parse,
                           parse_layers, read_edges, read_fanouts, update, with_ceilings)
from coupling_splits import SPLIT_RULE, SplitExplanation, resolve, split_claims, split_credit  # noqa: E402
from coupling_verdicts import VERDICTS_PATH, flips, measured_verdicts, parse_verdicts, verdict_file  # noqa: E402
from coupling_violations import violations  # noqa: E402
from trailers import git, text_at as file_at  # noqa: E402

Renamed = Dict[Tuple[str, str], str]


def refs(current_path: str, fanouts_path: str) -> int:
    """Each frozen class's reference set, `<class><TAB><target>` a line, sorted (the module docstring's `refs`)."""
    with open(current_path) as f:
        frozen = of_kind(parse(f.read()), "fanout")
    fanouts = read_fanouts(fanouts_path)
    targets = fanouts[1] if fanouts else {}
    for cls in sorted(frozen):
        for target in sorted(targets.get(cls, ())):
            print(f"{cls}\t{target}")
    return 0


def renames_of(ranges: List[str], current: Rows, baseline: Rows) -> Tuple[Renamed, List[str], List[str]]:
    """The (kind, key) rows each range's renames carry over (A RENAME), from the base's baseline to the measured rows and
    to the recorded baseline, with the notes and refusals to say."""
    renamed: Renamed = {}
    notes: List[str] = []
    refused: List[str] = []
    for rng in ranges:
        before = parse(file_at(rng.split("..")[0], BASELINE_PATH))
        evidence = RenameEvidence(rng)
        for kind, _, _ in FROZEN:
            for after in (current, baseline):
                credited, n, r = rename_credit(kind, of_kind(before, kind), of_kind(after, kind), evidence)
                renamed.update({(kind, key): old for key, old in credited.items()})
                notes += [x for x in n if x not in notes]
                refused += [x for x in r if x not in refused]
    return renamed, notes, refused


def is_rename(name: str, named: List[str], before: Rows, renamed: Renamed) -> bool:
    """Whether a `Coupling: split of <X> into <A>` claim is a rename the range already credits: X renamed to its one
    piece (a trailer written before rename credit, 2026-11-19, to get the row across)."""
    original = resolve(name, of_kind(before, "fanout"))
    targets = {new for (kind, new), old in renamed.items() if kind == "fanout" and old == original}
    return bool(targets) and len(named) == 1 and resolve(named[0], targets) is not None


def unjustified(rng: str, path: str, rose: Callable[[str, str], List[str]]) -> List[str]:
    """The commits of `rng` whose own step grows `path` (`rose` over its parsed texts) with no `Coupling:` trailer
    (trailers.py, THE ATTRIBUTION RULE)."""
    return trailers.unjustified_rises(rng, path, "Coupling", rose)


def rise_refused(rng: str, path: str, header: str, rises: List[str], rose: Callable[[str, str], List[str]]) -> bool:
    """Whether a commit of `rng` makes one of `rises` (what `path` gained) without its own `Coupling:` trailer: said, with
    the rises and the commits."""
    found = unjustified(rng, path, rose) if rises else []
    if not found:
        return False
    print(header)
    for item in rises:
        print(f"  - {item}")
    print("  Each rise needs the trailer on the commit making it, not on another commit of the push; without one:")
    for u in found:
        print(f"  - {u}")
    trailers.print_misplaced(rng, "Coupling", path)
    return True


def file_text(path: str) -> str:
    with open(path) as f:
        return f.read()


def recorded_cycles(baseline: Rows, baseline_path: str) -> bool:
    """Whether the baseline itself records a package cycle: said, with each edge."""
    recorded = sorted(of_kind(baseline, "cycle-edge"))
    if recorded:
        print(f"THE COUPLING BASELINE RECORDS A PACKAGE CYCLE ({baseline_path}): the package graph has none, and no "
              "`Coupling:` trailer lets one in:")
        for key in recorded:
            print(f"  - cycle-edge {key}")
    return bool(recorded)


def say_violations(found: List[str], baseline_path: str) -> bool:
    if found:
        print(f"COUPLING REFUSED (a package cycle, a layer or rank violation, or a row past {baseline_path}):")
        for v in found:
            print(f"  - {v}")
        print("  Cut the new dependency (scripts/coupling-report shows the class references behind each edge). Only a row past")
        print("  the baseline -- never a cycle, a layer or a rank violation -- may be recorded instead: scripts/coupling-report")
        print("  --update, committed with a `Coupling: <why this coupling is right>` trailer.")
    return bool(found)


def check_splits(rng: str, before: Rows, baseline: Rows, fanouts: Optional[Fanouts],
                 renamed: Renamed) -> Tuple[bool, Set[str], Set[str]]:
    """The split claims of `rng` (a rename's claim aside), said: whether any is refused, the pieces and packages credited."""
    claims = [(name, named) for name, named in split_claims(rng) if not is_rename(name, named, before, renamed)]
    split, split_packages, notes, refused = split_credit(rng, before, baseline, fanouts, claims)
    for note in notes:
        print(f"coupling: {note} -- each piece frozen at its own value")
    if refused:
        print(f"A `Coupling: split of <X>` CLAIM IS REFUSED in {rng}: the pieces must take over X's code, each below X's"
              " frozen value, and reference (with what X keeps) nothing X did not:")
        for r in refused:
            print(f"  - {r}")
        print(SPLIT_RULE)
    return bool(refused), split, split_packages


def check_frozen(rng: str, before: Rows, baseline: Rows, split: Set[str], split_packages: Set[str], renamed: Renamed) -> bool:
    frozen = frozen_growth(before, baseline, split, split_packages, renamed)
    if frozen:
        print(f"THE COUPLING BASELINE ADMITS A CLASS OR PACKAGE PAST A LIMIT in {rng}: a frozen row only falls, and no"
              " trailer admits a new one -- cut it, or exempt it with its why in " + EXEMPTIONS_PATH + ":")
        for g in frozen:
            print(f"  - {g}")
        trailers.print_misplaced(rng, "Coupling")
    return bool(frozen)


def check_range(rng: str, baseline: Rows, fanouts: Optional[Fanouts], renamed: Renamed, layers_path: str) -> bool:
    """One pushed range against the baseline: its split claims, the frozen rows, the baseline's growth and the layer
    table's loosening; whether any is refused."""
    base = rng.split("..")[0]
    before = parse(file_at(base, BASELINE_PATH))
    failed, split, split_packages = check_splits(rng, before, baseline, fanouts, renamed)
    failed |= check_frozen(rng, before, baseline, split, split_packages, renamed)
    failed |= rise_refused(rng, BASELINE_PATH, f"THE COUPLING BASELINE GREW in {rng} with no `Coupling: <why>` trailer on the"
                           " commit changing it:", growth(before, baseline), lambda a, b: growth(parse(a), parse(b)))
    if os.path.exists(layers_path):
        loosened = loosening(parse_layers(file_at(base, LAYERS_PATH)), parse_layers(file_text(layers_path)))
        failed |= rise_refused(rng, LAYERS_PATH, f"THE LAYER TABLE LOOSENED in {rng} ({layers_path}) with no `Coupling: <why>`"
                               " trailer on the commit changing it:", loosened,
                               lambda a, b: loosening(parse_layers(a), parse_layers(b)))
    return failed


def loosening_refused(rng: str, exemptions_path: str) -> bool:
    """Whether a commit of `rng` loosens the exemption table without its own `Coupling:` trailer."""
    base = rng.split("..")[0]
    loose = exemption_loosening(parse_exemptions(file_at(base, EXEMPTIONS_PATH)), parse_exemptions(file_text(exemptions_path)))
    return rise_refused(rng, EXEMPTIONS_PATH, f"THE EXEMPTION TABLE LOOSENED in {rng} ({exemptions_path}) with no"
                        " `Coupling: <why>` trailer on the commit changing it:", loose,
                        lambda a, b: exemption_loosening(parse_exemptions(a), parse_exemptions(b)))


def unadmitted_refused(rng: str, exemptions_path: str) -> bool:
    """Whether a commit of `rng` adds an exemption row that nothing admits."""
    unadmitted = unadmitted_exemptions(rng)
    if unadmitted:
        print(f"AN EXEMPTION ROW IS NOT ADMITTED in {rng} ({exemptions_path}): a row added carries a specific why, its"
              " `added` date and the commit's own `Coupling: exempt <Class> — <reason>` trailer:")
        for item in unadmitted:
            print(f"  - {item}")
        print(f"  The why: {REASON_SHAPE}.")
        trailers.print_misplaced(rng, "Coupling", EXEMPTIONS_PATH)
    return bool(unadmitted)


def check_exemptions(ranges: List[str], exemptions_path: str) -> bool:
    """The exemption table: a loosening a commit makes without its trailer, a row added that
    nothing admits; whether any is refused."""
    failed = False
    if os.path.exists(exemptions_path):
        for rng in ranges:
            failed |= loosening_refused(rng, exemptions_path)
    for rng in ranges:
        failed |= unadmitted_refused(rng, exemptions_path)
    return failed


def say_falls(current: Rows, baseline: Rows) -> None:
    down = falls(current, baseline)
    if down:
        print(f"coupling fell ({len(down)} rows below the baseline, not yet recorded: scripts/coupling-report --update"
              " records them, no trailer needed):")
        for d in down[:20]:
            print(f"  - {d}")
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")


def verdict_rises(before: str, after: str) -> List[str]:
    """The types a verdict file's text moved into the data bucket between two texts (THE CLASSIFICATION)."""
    return [f"{t} was a collaborator, is data" for t in flips(parse_verdicts(before), parse_verdicts(after))[0]]


def check_verdicts(ranges: List[str], collaborators_path: str, fanouts_path: str) -> bool:
    """THE CLASSIFICATION: the types whose measured verdict flipped from the recorded one (test/resources/
    coupling-collaborators.tsv) are said; a collaborator that is data now is refused until the file records it, and a
    commit of a range recording it without its own `Coupling:` trailer is refused too. Whether any is refused."""
    fanouts = read_fanouts(fanouts_path)
    if not collaborators_path or fanouts is None or not os.path.exists(VERDICTS_PATH):
        return False
    recorded = parse_verdicts(file_text(VERDICTS_PATH))
    to_data, to_collaborator = flips(recorded, measured_verdicts(file_text(collaborators_path), sorted(fanouts[0])))
    for t in to_collaborator:
        print(f"coupling: {t} is a collaborator now, was data: it counts in fan-out and abstractness")
    if to_data:
        print(f"A TYPE MOVED INTO THE DATA BUCKET ({VERDICTS_PATH} still has it a collaborator): it drops out of every fan-out and"
              " abstractness count, so it is a loosening -- record it with scripts/coupling-report --update and commit that"
              " with `Coupling: <why it is data, not a collaborator>`:")
        for t in to_data:
            print(f"  - {t} was a collaborator, is data")
    failed = bool(to_data)
    for rng in ranges:
        base = rng.split("..")[0]
        failed |= rise_refused(rng, VERDICTS_PATH, f"A TYPE MOVED INTO THE DATA BUCKET in {rng} with no `Coupling: <why>` trailer"
                               " on the commit recording it:", verdict_rises(file_at(base, VERDICTS_PATH), file_text(VERDICTS_PATH)),
                               verdict_rises)
    return failed


def check(current_path: str, baseline_path: str, ranges: List[str], layers_path: str = LAYERS_PATH,
          edges_path: str = "", exemptions_path: str = EXEMPTIONS_PATH, fanouts_path: str = "",
          collaborators_path: str = "") -> int:
    current, baseline = parse(file_text(current_path)), parse(file_text(baseline_path))
    ranks = parse_layers(file_text(layers_path))[2] if os.path.exists(layers_path) else {}
    fanouts = read_fanouts(fanouts_path)
    edges: Edges = read_edges(file_text(edges_path)) if edges_path else {}
    failed = recorded_cycles(baseline, baseline_path)
    renamed, rename_notes, rename_refused = renames_of(ranges, current, baseline)
    for note in rename_notes:
        print(f"coupling: {note}")
    failed |= say_violations(violations(current, baseline, ranks, edges, renamed), baseline_path)
    for r in rename_refused:
        print(f"  no rename credit: {r}")
    for rng in ranges:
        failed |= check_range(rng, baseline, fanouts, renamed, layers_path)
    failed |= check_exemptions(ranges, exemptions_path)
    failed |= check_verdicts(ranges, collaborators_path, fanouts_path)
    say_falls(current, baseline)
    if not failed:
        print(f"coupling: within {baseline_path} ({len(current)} rows)")
    return 1 if failed else 0


def update_all(current_path: str, baseline_path: str, fanouts_path: str, collaborators_path: str) -> int:
    """`update` (the module docstring): the baseline and, given the measured verdicts, the classification file."""
    code = update(current_path, baseline_path)
    fanouts = read_fanouts(fanouts_path)
    if code == 0 and collaborators_path and fanouts is not None:
        with open(VERDICTS_PATH, "w") as f:
            f.write(verdict_file(measured_verdicts(file_text(collaborators_path), sorted(fanouts[0]))))
    return code


def snapshot() -> str:
    """A commit of the working tree (tracked and untracked, .gitignore respected) on top of HEAD, built in a copy of
    the index: no ref, the index and the tree untouched."""
    index = git("rev-parse", "--git-path", "index").strip()
    with tempfile.TemporaryDirectory() as d:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(d, "index"))
        if os.path.exists(index):
            shutil.copy(index, env["GIT_INDEX_FILE"])

        def run(*a: str) -> str:
            return subprocess.run(["git", *a], check=True, capture_output=True, text=True, env=env).stdout.strip()

        run("add", "-A")
        return run("commit-tree", run("write-tree"), "-p", "HEAD", "-m", "split-check: the working tree")


def default_base() -> str:
    found = subprocess.run(["git", "merge-base", "HEAD", "origin/main"], capture_output=True, text=True)
    return found.stdout.strip() if found.returncode == 0 else git("rev-parse", "HEAD").strip()


def piece_line(piece: str, value: Optional[int], frozen: int) -> str:
    if value is None:
        return f"  piece {piece}: fan-out - -- no class {frozen}"
    return f"  piece {piece}: fan-out {value} -- {'below' if value < frozen else 'NOT below'} {frozen}"


def say_explanation(e: SplitExplanation, fanouts: Optional[Fanouts]) -> None:
    """What the judgement of one split read: X's row, each piece's fan-out against it, the union of the references."""
    print(f"  {e.original}: frozen at {e.frozen} at the base, {len(e.recorded)} references recorded ({REFS_PATH})")
    counts = (fanouts or ({}, {}))[0]
    if e.original in counts:
        print(f"  {e.original} (what it keeps): fan-out {counts[e.original]}")
    for piece in e.pieces:
        print(piece_line(piece, e.values.get(piece), e.frozen))
    outside = f" ({', '.join(e.new[:5])}{', ...' if len(e.new) > 5 else ''})" if e.new else " (a subset)"
    print(f"  union of references: {len(e.union)}, of them outside {e.original}'s set: {len(e.new)}{outside}")


def distance_verdict(a: Optional[int], b: Optional[int], split_package: bool, reached: bool, cap: int) -> str:
    """What the split makes of one package's distance row, `a` at the base and `b` measured."""
    if a is None:
        return "NEW past the limit: refused"
    if b is None or b < a:
        return "fell"
    if split_package and b - a <= DISTANCE_SPLIT_TOLERANCE:
        if b <= cap:
            return f"rose within the split's {DISTANCE_SPLIT_TOLERANCE}: tolerated"
        return f"rose past its ceiling: refused -- free {(b - cap) / 100:.2f} of its D: {DISTANCE_CUT}"
    return "rose: refused" + ("" if reached else " (a package the split does not reach)")


def headroom(pkg: str, a: Optional[int], b: Optional[int], caps: Dict[str, int]) -> str:
    """How far below its ceiling a package's distance sits ("" for a package without a ceiling)."""
    held = b if b is not None else (a or 0)
    return f"ceiling {caps[pkg]}, headroom {(caps[pkg] - held) / 100:.2f} left" if pkg in caps else ""


def distance_line(pkg: str, a: Optional[int], b: Optional[int], caps: Dict[str, int], split_package: bool, reached: bool) -> str:
    verdict = distance_verdict(a, b, split_package, reached, caps.get(pkg, 0))
    room = headroom(pkg, a, b, caps)
    return (f"  distance {pkg}: {'-' if a is None else a} -> {'-' if b is None else b} -- {verdict}"
            + (f"; {room}" if room and a is not None else ""))


def say_distances(before: Rows, measured: Rows, packages: Set[str], reached: Set[str]) -> None:
    """Each distance row the split may move: its value before and after, its verdict, the headroom its ceiling leaves."""
    old_d, new_d = of_kind(before, "distance"), of_kind(measured, "distance")
    caps = ceilings(before)
    for pkg in sorted(set(old_d) | set(new_d)):
        a, b = old_d.get(pkg), new_d.get(pkg)
        if a != b:
            print(distance_line(pkg, a, b, caps, pkg in packages, pkg in reached))
        elif pkg in reached:
            print(f"  distance {pkg}: {a} -- {headroom(pkg, a, b, caps)}")


def split_check(current_path: str, fanouts_path: str, original: str, pieces: List[str], base: str = "") -> int:
    """`split-check` (the module docstring): the ratchet's own split_credit and frozen_growth over base..<the working
    tree>, explained."""
    base = base or default_base()
    before = parse(file_at(base, BASELINE_PATH))
    measured = parse(with_ceilings(file_text(current_path), [before]))  # what --update would write
    fanouts = read_fanouts(fanouts_path)
    rng = f"{base}..{snapshot()}"
    explain: List[SplitExplanation] = []
    admitted, packages, notes, refused = split_credit(rng, before, measured, fanouts, [(original, pieces)], explain)
    if explain:  # the names as the ratchet resolved them (a simple name to its one class)
        original, pieces = explain[0].original, explain[0].pieces
    print(f"split-check: {original} into {', '.join(pieces)}, the working tree against {base[:12]}")
    for e in explain:
        say_explanation(e, fanouts)
    return say_split(before, measured, rng, (original, pieces), (admitted, packages), notes, refused)


def say_split(before: Rows, measured: Rows, rng: str, split: Tuple[str, List[str]], credit: Tuple[Set[str], Set[str]],
              notes: List[str], refused: List[str]) -> int:
    """The split's distances, notes and refusals against the rows `--update` would write; the exit code."""
    (original, pieces), (admitted, packages) = split, credit
    units = layer_units(file_at(rng.split("..", 1)[1], LAYERS_PATH))
    reached = packages or ({package_of(c, units) for c in pieces + [original]})
    say_distances(before, measured, packages, reached)
    grown = frozen_growth(before, measured, admitted, packages)
    ours = {original, *pieces} | reached
    mine = [g for g in grown if g.split(" ")[1] in ours]
    say_outcome(notes, refused, mine, [g for g in grown if g not in mine])
    if refused or mine:
        print(SPLIT_RULE)
        return 1
    print(f"commit it with: Coupling: split of {original} into {', '.join(pieces)}"
          " (after scripts/coupling-report --update records the pieces' rows)")
    return 0


def say_outcome(notes: List[str], refused: List[str], mine: List[str], others: List[str]) -> None:
    """The split's accepted notes, its refusals (the judgement's and the frozen rows it moves), and the rows outside it."""
    for note in notes:
        print(f"ACCEPTED: {note}")
    for r in refused + mine:
        print(f"REFUSED: {r}")
    if others:
        print("  (the range also holds rows outside this split the ratchet would refuse: " + "; ".join(others[:5])
              + (", ..." if len(others) > 5 else "") + ")")


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    c = sub.add_parser("check")
    c.add_argument("--current", required=True)
    c.add_argument("--baseline", default=BASELINE_PATH)
    c.add_argument("--layers", default=LAYERS_PATH)
    c.add_argument("--edges", default="")
    c.add_argument("--exemptions", default=EXEMPTIONS_PATH)
    c.add_argument("--fanouts", default="")
    c.add_argument("--collaborators", default="")
    c.add_argument("--range", action="append", default=[])
    s = sub.add_parser("split-check")
    s.add_argument("--current", required=True)
    s.add_argument("--fanouts", required=True)
    s.add_argument("--split", required=True)
    s.add_argument("--into", action="append", required=True)
    s.add_argument("--base", default="")
    u = sub.add_parser("update")
    u.add_argument("--current", required=True)
    u.add_argument("--baseline", default=BASELINE_PATH)
    u.add_argument("--fanouts", default="")
    u.add_argument("--collaborators", default="")
    n = sub.add_parser("new-exemptions")
    n.add_argument("--since", default="7d")
    n.add_argument("--rev", default="")
    r = sub.add_parser("refs")
    r.add_argument("--current", required=True)
    r.add_argument("--fanouts", required=True)
    args = parser.parse_args(argv)
    try:
        if args.command == "update":
            return update_all(args.current, args.baseline, args.fanouts, args.collaborators)
        if args.command == "new-exemptions":
            return new_exemptions(args.since, args.rev)
        if args.command == "refs":
            return refs(args.current, args.fanouts)
        if args.command == "split-check":
            return split_check(args.current, args.fanouts, args.split, args.into, args.base)
        return check(args.current, args.baseline, args.range, args.layers, args.edges, args.exemptions, args.fanouts,
                     args.collaborators)
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"coupling_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
