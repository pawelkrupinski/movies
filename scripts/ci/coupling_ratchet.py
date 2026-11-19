#!/usr/bin/env python3
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
# THE LIMITS: a class's fan-out (`limit fanout`, a declared contract's members counted once) and a package's distance D
# (`limit distance` in hundredths, packages of `limit distance-min-types` types or more) are FROZEN where they were past
# the limit when the limits came in -- a `fanout` / `distance` row may only fall -- and a class or package NEWLY past a
# limit fails: no `--update` and no trailer admits it, only an exemption row (test/resources/coupling-exemptions.tsv),
# and the exemption table may grow or loosen -- or a contract's members change, either way -- only in a commit changing it
# carrying `Coupling: <why>` -- and a fan-out row added (an agent's exemption of a new class, the owner 2026-11-19) only
# with a SPECIFIC why (its role and the collaborators it needs: reason_problems), its `added` date, and the adding commit's
# own `Coupling: exempt <Class> — <reason>` trailer (the exemption table's header, A FAN-OUT ROW ADDED). A declared contract has at most 8 members (`contract-size` grandfathers one declared before).
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
# another's) re-takes the rows at their true values, as a changed limit does, under its `Coupling:` trailer.
#
# A SPLIT (the owner, 2026-11-18; the union rule, 2026-11-18): a frozen class X cut into pieces may freeze the pieces
# past the limit at their new values when a commit of the range carries `Coupling: split of <X>` (`... into <A>, <B>`
# names the pieces; without it they are the fanout rows the range adds) and ALL hold: X had a frozen row at the range's
# base; every piece takes over X's code (git sees its source file as a rename or copy of X's, or it defines 2+ members
# X's file no longer does); every piece's fan-out is strictly below X's frozen value; and the UNION of what the pieces
# and X's remainder (if X still exists) reference -- folded as the count folds it, their references to each other and to
# X internal to the split -- lies within X's frozen reference set at the base (test/resources/coupling-fanout-refs.tsv,
# which `--update` writes beside the baseline, as many entries as X's row; a contract X used covers its members): the
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
#                             [--fanouts <file>] [--range A..B ...]
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
# A row is `kind<TAB>key<TAB>value`; `#` lines are comments (project/Coupling.scala's BaselineHeader has the kinds).
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from typing import Dict, List, Optional, Set, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # trailers.py beside it, under `python3 -I` too
import trailers  # noqa: E402

BASELINE_PATH = "test/resources/coupling-baseline.tsv"
LAYERS_PATH = "test/resources/coupling-layers.tsv"
EXEMPTIONS_PATH = "test/resources/coupling-exemptions.tsv"
REFS_PATH = "test/resources/coupling-fanout-refs.tsv"
DISTANCE_SPLIT_TOLERANCE = 2  # hundredths a valid split may raise the distance of a package holding X, a piece, or a use
DISTANCE_CAP = "distance-cap"  # a frozen distance row's ceiling (THE CEILING): it only falls
DISTANCE_CUT = ("move a leaf value type down to models or tools, or a trait to the seam"
                " (84a5db6d1 took services.movies from 0.39 to 0.28 so)")
PRESENCE = ("module-edge", "cycle-edge")  # only the pair is held, its weight is information (a cycle edge: none at all)
VALUED = ("ce", "fanout", "distance")
FROZEN = (("fanout", "class", "fan-out"), ("distance", "package", "distance D (hundredths)"))  # only an exemption admits a new one
DEFAULT_LIMITS = {"fanout": 9, "fanout-measure": 1, "new-package-ce": 8, "distance": 20, "distance-min-types": 8}
# How a frozen kind is measured (project/Coupling.scala's FanOutMeasure): a baseline without the row is measure 1. A
# measurement fix raises it, re-taking the kind's rows at their true values under its `Coupling:` trailer.
MEASURES = ("fanout-measure",)

Rows = Dict[Tuple[str, str], int]
Edges = Dict[str, List[str]]  # "<from package>-><to package>" -> its class references, "<from class> -> <to class>"


def parse(text: str) -> Rows:
    rows: Rows = {}
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        parts = line.split("\t")
        if len(parts) != 3 or not parts[2].lstrip("-").isdigit():
            raise ValueError(f"line {n}: not `kind<TAB>key<TAB>value`: {line!r}")
        rows[(parts[0], parts[1])] = int(parts[2])
    return rows


def limits(rows: Rows) -> Dict[str, int]:
    out = dict(DEFAULT_LIMITS)
    out.update({key: value for (kind, key), value in rows.items() if kind == "limit"})
    return out


def of_kind(rows: Rows, kind: str) -> Dict[str, int]:
    return {key: value for (k, key), value in rows.items() if k == kind}


def read_edges(text: str) -> Edges:
    """couplingReport's edges file: `<from package>-><to package><TAB><from class>-><to class>` a line."""
    out: Edges = {}
    for line in text.splitlines():
        if "\t" in line:
            key, ref = line.split("\t", 1)
            out.setdefault(key, []).append(ref.replace("->", " -> "))
    return out


def behind(edges: Edges, key: str) -> str:
    """The class references behind a package edge, the first three: what to cut."""
    refs = edges.get(key, [])
    return f": {', '.join(refs[:3])}{', ...' if len(refs) > 3 else ''}" if refs else ""


def violations(current: Rows, baseline: Rows, ranks: Dict[str, int] = None, edges: Edges = None,
               renamed: Dict[Tuple[str, str], str] = None) -> List[str]:
    """The measured rows the ratchet refuses -- a package cycle edge, a layer or rank violation, a row past the
    baseline -- one sentence each. `renamed`: the (kind, key) rows a range's renames carry over (`rename_credit`)."""
    lim = limits(baseline)
    ranks, edges, renamed = ranks or {}, edges or {}, renamed or {}
    out = []
    for key, refs in sorted(of_kind(current, "cycle-edge").items()):
        out.append(f"a package cycle edge: {key} ({refs} class references{behind(edges, key)}) -- the package graph has no "
                   "cycle, and no `Coupling:` trailer excuses one: cut it (scripts/coupling-report names every edge of the cycle)")
    for key, refs in sorted(of_kind(current, "layer-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no layer: give it a row in test/resources/coupling-layers.tsv")
        else:
            out.append(f"a package edge into a higher layer: {key} ({refs} class references{behind(edges, key)}) -- cut it, or "
                       "allow it with its why in test/resources/coupling-layers.tsv")
    for key, refs in sorted(of_kind(current, "rank-violation").items()):
        a, b = key.split("->")
        if b == "?":
            out.append(f"package {a} has no rank in its layer: give it a `rank` row in test/resources/coupling-layers.tsv")
        else:
            at = lambda p: f" (rank {ranks[p]})" if p in ranks else ""
            out.append(f"an upward edge inside a layer: {a}{at(a)} -> {b}{at(b)} ({refs} class references{behind(edges, key)}) -- "
                       f"the allowed direction is {b} -> {a}: cut it (move the type down, or a trait into {a}), or re-rank with "
                       "its why in test/resources/coupling-layers.tsv")
    for key, refs in sorted(of_kind(current, "module-edge").items()):
        if key not in of_kind(baseline, "module-edge"):
            out.append(f"a new cross-module edge kind: {key} ({refs} class references)")
    base_ce = of_kind(baseline, "ce")
    for key, ce in sorted(of_kind(current, "ce").items()):
        allowed = base_ce.get(key, lim["new-package-ce"])
        if ce > allowed:
            was = f"its baseline {allowed}" if key in base_ce else f"the new-package limit {allowed}"
            out.append(f"package {key}: Ce {ce} past {was}")
    for kind, noun, what in FROZEN:
        base, cur = of_kind(baseline, kind), of_kind(current, kind)
        for key, value in sorted(cur.items()):
            if key in base:
                if value > base[key]:
                    out.append(f"{noun} {key}: {what} {value} past its frozen row {base[key]} -- a frozen row only falls")
            elif not moved(key, value, base, cur) and (kind, key) not in renamed:
                out.append(f"{noun} {key}: {what} {value} past the limit {lim[kind]} -- cut it below the limit; only an "
                           f"exemption row admits it ({EXEMPTIONS_PATH}, with `Coupling: <why>`), never --update")
    return out


def moved(key: str, value: int, old: Dict[str, int], new: Dict[str, int]) -> bool:
    """Whether `key` is a frozen row that moved: a row of the same simple name, at or above `value`, gone from `new`."""
    simple = key.rsplit(".", 1)[-1]
    return any(k.rsplit(".", 1)[-1] == simple and k not in new and v >= value for k, v in old.items())


# scripts/pure-move-check's code; COUPLING_PURE_MOVE_CHECK names it for a copy of this tool elsewhere (the mutation test's)
PURE_MOVE_CHECK = os.environ.get("COUPLING_PURE_MOVE_CHECK") or os.path.join(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "pure_move_check.py")


class RenameEvidence:
    """What git says a range renamed (A RENAME, the module docstring), read once per range and only when a rename is in
    question: the range's `git diff -M` renames, and the commits scripts/pure-move-check calls PURE."""

    def __init__(self, rng: str):
        self.base, head = rng.split("..", 1)
        self.head = head or "HEAD"
        self.rng = f"{self.base}..{self.head}"
        self._renames: Optional[Set[Tuple[str, str]]] = None
        self._copies: Optional[Set[Tuple[str, str]]] = None
        self._pure: Dict[str, bool] = {}
        self._sources: Dict[Tuple[str, str], Optional[str]] = {}
        self._packages: Dict[Tuple[str, str], str] = {}

    def _read(self) -> None:
        if self._renames is None:
            status = git("diff", "-M", "-C", "--name-status", self.base, self.head)
            pairs = [(cols[0][:1], cols[1], cols[2]) for cols in (line.split("\t") for line in status.splitlines())
                     if len(cols) == 3 and cols[0][:1] in "RC"]
            self._renames = {(a, b) for s, a, b in pairs if s == "R"}
            self._copies = {(a, b) for _, a, b in pairs}

    def renames(self) -> Set[Tuple[str, str]]:
        self._read()
        return self._renames

    def copies(self) -> Set[Tuple[str, str]]:
        self._read()
        return self._copies

    def source(self, rev: str, cls: str) -> Optional[str]:
        if (rev, cls) not in self._sources:
            self._sources[(rev, cls)] = source_of(rev, cls)
        return self._sources[(rev, cls)]

    def package(self, rev: str, path: str) -> str:
        if (rev, path) not in self._packages:
            self._packages[(rev, path)] = package_of(".".join(re.findall(r"^package\s+([\w.]+)", file_at(rev, path), re.M)) + ".X")
        return self._packages[(rev, path)]

    def pure(self, sha: str) -> bool:
        """Whether scripts/pure-move-check calls commit `sha` a PURE MOVE (exit 0; NOT PURE and undecidable are not)."""
        if sha not in self._pure:
            done = subprocess.run([sys.executable, "-I", PURE_MOVE_CHECK, f"{sha}^..{sha}", "--quiet"], capture_output=True, text=True)
            self._pure[sha] = done.returncode == 0
        return self._pure[sha]

    def commits(self, *paths: str) -> List[str]:
        return git("rev-list", "--reverse", self.rng, "--", *paths).split() if paths else []

    def class_renamed(self, old: str, new: str) -> str:
        """How the range renamed class `old` to `new` ("" when it did not): git's rename of the one's source to the
        other's, or a PURE commit after which `new` is declared and `old` no longer is."""
        origin, dest = self.source(self.base, old), self.source(self.head, new)
        if origin is None or dest is None:
            return ""
        if (origin, dest) in self.renames():
            return f"git renames {origin} to {dest}"
        for sha in self.commits(origin):
            was, now = self.source(f"{sha}^", old), self.source(sha, new)
            if was and now and not self.source(sha, old) and (was == now or (was, now) in self.commit_renames(sha)) \
                    and self.pure(sha):
                return f"scripts/pure-move-check calls {sha[:9]} a PURE MOVE"
        return ""

    def split_away(self, origin: Optional[str], dest: Optional[str]) -> List[str]:
        """The other production sources of the range that took over code of `origin` renamed to `dest`: a copy of it
        (git's detection), or a file gaining MIN_MOVED_MEMBERS+ of the members `origin` declared and `dest` does not --
        whatever their fan-out (a piece below the limit is a piece too)."""
        if not origin or not dest:
            return []
        left = members(file_at(self.base, origin)) - members(file_at(self.head, dest))
        changed = git("diff", "--name-only", "--diff-filter=ACMR", "-M", self.base, self.head, "--", "*/src/main/*.scala").split()
        out = []
        for path in changed:
            if path == dest:
                continue
            gained = members(file_at(self.head, path)) - members(file_at(self.base, path))
            if (origin, path) in self.copies() or len(gained & left) >= MIN_MOVED_MEMBERS:
                out.append(path)
        return out

    def commit_renames(self, sha: str) -> Set[Tuple[str, str]]:
        status = git("diff", "-M", "--name-status", f"{sha}^", sha)
        return {(c[1], c[2]) for c in (line.split("\t") for line in status.splitlines()) if len(c) == 3 and c[0][:1] == "R"}

    def package_renamed(self, old: str, new: str) -> str:
        """How the range renamed package `old` to `new` ("" when it did not): every source of `old` git (or a PURE
        commit) renames lands in `new`, and at least one does."""
        def into(pairs, before, after):
            moved = [self.package(after, b) for a, b in pairs if a.endswith(".scala") and self.package(before, a) == old]
            return bool(moved) and all(p == new for p in moved)
        if into(self.renames(), self.base, self.head):
            return "git renames its sources"
        for sha in self.commits(*sorted({a for a, _ in self.renames()} | {a for a, _ in self.copies()})):
            if into(self.commit_renames(sha), f"{sha}^", sha) and self.pure(sha):
                return f"scripts/pure-move-check calls {sha[:9]} a PURE MOVE"
        return ""


def rename_credit(kind: str, before: Dict[str, int], after: Dict[str, int],
                  evidence: RenameEvidence) -> Tuple[Dict[str, str], List[str], List[str]]:
    """The rows of `after` (of a FROZEN `kind`) that are renames of a row of `before` (A RENAME, the module docstring):
    new name -> old name, with a note for each credited and why not for each refused. A row of the same simple name is
    `moved`'s."""
    gone = {k: v for k, v in before.items() if k not in after}
    new = {k: v for k, v in after.items() if k not in before and not moved(k, v, before, after)}
    if not gone or not new:
        return {}, [], []
    renamed = evidence.class_renamed if kind == "fanout" else evidence.package_renamed
    claims: Dict[str, List[Tuple[str, str]]] = {}
    for key in sorted(new):
        for old in sorted(gone):
            how = renamed(old, key)
            if how:
                claims.setdefault(old, []).append((key, how))
    credited, notes, refused = {}, [], []
    for old, found in sorted(claims.items()):
        names = [k for k, _ in found]
        if len(found) > 1:
            refused.append(f"{kind} {old} ({gone[old]}) went to {', '.join(names)}: a rename has one new name -- several is "
                           f"a split (`Coupling: split of {old}`, scripts/coupling-report --check-split)")
            continue
        key, how = found[0]
        if new[key] > gone[old]:
            refused.append(f"{kind} {key} is a rename of {old} ({how}) but {new[key]} is past {old}'s frozen row {gone[old]}"
                           " -- a rename carries a row at or below its value, never above")
            continue
        if kind == "fanout":
            others = evidence.split_away(evidence.source(evidence.base, old), evidence.source(evidence.head, key))
            if others:
                refused.append(f"fanout {old} ({gone[old]}) renamed to {key}, but {', '.join(others)} also took over its code:"
                               f" a split posing as a rename (`Coupling: split of {old}`, scripts/coupling-report --check-split)")
                continue
        credited[key] = old
        notes.append(f"{kind} {old} ({gone[old]}) renamed to {key} ({how}): its frozen row carries over at {new[key]}")
    return credited, notes, refused


SPLIT_TRAILER = re.compile(r"split of\s+(\S+)(?:\s+into\s+(.+))?$")
DECLARES = re.compile(r"\b(?:def|val|var|type|class|object|trait|enum)\s+([A-Za-z_][A-Za-z0-9_]*)")
MIN_MOVED_MEMBERS = 2


def split_claims(rng: str) -> List[Tuple[str, List[str]]]:
    """The `Coupling: split of <X> [into <A>, <B>]` trailers of the commits of `rng`: (X, the pieces named)."""
    log = git("log", "--format=%x00%(trailers:key=Coupling,valueonly,separator=%x1e)", rng)
    claims = []
    for value in (v.strip() for entry in log.split("\x00") for v in entry.split("\x1e")):
        m = SPLIT_TRAILER.match(value)
        if m:
            claims.append((m.group(1), [p.strip() for p in (m.group(2) or "").split(",") if p.strip()]))
    return claims


def resolve(name: str, known) -> Optional[str]:
    """`name` among `known` class names: itself, or the one class of that simple name."""
    if name in known:
        return name
    same = [k for k in known if k.rsplit(".", 1)[-1] == name]
    return same[0] if len(same) == 1 else None


def source_of(rev: str, cls: str) -> Optional[str]:
    """The production Scala file declaring `cls` (its package clauses its package) at `rev`, None for none."""
    pkg, _, simple = cls.rpartition(".")
    found = subprocess.run(["git", "grep", "-l", "-E", rf"(class|object|trait|enum)[[:space:]]+{simple}([^[:alnum:]_]|$)",
                            rev, "--", "*/src/main/*.scala"], capture_output=True, text=True).stdout
    for path in (line.split(":", 1)[1] for line in found.splitlines() if ":" in line):
        if ".".join(re.findall(r"^package\s+([\w.]+)", file_at(rev, path), re.M)) == pkg:
            return path
    return None


def members(text: str) -> Set[str]:
    return set(DECLARES.findall(text))


def took_over(base: str, head: str, origin: str, piece: str, copies: Set[Tuple[str, str]]) -> Tuple[bool, str]:
    """Whether `piece` (at `head`) takes over code of the class whose file at `base` was `origin`: its file a rename or
    copy of `origin` (git's detection over the range), or 2+ members `origin` declared at `base` and no longer does,
    now declared in the piece's file and not before. With why not, when not."""
    path = source_of(head, piece)
    if path is None:
        return False, "no source file declares it"
    if path == origin:
        return False, "it lives in the split class's own file: move it to its own, so the move is seen"
    if (origin, path) in copies:
        return True, ""
    moved = members(file_at(base, origin)) - members(file_at(head, origin))
    taken = (members(file_at(head, path)) - members(file_at(base, path))) & moved
    return len(taken) >= MIN_MOVED_MEMBERS, ""


def package_of(cls: str) -> str:
    """A class's package as the report groups it (Coupling.packageOf): its full package cut to the top two segments."""
    full = cls.rpartition(".")[0]
    return ".".join(full.split(".")[:2]) if full else "<root>"


def recorded_refs(text: str) -> Dict[str, Set[str]]:
    """The frozen reference sets (test/resources/coupling-fanout-refs.tsv): `<class><TAB><target>` a line."""
    out: Dict[str, Set[str]] = {}
    for line in text.splitlines():
        if "\t" in line and not line.startswith("#"):
            cls, target = line.split("\t", 1)
            out.setdefault(cls, set()).add(target)
    return out


def contract_members(exemptions_text: str) -> Dict[str, List[str]]:
    """Each declared contract's member patterns (the exemption table's `member` rows)."""
    out: Dict[str, List[str]] = {}
    for (kind, pattern), contract in parse_exemptions(exemptions_text).items():
        if kind.startswith("member:"):
            out.setdefault(contract, []).append(pattern)
    return out


def pattern_matches(pattern: str, name: str) -> bool:
    """Coupling.Exemptions.matches: `a.*` names the package `a` too; `*` is any run of characters."""
    return (pattern.endswith(".*") and name == pattern[:-2]) or re.fullmatch(".*".join(map(re.escape, pattern.split("*"))), name) is not None


def covered(target: str, refs: Set[str], members: Dict[str, List[str]]) -> bool:
    """Whether `target` is within the reference set `refs`: in it, or a member of a contract it counts as one."""
    return target in refs or any(f"contract:{c}" in refs and any(pattern_matches(p, target) for p in ps)
                                 for c, ps in members.items())


def split_credit(rng: str, before: Rows, after: Rows, fanouts: Optional[Fanouts],
                 claims: Optional[List[Tuple[str, List[str]]]] = None,
                 explain: Optional[List[dict]] = None) -> Tuple[Set[str], Set[str], List[str], List[str]]:
    """The fanout rows of `after` that `rng`'s `Coupling: split of <X>` trailers honestly admit and the packages whose
    distance they may raise, with a note for each split admitted and a reason for each refused (the rule: the module
    docstring, A SPLIT). `claims` judges those splits instead of the range's trailers (`split-check`, before any commit
    carries one); `explain` collects, per claim, what the judgement read."""
    base, head = rng.split("..", 1)
    head = head or "HEAD"
    frozen, now = of_kind(before, "fanout"), of_kind(after, "fanout")
    added = sorted(k for k in now if k not in frozen and not moved(k, now[k], frozen, now))
    claims = split_claims(rng) if claims is None else claims
    counts, targets = fanouts or ({}, {})
    base_refs = recorded_refs(file_at(base, REFS_PATH)) if claims else {}
    members = contract_members(file_at(base, EXEMPTIONS_PATH)) if claims else {}
    copies = None
    admitted, packages, notes, refused = set(), set(), [], []
    for name, named in claims:
        original = resolve(name, frozen)
        if original is None:
            refused.append(f"split of {name}: {name} has no frozen fanout row at {base[:12]}")
            continue
        if not named and len(claims) > 1:
            refused.append(f"split of {original}: {len(claims)} splits in {rng} -- name each one's pieces (`into <A>, <B>`)")
            continue
        known = set(now) | set(counts)
        pieces = [resolve(n, known) or n for n in named] or added
        if copies is None:
            status = git("diff", "-M", "-C", "--name-status", base, head)
            copies = {(cols[1], cols[2]) for cols in (line.split("\t") for line in status.splitlines())
                      if len(cols) == 3 and cols[0][:1] in "RC"}

        why = []
        origin = source_of(base, original)
        values = {}
        union: Set[str] = set()
        internal = set(pieces) | {original}
        for piece in pieces:
            v = now.get(piece, counts.get(piece))
            if v is None:
                why.append(f"{piece} is no class at {head}")
                continue
            values[piece] = v
            if v >= frozen[original]:
                why.append(f"{piece} {v} is not below {original}'s {frozen[original]}")
            ok, because = took_over(base, head, origin, piece, copies) if origin else (False, "")
            if not ok:
                why.append(f"{piece} takes over none of {original}'s code" + (f" ({because})" if because else ""))
            if piece in targets:
                union |= targets[piece]
            else:
                why.append(f"{piece}'s references are not measured (couplingReport's fanouts, with their targets)")
        if original in counts:
            if original in targets:
                union |= targets[original]
            else:
                why.append(f"what {original} keeps is not measured (couplingReport's fanouts, with its targets)")
        union -= internal
        shown = ", ".join(f"{p} {v}" for p, v in values.items())
        if not pieces:
            why.append("no pieces: name them (`into <A>, <B>`) or record their rows (scripts/coupling-report --update)")
        was = base_refs.get(original, set())
        if len(was) != frozen[original]:
            why.append(f"{original}'s recorded reference set at {base[:12]} ({REFS_PATH}) has {len(was)}, its row "
                       f"{frozen[original]}: record both with scripts/coupling-report --update in an earlier commit")
        new = sorted(t for t in union if not covered(t, was, members))
        if new:
            why.append(f"the pieces add what {original} did not reference at {base[:12]}: {', '.join(new[:5])}"
                       + (", ..." if len(new) > 5 else "") + " -- a split adds no dependency")
        summary = f"split of {original} ({frozen[original]}) into {shown}"
        if explain is not None:
            explain.append({"original": original, "frozen": frozen[original], "pieces": pieces, "values": values,
                            "union": union, "recorded": was, "new": new, "why": why})
        if why:
            refused.append(f"{summary}: " + "; ".join(why))
        else:
            admitted |= set(pieces)
            # the packages holding X or a piece, and those the split's references reach: each piece is one more outside
            # class using them (Ca counts classes), so their instability falls and their distance may rise
            packages |= {package_of(c) for c in internal} | {package_of(t) for t in union if not t.startswith("contract:")}
            notes.append(f"{summary}: their references ({len(union)}) within {original}'s {frozen[original]}")
    return admitted, packages, notes, refused


LIMIT_KEYS = {"fanout": ("fanout", "fanout-measure"), "distance": ("distance", "distance-min-types")}


def refrozen(old: Rows, new: Rows, kind: str) -> bool:
    """Whether the limits of a frozen kind changed between `old` and `new`: its rows are then taken again, under the new
    limit -- the change that sets a limit (a raise is growth, needing its trailer; a fall tightens) or how the kind is
    measured (a measure's absent row is measure 1)."""
    old_lim, new_lim = limits(old), limits(new)
    return any((("limit", key) not in old and key not in MEASURES) or old_lim.get(key) != new_lim.get(key)
               for key in LIMIT_KEYS[kind])


def ceilings(rows: Rows) -> Dict[str, int]:
    """Each frozen distance row's ceiling (THE CEILING): its `distance-cap` row, else the row + DISTANCE_SPLIT_TOLERANCE."""
    caps = of_kind(rows, DISTANCE_CAP)
    return {pkg: caps.get(pkg, d + DISTANCE_SPLIT_TOLERANCE) for pkg, d in of_kind(rows, "distance").items()}


def past_ceiling(key: str, value: int, cap: int) -> str:
    return (f", past its ceiling {cap}: free {(value - cap) / 100:.2f} of its D (the ceiling only falls) -- {DISTANCE_CUT}"
            if value > cap else "")


def frozen_growth(old: Rows, new: Rows, split: Set[str] = frozenset(), split_packages: Set[str] = frozenset(),
                  renamed: Dict[Tuple[str, str], str] = None) -> List[str]:
    """The frozen rows the baseline gained or raised from `old` to `new` -- never admitted, whatever the trailer, unless
    the kind's limit itself changed (`refrozen`: the rows are taken again, and the limit change carries the trailer), the
    row is a piece of an honest split (`split`, from `split_credit`), or the distance of a package holding one
    (`split_packages`) rose by DISTANCE_SPLIT_TOLERANCE at most and stays within its ceiling at `old`, or the row is a
    rename's (`renamed`, (kind, key) from `rename_credit`) -- and a ceiling raised (THE CEILING)."""
    out = []
    renamed = renamed or {}
    for kind, _, _ in FROZEN:
        if refrozen(old, new, kind):
            continue
        before, after = of_kind(old, kind), of_kind(new, kind)
        caps = ceilings(old)
        for key, value in sorted(after.items()):
            if key in before:
                cap = caps.get(key, before[key]) if kind == "distance" else value
                tolerated = (kind == "distance" and key in split_packages and value - before[key] <= DISTANCE_SPLIT_TOLERANCE
                             and value <= cap)
                if value > before[key] and not tolerated:
                    out.append(f"{kind} {key} raised {before[key]} -> {value}" + past_ceiling(key, value, cap))
            elif not moved(key, value, before, after) and not (kind == "fanout" and key in split) and (kind, key) not in renamed:
                out.append(f"{kind} {key} added at {value}")
    if not refrozen(old, new, "distance"):
        was = ceilings(old)
        for key, cap in sorted(ceilings(new).items()):
            if key in was and cap > was[key]:
                out.append(f"{DISTANCE_CAP} {key} raised {was[key]} -> {cap} -- a ceiling only falls"
                           + ("" if (DISTANCE_CAP, key) in new else f" (no `{DISTANCE_CAP}` row: scripts/coupling-report --update writes it)"))
            elif ("distance", key) in renamed and renamed[("distance", key)] in was and cap > was[renamed[("distance", key)]]:
                old_cap = was[renamed[("distance", key)]]
                out.append(f"{DISTANCE_CAP} {key} {cap} is past {renamed[('distance', key)]}'s {old_cap}: a renamed package keeps"
                           f" its ceiling -- set `{DISTANCE_CAP}\t{key}\t{old_cap}` in the baseline")
    return out


def with_ceilings(measured_text: str, sides: List[Rows]) -> str:
    """The baseline `update` writes: the measured rows, each frozen distance row followed by its ceiling -- the least of
    each side's (`sides`: the baselines it replaces) and the row + DISTANCE_SPLIT_TOLERANCE."""
    measured = parse(measured_text)
    caps = [ceilings(side) for side in sides]
    out = []
    for line in measured_text.splitlines():
        parts = line.split("\t")
        if len(parts) == 3 and parts[0] == DISTANCE_CAP:
            continue
        out.append(line)
        if len(parts) == 3 and parts[0] == "distance":
            pkg, d = parts[1], of_kind(measured, "distance")[parts[1]]
            out.append(f"{DISTANCE_CAP}\t{pkg}\t{min([c[pkg] for c in caps if pkg in c] + [d + DISTANCE_SPLIT_TOLERANCE])}")
    return "\n".join(out) + "\n"


def sides_of(path: str) -> List[Rows]:
    """The baselines an update replaces: the file as it stands and, while git holds it unmerged (a rebase's conflict
    that agentctl's regen resolves), each stage of it -- the base, ours and theirs."""
    sides = []
    if os.path.exists(path):
        with open(path) as f:
            text = f.read()
        if "\n<<<<<<< " not in "\n" + text:
            sides.append(parse(text))
    for stage in (1, 2, 3):
        found = subprocess.run(["git", "show", f":{stage}:{path}"], capture_output=True, text=True)
        if found.returncode == 0:
            sides.append(parse(found.stdout))
    return sides


def update(current_path: str, baseline_path: str) -> int:
    with open(current_path) as f:
        measured = f.read()
    text = with_ceilings(measured, sides_of(baseline_path))
    with open(baseline_path, "w") as f:
        f.write(text)
    return 0


def growth(old: Rows, new: Rows) -> List[str]:
    """How the baseline grew from `old` to `new`: rows added, values raised (the presence kinds' weights aside)."""
    out = []
    old_lim, new_lim = limits(old), limits(new)
    for key in sorted(new_lim):
        if new_lim[key] > old_lim.get(key, 0):
            out.append(f"limit {key} raised {old_lim.get(key, 0)} -> {new_lim[key]}")
    for kind, _, _ in FROZEN:
        if refrozen(old, new, kind):
            out.append(f"{kind} rows taken again under new limits ({len(of_kind(new, kind))} frozen)")
    for kind in PRESENCE:
        for key in sorted(set(of_kind(new, kind)) - set(of_kind(old, kind))):
            out.append(f"{kind} {key} added")
    old_ce = of_kind(old, "ce")
    for key, ce in sorted(of_kind(new, "ce").items()):
        allowed = old_ce.get(key, old_lim["new-package-ce"])
        if ce > allowed:
            out.append(f"ce {key} raised {allowed} -> {ce}")
    return out


def falls(current: Rows, baseline: Rows) -> List[str]:
    """The baseline rows the measured code is below (or no longer has): falls `--update` would record."""
    out = []
    for kind in PRESENCE:
        out += [f"{kind} {key} gone" for key in sorted(set(of_kind(baseline, kind)) - set(of_kind(current, kind)))]
    for kind in VALUED:
        cur = of_kind(current, kind)
        for key, value in sorted(of_kind(baseline, kind).items()):
            if cur.get(key, 0) < value:
                out.append(f"{kind} {key} {value} -> {cur.get(key, 0 if kind == 'ce' else 'below the limit')}")
    return out


Layers = Tuple[Dict[str, int], set, Dict[str, int]]


def parse_layers(text: str) -> Layers:
    """The layer table's `layer` rows (prefix -> layer), `allow` edges and `rank` rows (package -> rank)
    (project/Coupling.scala's Layers.parse is the one that judges the table's shape; this reads only what loosening
    compares and the violations cite)."""
    rows: Dict[str, int] = {}
    allowed = set()
    ranks: Dict[str, int] = {}
    for line in text.splitlines():
        cols = line.split("\t")
        if cols[0] == "layer" and len(cols) >= 3 and cols[1].isdigit():
            rows[cols[2]] = int(cols[1])
        elif cols[0] == "rank" and len(cols) >= 3 and cols[1].isdigit():
            ranks[cols[2]] = int(cols[1])
        elif cols[0] == "allow" and len(cols) >= 2:
            allowed.add(cols[1])
    return rows, allowed, ranks


def layer_of(rows: Dict[str, int], pkg: str):
    """A package's layer: the longest prefix row naming it (Coupling.Layers.layerOf), None for none."""
    named = [p for p in rows if pkg == p or pkg.startswith(p + ".")]
    return rows[max(named, key=len)] if named else None


def loosening(old: Layers, new: Layers) -> List[str]:
    """How the layer table loosened from `old` to `new`: an `allow` edge added, or a package that had a layer or a rank
    given another (up, its own edges may point higher; down, lower packages may reach it). A new package taking its
    first layer or rank, or a row dropped (its packages unplaced: a violation), loosens nothing."""
    out = [f"allow {edge} added" for edge in sorted(new[1] - old[1])]
    for prefix in sorted(set(old[0]) | set(new[0])):
        was, now = layer_of(old[0], prefix), layer_of(new[0], prefix)
        if was is not None and now is not None and was != now:
            out.append(f"{prefix} moved from layer {was} to {now}")
    for pkg in sorted(set(old[2]) & set(new[2])):
        if old[2][pkg] != new[2][pkg]:
            out.append(f"{pkg} re-ranked from {old[2][pkg]} to {new[2][pkg]}")
    return out


CONTRACT_KINDS = ("contract", "member")
CONTRACT_MAX_MEMBERS = 8  # a contract's members, past which it is no longer one thing read together (`contract-size` grandfathers one)


def parse_exemptions(text: str) -> Dict[Tuple[str, str], str]:
    """The exemption table's rows, (kind, pattern) -> its value (project/Coupling.scala's Exemptions.parse judges the
    table's shape; this reads what loosening compares and the contract sizes). A contract's row is keyed by its
    contract too (`member:scrape`): one type may be a member of two contracts."""
    rows: Dict[Tuple[str, str], str] = {}
    for line in text.splitlines():
        cols = line.split("\t")
        if line.strip() and not line.startswith("#") and len(cols) >= 3:
            kind = f"{cols[0]}:{cols[2]}" if cols[0] in CONTRACT_KINDS else cols[0]
            rows[(kind, cols[1])] = cols[2]
    return rows


def exemption_loosening(old: Dict[Tuple[str, str], str], new: Dict[Tuple[str, str], str]) -> List[str]:
    """How the exemption table loosened from `old` to `new`: a row added (a class or package let past a limit, or a type
    folded into a contract), or a row's limit or contract changed. A row dropped tightens -- except a contract's member:
    any change to a contract's members reshapes what its users count as one, and carries its why."""
    out = []
    for key, value in sorted(new.items()):
        if key not in old:
            out.append(f"{key[0]} {key[1]} ({value}) added")
        elif old[key] != value:
            out.append(f"{key[0]} {key[1]} changed {old[key]} -> {value}")
    for key, value in sorted(old.items()):
        if key not in new and key[0].startswith("member:"):
            out.append(f"{key[0]} {key[1]} ({value}) dropped")
    return out


FAN_OUT_KINDS = ("root", "registry", "config", "class")  # project/Coupling.scala's Exemptions.FanOutKinds
REASON_MIN_CHARS, REASON_MIN_WORDS, ROLE_MAX_WORDS = 40, 6, 5
# What says nothing of the class (the owner, 2026-11-19: "not a generic 'complex' or 'needed'"): each a pattern over the
# lowercased why, and the phrase to name.
GENERIC_REASONS = (
    (r"\bcomplex(ity)?\b", "complex"),
    (r"(^|\b(is|are|it's|they're|all|were|was|be)\s+)(needed|necessary|required)\b", "needed"),
    (r"\bfor now\b", "for now"),
    (r"\btodo\b", "TODO"),
    (r"\btemporar(y|ily)\b", "temporary"),
    (r"\b(fix|split|cut|clean|refactor)\w*\s+(\w+\s+)?later\b", "later"),
    (r"\bhard to (split|cut|refactor|break)\b", "hard to split"),
    (r"\b(can'?t|cannot|could not) be (split|cut)\b", "cannot be split"),
    (r"\btoo (many|big|large)\b", "too many"),
    (r"\blegacy\b", "legacy"),
    (r"\b(big|large|god) class\b", "big class"),
    (r"\buses (a lot|lots|many things)\b", "uses a lot"),
)
EXEMPT_TRAILER = re.compile(r"exempt\s+(\S+)\s*(?:—|–|--|-|:)\s*(.*)$")
ISO_DATE = re.compile(r"\d{4}-\d{2}-\d{2}$")
REASON_SHAPE = ("`<role>: <what it does, and why it needs these collaborators>`, e.g. `pipeline stage: clusters a family's"
                " members; needs the evidence, edges and solver it sequences`")


def reason_problems(pattern: str, why: str) -> List[str]:
    """Why `why` is no SPECIFIC reason for exempting `pattern` (A FAN-OUT ROW ADDED, the exemption table's header): it
    leads with the class's role, says 40+ characters and 6+ words besides the class's name, and no generic phrase."""
    text = why.strip()
    out = []
    role, sep, _ = text.partition(":")
    if not sep or not role.strip() or len(role.split()) > ROLE_MAX_WORDS:
        out.append(f"lead with the class's role: {REASON_SHAPE}")
    rest = text
    for name in sorted({pattern, pattern.rsplit(".", 1)[-1]}, key=len, reverse=True):
        rest = rest.replace(name, " ")
    if not re.sub(r"[\W_]+", "", rest):
        out.append("the class's name is the whole reason")
    elif len(rest.strip()) < REASON_MIN_CHARS or len(rest.split()) < REASON_MIN_WORDS:
        out.append(f"too short: {REASON_MIN_CHARS}+ characters and {REASON_MIN_WORDS}+ words besides the class's name")
    lowered = text.lower()
    out += [f"`{phrase}` is generic: say what the class does and which collaborators it needs for it"
            for rx, phrase in GENERIC_REASONS if re.search(rx, lowered)]
    return out


def exemption_columns(text: str) -> Dict[Tuple[str, str], List[str]]:
    """The exemption table's fan-out rows, (kind, pattern) -> all its columns."""
    return {(cols[0], cols[1]): cols for cols in (line.split("\t") for line in text.splitlines()
                                                  if line.strip() and not line.startswith("#"))
            if len(cols) >= 5 and cols[0] in FAN_OUT_KINDS}


def coupling_trailers(sha: str) -> List[str]:
    """The `Coupling:` trailers of `sha` as git reads them (the final trailer block)."""
    out = git("log", "-1", "--format=%(trailers:key=Coupling,valueonly,separator=%x1e)", sha)
    return [v.strip() for v in out.split("\x1e") if v.strip()]


def exempt_trailer(pattern: str, values: List[str]) -> Optional[str]:
    """The reason of `sha`'s `Coupling: exempt <pattern> — <reason>` trailer (its full or simple name), None for none."""
    names = {pattern, pattern.rsplit(".", 1)[-1]}
    found = [m.group(2) for m in (EXEMPT_TRAILER.match(v) for v in values) if m and m.group(1) in names]
    return found[0] if found else None


def rows_added(sha: str) -> List[List[str]]:
    """The fan-out exemption rows commit `sha` adds (a commit creating the table adds none: no step)."""
    before, after = trailers.text_at(f"{sha}^", EXEMPTIONS_PATH), trailers.text_at(sha, EXEMPTIONS_PATH)
    if not before or not after:
        return []
    old = exemption_columns(before)
    return [cols for key, cols in sorted(exemption_columns(after).items()) if key not in old]


def unadmitted_exemptions(rng: str) -> List[str]:
    """Each fan-out exemption row a commit of `rng` adds without what admits it (A FAN-OUT ROW ADDED): a specific why,
    its `added` date, and the commit's own `Coupling: exempt <Class> — <reason>` trailer."""
    out = []
    for sha in git("rev-list", "--reverse", rng, "--", EXEMPTIONS_PATH).split():
        added = rows_added(sha)
        if not added:
            continue
        values = coupling_trailers(sha)
        subject = git("log", "-1", "--format=%s", sha).strip()
        for cols in added:
            kind, pattern, why = cols[0], cols[1], cols[4]
            where = f"{sha[:9]} {subject}: {kind} {pattern}"
            out += [f"{where}: its why ({why!r}): {p}" for p in reason_problems(pattern, why)]
            if len(cols) < 6 or not ISO_DATE.match(cols[5]):
                out.append(f"{where}: no `added` date (the sixth column, YYYY-MM-DD: the day it is added)")
            reason = exempt_trailer(pattern, values)
            if reason is None:
                out.append(f"{where}: no `Coupling: exempt {pattern.rsplit('.', 1)[-1]} — <reason>` trailer in the commit's"
                           " final trailer block")
            else:
                out += [f"{where}: its trailer's reason ({reason!r}): {p}" for p in reason_problems(pattern, reason)]
    return out


def new_exemptions(since: str, rev: str = "") -> int:
    """The exemption rows added since `since` (`7d`, or anything `git log --since` reads) on `rev` (default origin/main,
    else HEAD): each one's date, commit, reason and trailer, and whether it is still in the table -- the weekly list the
    owner strikes from (`scripts/coupling-report --new-exemptions --since 7d`)."""
    if not rev:
        rev = "origin/main" if subprocess.run(["git", "rev-parse", "-q", "--verify", "origin/main"],
                                              capture_output=True).returncode == 0 else "HEAD"
    days = re.fullmatch(r"(\d+)d", since)
    after = f"{days.group(1)} days ago" if days else since
    shas = git("log", "--reverse", f"--since={after}", "--format=%H", rev, "--", EXEMPTIONS_PATH).split()
    present = exemption_columns(file_at(rev, EXEMPTIONS_PATH))
    print(f"# Coupling exemptions added since {since} (on {rev}, {EXEMPTIONS_PATH})\n")
    found = 0
    for sha in shas:
        values = coupling_trailers(sha)
        day, subject = git("log", "-1", "--format=%cs%x1f%s", sha).strip().split("\x1f", 1)
        for cols in rows_added(sha):
            found += 1
            kind, pattern, why = cols[0], cols[1], cols[4]
            added = cols[5] if len(cols) > 5 else day
            struck = "" if (kind, pattern) in present else " (struck since)"
            print(f"- {added} {sha[:9]} `{kind}` **{pattern}**{struck} ({cols[2]}, {cols[3]}): {why}")
            print(f"  commit: {subject}")
            reason = exempt_trailer(pattern, values)
            print(f"  trailer: Coupling: exempt {pattern.rsplit('.', 1)[-1]} — {reason}" if reason is not None else "  trailer: none")
    if not found:
        print("No exemption rows added.")
    print(f"\nTo strike one: drop its row from {EXEMPTIONS_PATH} (no trailer needed) and cut the class below the limit.")
    return 0


def oversized_contracts(rows: Dict[Tuple[str, str], str]) -> List[str]:
    """The declared contracts past CONTRACT_MAX_MEMBERS members, or past their own `contract-size` row (a contract
    declared before the cap, grandfathered at its size with its why), one sentence each."""
    sizes: Dict[str, int] = {}
    for (kind, _), contract in rows.items():
        if kind.startswith("member:"):
            sizes[contract] = sizes.get(contract, 0) + 1
    allowed = {pattern: int(value) for (kind, pattern), value in rows.items() if kind == "contract-size" and value.isdigit()}
    out = []
    for contract, size in sorted(sizes.items()):
        cap = allowed.get(contract, CONTRACT_MAX_MEMBERS)
        if size > cap:
            out.append(f"contract {contract} has {size} members, past {'its grandfathered size' if contract in allowed else 'the cap'} {cap}"
                       " -- a contract is a few types read together: split it, or count the rest one by one")
    return out


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def file_at(rev: str, path: str) -> str:
    shown = subprocess.run(["git", "show", f"{rev}:{path}"], capture_output=True, text=True)
    return shown.stdout if shown.returncode == 0 else ""


def print_misplaced(rng: str, path: Optional[str] = None) -> None:
    """Why a refusal for want of a `Coupling:` trailer saw none: each such line of `rng` git reads as prose."""
    for note in trailers.misplaced_notes(rng, "Coupling", path):
        print(f"  {note}")


def unjustified(rng: str, path: str, rose) -> List[str]:
    """The commits of `rng` whose own step grows `path` (`rose` over its parsed texts) with no `Coupling:` trailer
    (trailers.py, THE ATTRIBUTION RULE)."""
    return trailers.unjustified_rises(rng, path, "Coupling", rose)


def print_unjustified(found: List[str]) -> None:
    print("  Each rise needs the trailer on the commit making it, not on another commit of the push; without one:")
    for u in found:
        print(f"  - {u}")


Fanouts = Tuple[Dict[str, int], Dict[str, Set[str]]]  # every class's held fan-out, and what it counts (when given)


def read_fanouts(path: str) -> Optional[Fanouts]:
    """couplingReport's fanouts file: `<class><TAB><held fan-out>[<TAB><target>,<target>...]` a line, every class; None
    without one."""
    if not path:
        return None
    counts, targets = {}, {}
    with open(path) as f:
        for cols in (line.split("\t") for line in f.read().splitlines() if "\t" in line):
            counts[cols[0]] = int(cols[1])
            if len(cols) > 2:
                targets[cols[0]] = set(filter(None, cols[2].split(",")))
    return counts, targets


def refs(current_path: str, fanouts_path: str) -> int:
    """Each frozen class's reference set, `<class><TAB><target>` a line, sorted (the module docstring's `refs`)."""
    with open(current_path) as f:
        frozen = of_kind(parse(f.read()), "fanout")
    _, targets = read_fanouts(fanouts_path)
    for cls in sorted(frozen):
        for target in sorted(targets.get(cls, ())):
            print(f"{cls}\t{target}")
    return 0


def renames_of(ranges: List[str], current: Rows, baseline: Rows) -> Tuple[Dict[Tuple[str, str], str], List[str], List[str]]:
    """The (kind, key) rows each range's renames carry over (A RENAME), from the base's baseline to the measured rows and
    to the recorded baseline, with the notes and refusals to say."""
    renamed: Dict[Tuple[str, str], str] = {}
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


def is_rename(name: str, named: List[str], before: Rows, renamed: Dict[Tuple[str, str], str]) -> bool:
    """Whether a `Coupling: split of <X> into <A>` claim is a rename the range already credits: X renamed to its one
    piece (a trailer written before rename credit, 2026-11-19, to get the row across)."""
    original = resolve(name, of_kind(before, "fanout"))
    targets = {new for (kind, new), old in renamed.items() if kind == "fanout" and old == original}
    return bool(targets) and len(named) == 1 and resolve(named[0], targets) is not None


def check(current_path: str, baseline_path: str, ranges: List[str], layers_path: str = LAYERS_PATH,
          edges_path: str = "", exemptions_path: str = EXEMPTIONS_PATH, fanouts_path: str = "") -> int:
    with open(current_path) as f:
        current = parse(f.read())
    with open(baseline_path) as f:
        baseline = parse(f.read())
    ranks: Dict[str, int] = {}
    if os.path.exists(layers_path):
        with open(layers_path) as f:
            ranks = parse_layers(f.read())[2]
    fanouts = read_fanouts(fanouts_path)
    edges: Edges = {}
    if edges_path:
        with open(edges_path) as f:
            edges = read_edges(f.read())
    failed = False
    recorded = sorted(of_kind(baseline, "cycle-edge"))
    if recorded:
        failed = True
        print(f"THE COUPLING BASELINE RECORDS A PACKAGE CYCLE ({baseline_path}): the package graph has none, and no "
              "`Coupling:` trailer lets one in:")
        for key in recorded:
            print(f"  - cycle-edge {key}")
    renamed, rename_notes, rename_refused = renames_of(ranges, current, baseline)
    for note in rename_notes:
        print(f"coupling: {note}")
    found = violations(current, baseline, ranks, edges, renamed)
    if found:
        failed = True
        print(f"COUPLING REFUSED (a package cycle, a layer or rank violation, or a row past {baseline_path}):")
        for v in found:
            print(f"  - {v}")
        print("  Cut the new dependency (scripts/coupling-report shows the class references behind each edge). Only a row past")
        print("  the baseline -- never a cycle, a layer or a rank violation -- may be recorded instead: scripts/coupling-report")
        print("  --update, committed with a `Coupling: <why this coupling is right>` trailer.")
    for r in rename_refused:
        print(f"  no rename credit: {r}")
    for rng in ranges:
        base = rng.split("..")[0]
        before = parse(file_at(base, BASELINE_PATH))
        claims = [(name, named) for name, named in split_claims(rng) if not is_rename(name, named, before, renamed)]
        split, split_packages, notes, refused = split_credit(rng, before, baseline, fanouts, claims)
        for note in notes:
            print(f"coupling: {note} -- each piece frozen at its own value")
        if refused:
            failed = True
            print(f"A `Coupling: split of <X>` CLAIM IS REFUSED in {rng}: the pieces must take over X's code, each below X's"
                  " frozen value, and reference (with what X keeps) nothing X did not:")
            for r in refused:
                print(f"  - {r}")
            print(SPLIT_RULE)
        frozen = frozen_growth(before, baseline, split, split_packages, renamed)
        if frozen:
            failed = True
            print(f"THE COUPLING BASELINE ADMITS A CLASS OR PACKAGE PAST A LIMIT in {rng}: a frozen row only falls, and no"
                  " trailer admits a new one -- cut it, or exempt it with its why in " + EXEMPTIONS_PATH + ":")
            for g in frozen:
                print(f"  - {g}")
            print_misplaced(rng)
        grown = growth(before, baseline)
        unjustified_growth = unjustified(rng, BASELINE_PATH, lambda a, b: growth(parse(a), parse(b))) if grown else []
        if unjustified_growth:
            failed = True
            print(f"THE COUPLING BASELINE GREW in {rng} with no `Coupling: <why>` trailer on the commit changing it:")
            for g in grown:
                print(f"  - {g}")
            print_unjustified(unjustified_growth)
            print_misplaced(rng, BASELINE_PATH)
        if os.path.exists(layers_path):
            with open(layers_path) as f:
                loosened = loosening(parse_layers(file_at(base, LAYERS_PATH)), parse_layers(f.read()))
            unjustified_layers = unjustified(rng, LAYERS_PATH, lambda a, b: loosening(parse_layers(a), parse_layers(b))) \
                if loosened else []
            if unjustified_layers:
                failed = True
                print(f"THE LAYER TABLE LOOSENED in {rng} ({layers_path}) with no `Coupling: <why>` trailer on the commit"
                      " changing it:")
                for item in loosened:
                    print(f"  - {item}")
                print_unjustified(unjustified_layers)
                print_misplaced(rng, LAYERS_PATH)
    if os.path.exists(exemptions_path):
        with open(exemptions_path) as f:
            oversized = oversized_contracts(parse_exemptions(f.read()))
        if oversized:
            failed = True
            print(f"A DECLARED CONTRACT IS TOO LARGE ({exemptions_path}):")
            for item in oversized:
                print(f"  - {item}")
    for rng in ranges:
        base = rng.split("..")[0]
        if os.path.exists(exemptions_path):
            with open(exemptions_path) as f:
                loose = exemption_loosening(parse_exemptions(file_at(base, EXEMPTIONS_PATH)), parse_exemptions(f.read()))
            unjustified_exemptions = unjustified(rng, EXEMPTIONS_PATH, lambda a, b: exemption_loosening(
                parse_exemptions(a), parse_exemptions(b))) if loose else []
            if unjustified_exemptions:
                failed = True
                print(f"THE EXEMPTION TABLE LOOSENED in {rng} ({exemptions_path}) with no `Coupling: <why>` trailer on the"
                      " commit changing it:")
                for item in loose:
                    print(f"  - {item}")
                print_unjustified(unjustified_exemptions)
                print_misplaced(rng, EXEMPTIONS_PATH)
    for rng in ranges:
        unadmitted = unadmitted_exemptions(rng)
        if unadmitted:
            failed = True
            print(f"A FAN-OUT EXEMPTION ROW IS NOT ADMITTED in {rng} ({exemptions_path}): a row added carries a specific why, its"
                  " `added` date and the commit's own `Coupling: exempt <Class> — <reason>` trailer:")
            for item in unadmitted:
                print(f"  - {item}")
            print(f"  The why: {REASON_SHAPE}.")
            print_misplaced(rng, EXEMPTIONS_PATH)
    down = falls(current, baseline)
    if down:
        print(f"coupling fell ({len(down)} rows below the baseline, not yet recorded: scripts/coupling-report --update"
              " records them, no trailer needed):")
        for d in down[:20]:
            print(f"  - {d}")
        if len(down) > 20:
            print(f"  - ... and {len(down) - 20} more")
    if not failed:
        print(f"coupling: within {baseline_path} ({len(current)} rows)")
    return 1 if failed else 0


SPLIT_RULE = ("  The rule (the union rule, 0545f3a27; the D tolerance, 2cfa0ee95): every piece strictly below X's frozen fan-out,"
              " each taking over X's code,\n  the UNION of what the pieces and X's remainder reference within X's"
              f" reference set at the base ({REFS_PATH}),\n  and a distance row of a package the split holds or reaches"
              f" rising by {DISTANCE_SPLIT_TOLERANCE} hundredths at most, and never past its ceiling\n  (its `{DISTANCE_CAP}` row: the"
              " 2026-11-19 value + 0.02, only falling). Check a split before committing it:\n"
              "  scripts/coupling-report --check-split <X> <piece>...")


def file_text(path: str) -> str:
    with open(path) as f:
        return f.read()


def snapshot() -> str:
    """A commit of the working tree (tracked and untracked, .gitignore respected) on top of HEAD, built in a copy of
    the index: no ref, the index and the tree untouched."""
    import shutil
    import tempfile
    index = git("rev-parse", "--git-path", "index").strip()
    with tempfile.TemporaryDirectory() as d:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(d, "index"))
        if os.path.exists(index):
            shutil.copy(index, env["GIT_INDEX_FILE"])
        run = lambda *a: subprocess.run(["git", *a], check=True, capture_output=True, text=True, env=env).stdout.strip()
        run("add", "-A")
        return run("commit-tree", run("write-tree"), "-p", "HEAD", "-m", "split-check: the working tree")


def default_base() -> str:
    found = subprocess.run(["git", "merge-base", "HEAD", "origin/main"], capture_output=True, text=True)
    return found.stdout.strip() if found.returncode == 0 else git("rev-parse", "HEAD").strip()


def split_check(current_path: str, fanouts_path: str, original: str, pieces: List[str], base: str = "") -> int:
    """`split-check` (the module docstring): the ratchet's own split_credit and frozen_growth over base..<the working
    tree>, explained."""
    base = base or default_base()
    before = parse(file_at(base, BASELINE_PATH))
    measured = parse(with_ceilings(file_text(current_path), [before]))  # what --update would write
    fanouts = read_fanouts(fanouts_path)
    rng = f"{base}..{snapshot()}"
    explain: List[dict] = []
    admitted, packages, notes, refused = split_credit(rng, before, measured, fanouts, [(original, pieces)], explain)
    if explain:  # the names as the ratchet resolved them (a simple name to its one class)
        original, pieces = explain[0]["original"], explain[0]["pieces"]
    print(f"split-check: {original} into {', '.join(pieces)}, the working tree against {base[:12]}")
    for e in explain:
        print(f"  {e['original']}: frozen at {e['frozen']} at the base, {len(e['recorded'])} references recorded ({REFS_PATH})")
        counts = (fanouts or ({}, {}))[0]
        if e["original"] in counts:
            print(f"  {e['original']} (what it keeps): fan-out {counts[e['original']]}")
        for piece in e["pieces"]:
            v = e["values"].get(piece)
            mark = "no class" if v is None else ("below" if v < e["frozen"] else "NOT below")
            print(f"  piece {piece}: fan-out {'-' if v is None else v} -- {mark} {e['frozen']}")
        print(f"  union of references: {len(e['union'])}, of them outside {e['original']}'s set: {len(e['new'])}"
              + (f" ({', '.join(e['new'][:5])}{', ...' if len(e['new']) > 5 else ''})" if e["new"] else " (a subset)"))
    reached = packages or ({package_of(c) for c in pieces + [original]})
    old_d, new_d = of_kind(before, "distance"), of_kind(measured, "distance")
    caps = ceilings(before)
    for pkg in sorted(set(old_d) | set(new_d)):
        a, b = old_d.get(pkg), new_d.get(pkg)
        room = f"ceiling {caps[pkg]}, headroom {(caps[pkg] - (a if b is None else b)) / 100:.2f} left" if pkg in caps else ""
        if a == b:
            if pkg in reached:
                print(f"  distance {pkg}: {a} -- {room}")
            continue
        if a is None:
            verdict = "NEW past the limit: refused"
        elif b is None or b < a:
            verdict = "fell"
        elif pkg in packages and b - a <= DISTANCE_SPLIT_TOLERANCE and b <= caps[pkg]:
            verdict = f"rose within the split's {DISTANCE_SPLIT_TOLERANCE}: tolerated"
        elif pkg in packages and b - a <= DISTANCE_SPLIT_TOLERANCE:
            verdict = f"rose past its ceiling: refused -- free {(b - caps[pkg]) / 100:.2f} of its D: {DISTANCE_CUT}"
        else:
            verdict = "rose: refused" + ("" if pkg in reached else " (a package the split does not reach)")
        print(f"  distance {pkg}: {'-' if a is None else a} -> {'-' if b is None else b} -- {verdict}"
              + (f"; {room}" if room and a is not None else ""))
    grown = frozen_growth(before, measured, admitted, packages)
    ours = {original, *pieces} | reached
    mine = [g for g in grown if g.split(" ")[1] in ours]
    others = [g for g in grown if g not in mine]
    for note in notes:
        print(f"ACCEPTED: {note}")
    for r in refused:
        print(f"REFUSED: {r}")
    for g in mine:
        print(f"REFUSED: {g}")
    if others:
        print("  (the range also holds rows outside this split the ratchet would refuse: " + "; ".join(others[:5])
              + (", ..." if len(others) > 5 else "") + ")")
    if refused or mine:
        print(SPLIT_RULE)
        return 1
    print(f"commit it with: Coupling: split of {original} into {', '.join(pieces)}"
          " (after scripts/coupling-report --update records the pieces' rows)")
    return 0


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
    n = sub.add_parser("new-exemptions")
    n.add_argument("--since", default="7d")
    n.add_argument("--rev", default="")
    r = sub.add_parser("refs")
    r.add_argument("--current", required=True)
    r.add_argument("--fanouts", required=True)
    args = parser.parse_args(argv)
    try:
        if args.command == "update":
            return update(args.current, args.baseline)
        if args.command == "new-exemptions":
            return new_exemptions(args.since, args.rev)
        if args.command == "refs":
            return refs(args.current, args.fanouts)
        if args.command == "split-check":
            return split_check(args.current, args.fanouts, args.split, args.into, args.base)
        return check(args.current, args.baseline, args.range, args.layers, args.edges, args.exemptions, args.fanouts)
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"coupling_ratchet: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
