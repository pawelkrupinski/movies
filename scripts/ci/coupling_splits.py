"""The coupling ratchet's SPLIT rule (coupling_ratchet.py, A SPLIT): which `Coupling: split of <X>` claims of a range are
honest -- every piece takes over X's code, each below X's frozen fan-out, and the union of what the pieces and X's
remainder reference within X's frozen reference set at the base -- and the packages whose distance they may raise."""
from __future__ import annotations

import os
import re
import shutil
import tempfile
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Set, Tuple

from coupling_renames import MIN_MOVED_MEMBERS, Pairs, members, name_status_pairs, source_of
from coupling_rows import (DISTANCE_CAP, DISTANCE_SPLIT_TOLERANCE, LAYERS_PATH, REFS_PATH, Fanouts, Rows,
                           layer_units, moved, of_kind, package_of, recorded_refs)
from trailers import text_at as file_at
import tool_git

SPLIT_TRAILER = re.compile(r"split of\s+(\S+)(?:\s+into\s+(.+))?$")
Claims = List[Tuple[str, List[str]]]
SPLIT_RULE = ("  The rule (the union rule, 0545f3a27; the D tolerance, 2cfa0ee95): every piece strictly below X's frozen fan-out,"
              " each taking over X's code,\n  the UNION of what the pieces and X's remainder reference within X's"
              f" reference set at the base ({REFS_PATH}),\n  and a distance row of a package the split holds or reaches"
              f" rising by {DISTANCE_SPLIT_TOLERANCE} hundredths at most, and never past its ceiling\n  (its `{DISTANCE_CAP}` row: the"
              " 2026-11-19 value + 0.02, only falling). Check a split before committing it:\n"
              "  scripts/coupling-report --check-split <X> <piece>...")


@dataclass
class SplitExplanation:
    """What the judgement of one `split of <X>` claim read: X, its frozen row, the pieces and their fan-outs, the union
    of the references, X's recorded set, what the union adds, and why the claim is refused (empty: honest)."""
    original: str
    frozen: int
    pieces: List[str]
    values: Dict[str, int]
    union: Set[str]
    recorded: Set[str]
    new: List[str]
    why: List[str]


def split_claims(rng: str) -> Claims:
    """The `Coupling: split of <X> [into <A>, <B>]` trailers of the commits of `rng`: (X, the pieces named)."""
    log = tool_git.Git.here().text("log", "--format=%x00%(trailers:key=Coupling,valueonly,unfold,separator=%x1e)", rng)
    claims: Claims = []
    for value in (v.strip() for entry in log.split("\x00") for v in entry.split("\x1e")):
        m = SPLIT_TRAILER.match(value)
        if m:
            claims.append((m.group(1), [p.strip() for p in (m.group(2) or "").split(",") if p.strip()]))
    return claims


def resolve(name: str, known: Iterable[str]) -> Optional[str]:
    """`name` among `known` class names: itself, or the one class of that simple name."""
    names = list(known)
    if name in names:
        return name
    same = [k for k in names if k.rsplit(".", 1)[-1] == name]
    return same[0] if len(same) == 1 else None


def took_over(base: str, head: str, origin: str, piece: str, copies: Pairs) -> Tuple[bool, str]:
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
    moved_away = members(file_at(base, origin)) - members(file_at(head, origin))
    taken = (members(file_at(head, path)) - members(file_at(base, path))) & moved_away
    return len(taken) >= MIN_MOVED_MEMBERS, ""


class SplitContext:
    """What judging a range's split claims reads: the frozen and measured fan-out rows, the held counts and targets
    (`fanouts`), X's recorded reference sets at the base, git's copies over the range."""

    def __init__(self, rng: str, before: Rows, after: Rows, fanouts: Optional[Fanouts], has_claims: bool):
        self.rng = rng
        self.base, head = rng.split("..", 1)
        self.head = head or "HEAD"
        self.frozen, self.now = of_kind(before, "fanout"), of_kind(after, "fanout")
        self.counts, self.targets = fanouts or ({}, {})
        self.base_refs = recorded_refs(file_at(self.base, REFS_PATH)) if has_claims else {}
        self._copies: Optional[Pairs] = None

    def copies(self) -> Pairs:
        if self._copies is None:
            self._copies = {(a, b) for _, a, b in name_status_pairs("-M", "-C", self.base, self.head)}
        return self._copies

    def added(self) -> List[str]:
        """The fanout rows the range adds (no frozen row, no move)."""
        return sorted(k for k in self.now if k not in self.frozen and not moved(k, self.now[k], self.frozen, self.now))

    def piece_problems(self, original: str, piece: str, origin: Optional[str]) -> Tuple[List[str], Optional[int], Set[str]]:
        """Why a piece fails (no class, not below X, takes none of X's code, unmeasured), its fan-out, its references."""
        value = self.now.get(piece, self.counts.get(piece))
        if value is None:
            return [f"{piece} is no class at {self.head}"], None, set()
        why: List[str] = []
        if value >= self.frozen[original]:
            why.append(f"{piece} {value} is not below {original}'s {self.frozen[original]}")
        ok, because = took_over(self.base, self.head, origin, piece, self.copies()) if origin else (False, "")
        if not ok:
            why.append(f"{piece} takes over none of {original}'s code" + (f" ({because})" if because else ""))
        if piece in self.targets:
            return why, value, self.targets[piece]
        why.append(f"{piece}'s references are not measured (couplingReport's fanouts, with their targets)")
        return why, value, set()

    def judge(self, original: str, pieces: List[str]) -> SplitExplanation:
        """The judgement of X's split into `pieces`: the explanation, its `why` empty when the claim is honest."""
        why: List[str] = []
        values: Dict[str, int] = {}
        union: Set[str] = set()
        origin = source_of(self.base, original)
        for piece in pieces:
            problems, value, refs = self.piece_problems(original, piece, origin)
            why += problems
            union |= refs
            if value is not None:
                values[piece] = value
        kept, kept_why = self.kept_references(original)
        union = (union | kept) - (set(pieces) | {original})
        why += kept_why
        if not pieces:
            why.append("no pieces: name them (`into <A>, <B>`) or record their rows (scripts/coupling-report --update)")
        was = self.base_refs.get(original, set())
        if len(was) != self.frozen[original]:
            why.append(f"{original}'s recorded reference set at {self.base[:12]} ({REFS_PATH}) has {len(was)}, its row "
                       f"{self.frozen[original]}: record both with scripts/coupling-report --update in an earlier commit")
        new = sorted(t for t in union if t not in was)
        if new:
            why.append(f"the pieces add what {original} did not reference at {self.base[:12]}: {', '.join(new[:5])}"
                       + (", ..." if len(new) > 5 else "") + " -- a split adds no dependency")
        return SplitExplanation(original, self.frozen[original], pieces, values, union, was, new, why)

    def kept_references(self, original: str) -> Tuple[Set[str], List[str]]:
        """What X keeps references (when X is measured), and why not when its targets are not."""
        if original not in self.counts:
            return set(), []
        if original in self.targets:
            return self.targets[original], []
        return set(), [f"what {original} keeps is not measured (couplingReport's fanouts, with its targets)"]


def claim_refusal(ctx: SplitContext, name: str, named: List[str], claims: Claims) -> Tuple[Optional[str], str]:
    """The class a claim names, or why it is refused before its pieces are judged."""
    original = resolve(name, ctx.frozen)
    if original is None:
        return None, f"split of {name}: {name} has no frozen fanout row at {ctx.base[:12]}"
    if not named and len(claims) > 1:
        return None, f"split of {original}: {len(claims)} splits in {ctx.rng} -- name each one's pieces (`into <A>, <B>`)"
    return original, ""


def split_credit(rng: str, before: Rows, after: Rows, fanouts: Optional[Fanouts],
                 claims: Optional[Claims] = None,
                 explain: Optional[List[SplitExplanation]] = None) -> Tuple[Set[str], Set[str], List[str], List[str]]:
    """The fanout rows of `after` that `rng`'s `Coupling: split of <X>` trailers honestly admit and the packages whose
    distance they may raise, with a note for each split admitted and a reason for each refused (the rule: the module
    docstring, A SPLIT). `claims` judges those splits instead of the range's trailers (`split-check`, before any commit
    carries one); `explain` collects, per claim, what the judgement read."""
    claims = split_claims(rng) if claims is None else claims
    ctx = SplitContext(rng, before, after, fanouts, bool(claims))
    admitted: Set[str] = set()
    packages: Set[str] = set()
    notes: List[str] = []
    refused: List[str] = []
    for name, named in claims:
        original, refusal = claim_refusal(ctx, name, named, claims)
        if original is None:
            refused.append(refusal)
            continue
        known = set(ctx.now) | set(ctx.counts)
        pieces = [resolve(n, known) or n for n in named] or ctx.added()
        verdict = ctx.judge(original, pieces)
        if explain is not None:
            explain.append(verdict)
        shown = ", ".join(f"{p} {v}" for p, v in verdict.values.items())
        summary = f"split of {original} ({verdict.frozen}) into {shown}"
        if verdict.why:
            refused.append(f"{summary}: " + "; ".join(verdict.why))
            continue
        admitted |= set(pieces)
        packages |= reached_packages(ctx.head, pieces, original, verdict.union)
        notes.append(f"{summary}: their references ({len(verdict.union)}) within {original}'s {verdict.frozen}")
    return admitted, packages, notes, refused


def reached_packages(head: str, pieces: List[str], original: str, union: Set[str]) -> Set[str]:
    """The packages holding X or a piece, and those the split's references reach: each piece is one more outside
    class using them (Ca counts classes), so their instability falls and their distance may rise."""
    units = layer_units(file_at(head, LAYERS_PATH))
    internal = set(pieces) | {original}
    return {package_of(c, units) for c in internal} | {package_of(t, units) for t in union}


def snapshot() -> str:
    """A commit of the working tree (tracked and untracked, .gitignore respected) on top of HEAD, built in a copy of
    the index: no ref, the index and the tree untouched."""
    index = tool_git.Git.here().text("rev-parse", "--git-path", "index").strip()
    with tempfile.TemporaryDirectory() as d:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(d, "index"))
        if os.path.exists(index):
            shutil.copy(index, env["GIT_INDEX_FILE"])
        git = tool_git.Git(tool_git.shared_runner(), env, ".")
        git.text("add", "-A")
        return git.text("commit-tree", git.text("write-tree").strip(), "-p", "HEAD", "-m",
                        "split-check: the working tree").strip()


def default_base() -> str:
    git = tool_git.Git.here()
    return git.line("merge-base", "HEAD", "origin/main") or git.text("rev-parse", "HEAD").strip()
