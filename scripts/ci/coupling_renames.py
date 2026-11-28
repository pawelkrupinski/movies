"""What git says a range renamed, for the coupling ratchet's RENAME CREDIT (coupling_ratchet.py, A RENAME): the range's
`git diff -M` renames and copies, the commits scripts/pure-move-check calls PURE, the source file declaring a class, and
the frozen rows (a class's fan-out, a package's distance) that carry over to a new name."""
from __future__ import annotations

import os
import re
import subprocess
import sys
from typing import Dict, List, Optional, Set, Tuple

import ci_machine
from processes import tool_python
from coupling_rows import CLASS_KINDS, LAYERS_PATH, layer_units, moved, package_of
from trailers import text_at as file_at
import tool_git

HERE = os.path.dirname(os.path.abspath(__file__))
# scripts/pure-move-check's code; COUPLING_PURE_MOVE_CHECK names it for a copy of this tool elsewhere (the mutation test's)


def pure_move_check() -> str:
    return (ci_machine.environment().get("COUPLING_PURE_MOVE_CHECK")
            or os.path.join(os.path.dirname(HERE), "pure_move_check.py"))


DECLARES = re.compile(r"\b(?:def|val|var|type|class|object|trait|enum)\s+([A-Za-z_][A-Za-z0-9_]*)")
MIN_MOVED_MEMBERS = 2

Pairs = Set[Tuple[str, str]]


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


def name_status_pairs(*args: str) -> List[Tuple[str, str, str]]:
    """The (status letter, from, to) of each rename (R) or copy (C) in `git diff --name-status <args>`."""
    status = tool_git.Git.here().text("diff", *args, "--name-status")
    return [(cols[0][:1], cols[1], cols[2]) for cols in (line.split("\t") for line in status.splitlines())
            if len(cols) == 3 and cols[0][:1] in "RC"]


class RenameEvidence:
    """What git says a range renamed (A RENAME, the module docstring), read once per range and only when a rename is in
    question: the range's `git diff -M` renames, and the commits scripts/pure-move-check calls PURE."""

    def __init__(self, rng: str):
        self.base, head = rng.split("..", 1)
        self.head = head or "HEAD"
        self.rng = f"{self.base}..{self.head}"
        self._changes: Optional[Tuple[Pairs, Pairs]] = None
        self._pure: Dict[str, bool] = {}
        self._sources: Dict[Tuple[str, str], Optional[str]] = {}
        self._packages: Dict[Tuple[str, str], str] = {}
        self._units: Dict[str, Set[str]] = {}

    def _read(self) -> Tuple[Pairs, Pairs]:
        if self._changes is None:
            pairs = name_status_pairs("-M", "-C", self.base, self.head)
            self._changes = ({(a, b) for s, a, b in pairs if s == "R"}, {(a, b) for _, a, b in pairs})
        return self._changes

    def renames(self) -> Pairs:
        return self._read()[0]

    def copies(self) -> Pairs:
        return self._read()[1]

    def source(self, rev: str, cls: str) -> Optional[str]:
        if (rev, cls) not in self._sources:
            self._sources[(rev, cls)] = source_of(rev, cls)
        return self._sources[(rev, cls)]

    def package(self, rev: str, path: str) -> str:
        """The package of source `path` at `rev`, as the ratchet measured it there (its layer table's units)."""
        if rev not in self._units:
            self._units[rev] = layer_units(file_at(rev, LAYERS_PATH))
        if (rev, path) not in self._packages:
            declared = ".".join(re.findall(r"^package\s+([\w.]+)", file_at(rev, path), re.M))
            self._packages[(rev, path)] = package_of(declared + ".X", self._units[rev])
        return self._packages[(rev, path)]

    def pure(self, sha: str) -> bool:
        """Whether scripts/pure-move-check calls commit `sha` a PURE MOVE (exit 0; NOT PURE and undecidable are not)."""
        if sha not in self._pure:
            done = subprocess.run(tool_python.isolated(pure_move_check(), f"{sha}^..{sha}", "--quiet", python=sys.executable), capture_output=True, text=True)
            self._pure[sha] = done.returncode == 0
        return self._pure[sha]

    def commits(self, *paths: str) -> List[str]:
        return tool_git.Git.here().text("rev-list", "--reverse", self.rng, "--", *paths).split() if paths else []

    def pure_commit(self, origin: str, old: str, new: str) -> str:
        """The commit touching `origin` after which `new` is declared and `old` no longer is, that is a PURE MOVE ("" for none)."""
        for sha in self.commits(origin):
            was, now = self.source(f"{sha}^", old), self.source(sha, new)
            if was and now and not self.source(sha, old) and (was == now or (was, now) in self.commit_renames(sha)) \
                    and self.pure(sha):
                return sha
        return ""

    def class_renamed(self, old: str, new: str) -> str:
        """How the range renamed class `old` to `new` ("" when it did not): git's rename of the one's source to the
        other's, or a PURE commit after which `new` is declared and `old` no longer is."""
        origin, dest = self.source(self.base, old), self.source(self.head, new)
        if origin is None or dest is None:
            return ""
        if (origin, dest) in self.renames():
            return f"git renames {origin} to {dest}"
        sha = self.pure_commit(origin, old, new)
        return f"scripts/pure-move-check calls {sha[:9]} a PURE MOVE" if sha else ""

    def split_away(self, origin: Optional[str], dest: Optional[str]) -> List[str]:
        """The other production sources of the range that took over code of `origin` renamed to `dest`: a copy of it
        (git's detection), or a file gaining MIN_MOVED_MEMBERS+ of the members `origin` declared and `dest` does not --
        whatever their fan-out (a piece below the limit is a piece too)."""
        if not origin or not dest:
            return []
        left = members(file_at(self.base, origin)) - members(file_at(self.head, dest))
        changed = tool_git.Git.here().text("diff", "--name-only", "--diff-filter=ACMR", "-M", self.base, self.head, "--", "*/src/main/*.scala").split()
        out: List[str] = []
        for path in changed:
            if path == dest:
                continue
            gained = members(file_at(self.head, path)) - members(file_at(self.base, path))
            if (origin, path) in self.copies() or len(gained & left) >= MIN_MOVED_MEMBERS:
                out.append(path)
        return out

    def commit_renames(self, sha: str) -> Pairs:
        return {(a, b) for s, a, b in name_status_pairs("-M", f"{sha}^", sha) if s == "R"}

    def package_renamed(self, old: str, new: str) -> str:
        """How the range renamed package `old` to `new` ("" when it did not): every source of `old` git (or a PURE
        commit) renames lands in `new`, and at least one does."""
        if self.lands_in(self.renames(), self.base, self.head, old, new):
            return "git renames its sources"
        for sha in self.commits(*sorted({a for a, _ in self.renames()} | {a for a, _ in self.copies()})):
            if self.lands_in(self.commit_renames(sha), f"{sha}^", sha, old, new) and self.pure(sha):
                return f"scripts/pure-move-check calls {sha[:9]} a PURE MOVE"
        return ""

    def lands_in(self, pairs: Pairs, before: str, after: str, old: str, new: str) -> bool:
        """Whether the renamed sources of package `old` (at `before`) all land in package `new` (at `after`), at least one."""
        landed = [self.package(after, b) for a, b in pairs if a.endswith(".scala") and self.package(before, a) == old]
        return bool(landed) and all(p == new for p in landed)


def rename_claims(kind: str, gone: Dict[str, int], new: Dict[str, int],
                  evidence: RenameEvidence) -> Dict[str, List[Tuple[str, str]]]:
    """Each old name -> the (new name, how) pairs the range's renames give it."""
    renamed = evidence.class_renamed if kind in CLASS_KINDS else evidence.package_renamed
    claims: Dict[str, List[Tuple[str, str]]] = {}
    for key in sorted(new):
        for old in sorted(gone):
            how = renamed(old, key)
            if how:
                claims.setdefault(old, []).append((key, how))
    return claims


def judge_claim(kind: str, old: str, found: List[Tuple[str, str]], gone: Dict[str, int], new: Dict[str, int],
                evidence: RenameEvidence) -> Tuple[Optional[str], str]:
    """One old name's claims: (its new name, the note) when credited, (None, why not) when refused."""
    names = [k for k, _ in found]
    if len(found) > 1:
        return None, (f"{kind} {old} ({gone[old]}) went to {', '.join(names)}: a rename has one new name -- several is "
                      f"a split (`Coupling: split of {old}`, scripts/coupling-report --check-split)")
    key, how = found[0]
    if new[key] > gone[old]:
        return None, (f"{kind} {key} is a rename of {old} ({how}) but {new[key]} is past {old}'s frozen row {gone[old]}"
                      " -- a rename carries a row at or below its value, never above")
    if kind == "fanout":
        others = evidence.split_away(evidence.source(evidence.base, old), evidence.source(evidence.head, key))
        if others:
            return None, (f"fanout {old} ({gone[old]}) renamed to {key}, but {', '.join(others)} also took over its code:"
                          f" a split posing as a rename (`Coupling: split of {old}`, scripts/coupling-report --check-split)")
    return key, f"{kind} {old} ({gone[old]}) renamed to {key} ({how}): its frozen row carries over at {new[key]}"


def rename_credit(kind: str, before: Dict[str, int], after: Dict[str, int],
                  evidence: RenameEvidence) -> Tuple[Dict[str, str], List[str], List[str]]:
    """The rows of `after` (of a FROZEN `kind`) that are renames of a row of `before` (A RENAME, the module docstring):
    new name -> old name, with a note for each credited and why not for each refused. A row of the same simple name is
    `moved`'s."""
    gone = {k: v for k, v in before.items() if k not in after}
    new = {k: v for k, v in after.items() if k not in before and not moved(k, v, before, after)}
    if not gone or not new:
        return {}, [], []
    credited: Dict[str, str] = {}
    notes: List[str] = []
    refused: List[str] = []
    for old, found in sorted(rename_claims(kind, gone, new, evidence).items()):
        key, said = judge_claim(kind, old, found, gone, new, evidence)
        if key is None:
            refused.append(said)
        else:
            credited[key] = old
            notes.append(said)
    return credited, notes, refused
