"""THE PURE MOVE CHECK'S NORMAL FORM (scripts/pure_move_check.py): a Scala (or twirl) source with its comments dropped,
`package` clauses and imports set aside, every moved name on the old side mapped to its new one, and whitespace runs
collapsed. Stdlib only, beside pure_move_check.py."""
from __future__ import annotations

import functools
import re
from dataclasses import dataclass
from typing import Dict, FrozenSet, List, Optional, Set, Tuple

from pure_move_lexer import Piece, segments
from pure_move_moves import Moves
from pure_move_source import PACKAGE_CLAUSE, is_production, packages_of

IMPORT_LINE = re.compile(r"^\s*@?import\s")
NOWARN = re.compile(r"@nowarn(\([^)]*\))?\s*")
Pairs = List[Tuple[str, str]]


@functools.lru_cache(maxsize=None)
def boundary(old: str) -> "re.Pattern[str]":
    return re.compile(r"(?:(?<=_root_\.)|(?<![\w.$]))" + re.escape(old) + r"(?![\w$])")


@dataclass
class Normalised:
    lines: List[str]
    imports: Set[str]
    packages: List[str]


def _top_level_parts(body: str) -> List[str]:
    """`body` split at its commas outside braces."""
    parts: List[str] = []
    depth, cur = 0, ""
    for ch in body:
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append(cur)
            cur = ""
        else:
            cur += ch
    parts.append(cur)
    return parts


def _expanded(part: str) -> List[str]:
    """`a.b.{C, D => E}` -> ['a.b.C', 'a.b.D => E']; any other part as it is (whitespace collapsed), none when empty."""
    part = re.sub(r"\s+", " ", part.strip())
    m = re.match(r"^([\w.`]+)\.\{(.*)\}$", part)
    if m:
        return [f"{m.group(1)}.{s.strip()}" for s in m.group(2).split(",") if s.strip()]
    return [part] if part else []


def parse_imports(stmt: str) -> List[str]:
    """`import a.b.{C, D => E}, f.G` -> ['a.b.C', 'a.b.D => E', 'f.G']"""
    body = stmt.strip()
    body = re.sub(r"^@?import\s+", "", body).rstrip(";").strip()
    return [entry for part in _top_level_parts(body) for entry in _expanded(part)]


def _remap(target: str, rest: str, table: Dict[str, str]) -> Optional[str]:
    for old, new in sorted(table.items(), key=lambda p: -len(p[0])):
        if target == old or target.startswith(old + "."):
            return (new + target[len(old):] + (" " + rest if rest else "")).strip()
    return None


def map_name(entry: str, moves: Moves) -> str:
    """An import entry's target mapped: a moved class, else a moved package prefix."""
    target, _, rest = entry.partition(" ")
    for table in (moves.classes, moves.packages):
        mapped = _remap(target, rest, table)
        if mapped is not None:
            return mapped
    return entry


@functools.lru_cache(maxsize=64)
def _longest_first(names: FrozenSet[str]) -> Tuple[str, ...]:
    return tuple(sorted(names, key=lambda name: (-len(name), name)))


@functools.lru_cache(maxsize=64)
def _alternation(prefix: str, olds: Tuple[str, ...], suffix: str) -> "re.Pattern[str]":
    return re.compile(prefix + "(" + "|".join(map(re.escape, olds)) + ")" + suffix)


def map_text(text: str, pairs: Pairs, simple: Dict[str, str]) -> str:
    """Every old name in `text` as its new one, in one pass (the longest old name first at each place)."""
    if pairs:
        table = dict(pairs)
        olds = _longest_first(frozenset(table))
        text = _alternation(r"(?:(?<=_root_\.)|(?<![\w.$]))", olds, r"(?![\w$])").sub(lambda m: table[m.group(1)], text)
    if simple:
        text = _alternation(r"\b", _longest_first(frozenset(simple)), r"\b").sub(
            lambda m: simple[m.group(1)], text)
    return text


def unqualify(code: str, moves: Moves) -> str:
    """A moved class named by its new fully-qualified name, as its simple name: a file that named it bare while it
    shared its package names it qualified now, and both name the one class (a clash is the compiler's to refuse)."""
    fqns = _longest_first(frozenset(f for f in moves.classes.values() if "." in f))
    if not fqns:
        return code
    return _alternation(r"(?<![\w.$])(?:_root_\.)?", fqns, r"(?![\w$])").sub(lambda m: m.group(1).rsplit(".", 1)[1], code)


def relative_pairs(src: str, moves: Moves) -> Pairs:
    """A moved class named relative to a package the old file sits in (`adapters.TmdbStore` in package services.identity,
    or under the chained clauses `package services.identity` / `package adapters`) -> its new fully-qualified name."""
    clauses = packages_of(src)
    visible = [".".join(clauses[:k]) for k in range(1, len(clauses) + 1)]
    pairs: Pairs = []
    for old, new in moves.classes.items():
        for visible_package in visible:
            if old.startswith(visible_package + ".") and "." in old[len(visible_package) + 1:]:
                pairs.append((old[len(visible_package) + 1:], new))
    return pairs


def _normalised_code(text: str, moves: Moves) -> str:
    text = "\n".join(part if IMPORT_LINE.match(part) else unqualify(part, moves) for part in text.split("\n"))
    # a compiler-warning filter compiles to nothing: a move's `@nowarn("msg=unused import")` scope is no code
    text = NOWARN.sub("", text)
    return re.sub(r"(?<=\S)[ \t]+", " ", text)


def _normalised_piece(piece: Piece, old_side: bool, production: bool, pairs: Pairs, moves: Moves) -> str:
    kind, text = piece
    if kind == "comment":
        return re.sub(r"[^\n]", "", text) or " " if text.startswith("/*") else ""
    if old_side and (kind == "code" or not production):
        text = map_text(text, pairs, moves.simple)
    return _normalised_code(text, moves) if kind == "code" else text


def _import_statement(raw: List[str], i: int, first: str) -> Tuple[str, int]:
    """The import statement starting at raw[i] (`first`, stripped), continued over the lines of an open brace; and the
    index of the line after it."""
    stmt = first
    while stmt.count("{") > stmt.count("}") and i + 1 < len(raw):
        i += 1
        stmt += " " + raw[i].strip()
    return stmt, i + 1


def _split_lines(text: str) -> Normalised:
    lines: List[str] = []
    imports: Set[str] = set()
    packages: List[str] = []
    raw = text.splitlines()
    i = 0
    while i < len(raw):
        line = raw[i].rstrip()
        stripped = line.strip()
        clause = PACKAGE_CLAUSE.match(line)
        if clause:
            packages.append(clause.group(1))
            i += 1
        elif IMPORT_LINE.match(stripped):
            stmt, i = _import_statement(raw, i, stripped)
            imports.update(e for e in parse_imports(stmt) if e != "scala.annotation.nowarn")
        else:
            if stripped:
                lines.append(line)
            i += 1
    return Normalised(lines, imports, packages)


def normalise_scala(src: str, path: str, old_side: bool, moves: Moves, twirl: bool = False) -> Normalised:
    pairs, production = moves.path_pairs(), is_production(path)
    if old_side and not twirl:
        pairs = sorted(set(pairs + relative_pairs(src, moves)), key=lambda p: -len(p[0]))
    pieces: List[Piece] = [("code", src)] if twirl else segments(src)
    text = "".join(_normalised_piece(piece, old_side, production, pairs, moves) for piece in pieces)
    return _split_lines(text)
