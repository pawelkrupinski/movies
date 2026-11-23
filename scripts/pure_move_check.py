#!/usr/bin/env python3
"""THE PURE MOVE CHECK: whether <base>..<head> only moves code -- files renamed, packages and classes renamed -- so no
take, and no other behaviour, can move, and scripts/identity-what-if (which cannot compile a base across a package
rename) has nothing to measure. The owner, 2026-11-18: a pure move needs no what-if.

  1. THE MOVES: `git diff -M -C --name-status` over every first-parent hop of the range, composed hop by hop (a file
     renamed twice is one move), with the direct base..head pairs filling what the hops saw as a delete and an add.
     From the moved Scala files: the class map (each top-level name's old fully-qualified name -> its new one, a file
     renamed with its class renaming that class) and the package map (a package whose every file moved to one package).
  2. EACH CHANGED SCALA FILE, normalised on both sides: comments dropped; `package` clauses and imports set aside; every
     moved name on the old side mapped to its new one (dotted and slashed, `_root_.` too) -- in code, and in string
     literals only outside production (`<module>/src/main/`): a production literal naming a class is data, so its edit is
     never a move; whitespace runs collapsed (indentation kept: Scala 3 reads it), blank lines dropped. The two must be
     identical. The imports are held to what a move needs: an import the old side had (mapped); one added from the
     file's old package (it moved out) or of a moved class's new name (or package); one removed of a class now in the
     file's own package, or of a name the file no longer uses. An added wildcard, given or renaming import is no move
     (it can shadow a name). A twirl template (`*.scala.html`) is compared as text, its `@import`s as imports.
     A qualified access widened or narrowed (`private[identity]` -> `private[services]`, both public in bytecode) on a
     line declaring no implicit, given or extension and no overloaded name is reported as VISIBILITY, and is pure.
  3. NON-CODE REFERENCE EDITS are pure, reported apart: any other text file whose edit is the moved paths and names
     (path lists in tool configs and scripts, workflows, the identity tools' path lists) -- the old side, every moved
     path, package and class mapped, equals the new; and the reference lists, whatever their edit -- Markdown, docs/,
     .claude/, the AOT class list (aot-classes.txt) and the coupling, complexity, duplication and length baselines.
  4. NAME RISK: a moved class's old fully-qualified name (or a wholly moved package), dotted or slashed, in a string
     literal of production Scala, or anywhere in a production resource, conf/ or infra/ (logger-name filters, codec
     discriminators, serialized names, Class.forName), at the base or the head; a class-name read in moved production
     code or naming a moved class (getName, getCanonicalName, getTypeName, Class.forName); a renamed class's old
     simple name in a production literal. Each is NOT PURE, with its location.
  Any added or deleted file beyond the moves (a reference list's aside) is NOT PURE. A range moving no file is NOT PURE.

A static check: what it calls pure still has to compile (the pre-push hook compiles every config), and a package's
members seen through a package clause it cannot see are the compiler's to find.

usage: pure_move_check.py <base>..<head> [--quiet]
Exit 0 PURE MOVE, 1 NOT PURE (each non-move hunk named), 2 undecidable (a range git cannot read, base not an ancestor of
head, a Scala file that cannot be lexed). Tested by scripts/pure_move_check_test.py.
"""

from __future__ import annotations

import difflib
import functools
import re
import subprocess
import sys
from dataclasses import dataclass, field


class Undecidable(Exception):
    pass


def git(*args: str, ok: tuple[int, ...] = (0,)) -> str:
    done = subprocess.run(["git", "-c", "core.quotePath=false", *args], capture_output=True, text=True)
    if done.returncode not in ok:
        raise Undecidable(f"git {' '.join(args)}: {done.stderr.strip() or done.returncode}")
    return done.stdout


class Blobs:
    """`rev:path` contents through one `git cat-file --batch` (hundreds of files in a second)."""

    def __init__(self):
        self.proc = None
        self.cache: dict[str, bytes | None] = {}

    def get(self, rev: str, path: str) -> bytes | None:
        key = f"{rev}:{path}"
        if key in self.cache:
            return self.cache[key]
        if self.proc is None:
            self.proc = subprocess.Popen(["git", "cat-file", "--batch"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.proc.stdin.write(key.encode() + b"\n")
        self.proc.stdin.flush()
        header = self.proc.stdout.readline().decode().split()
        if len(header) < 3 or header[1] == "missing":
            self.cache[key] = None
            return None
        body = self.proc.stdout.read(int(header[2]))
        self.proc.stdout.read(1)
        self.cache[key] = body
        return body

    def text(self, rev: str, path: str) -> str | None:
        b = self.get(rev, path)
        if b is None:
            return None
        try:
            return b.decode("utf-8")
        except UnicodeDecodeError:
            return None


# ── lexing: Scala source as code, string literal and comment segments ────────────────────────────────────────────────

class Unlexable(Exception):
    pass


def _is_ident(ch: str) -> bool:
    return ch.isalnum() or ch == "_"


def string_end(src: str, j: int, interpolated: bool) -> int:
    """The index after the literal starting at src[j] == '"' (an interpolated one's splices skipped as code)."""
    n = len(src)
    if src.startswith('"""', j):
        k = j + 3
        while True:
            if k >= n:
                raise Unlexable("unterminated triple-quoted string")
            if interpolated and src.startswith("$$", k):
                k += 2
            elif interpolated and src.startswith("${", k):
                k = splice_end(src, k + 2)
            elif src.startswith('"""', k):
                while k + 3 < n and src[k + 3] == '"':
                    k += 1
                return k + 3
            else:
                k += 1
    k = j + 1
    while True:
        if k >= n or src[k] == "\n":
            raise Unlexable("unterminated string")
        if src[k] == "\\":
            k += 2
        elif interpolated and src.startswith("$$", k):
            k += 2
        elif interpolated and src.startswith("${", k):
            k = splice_end(src, k + 2)
        elif src[k] == '"':
            return k + 1
        else:
            k += 1


def splice_end(src: str, k: int) -> int:
    """The index after the `}` closing the splice whose body starts at src[k]."""
    depth = 1
    while k < len(src):
        c = src[k]
        if c == '"':
            k = string_end(src, k, k > 0 and _is_ident(src[k - 1]))
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return k + 1
        k += 1
    raise Unlexable("unterminated splice")


def interpolated_pieces(src: str, start: int, end: int) -> list[tuple[str, str]]:
    """An interpolated literal src[start:end] as its literal pieces and its `${...}` splices, each splice's body lexed as
    code: `s"p|${services.movies.ListingKey.serialised(k)}"` names a class in code, not in data."""
    pieces: list[tuple[str, str]] = []
    k, lit = start, start
    while k < end:
        if src.startswith("$$", k):
            k += 2
        elif src.startswith("${", k):
            close = splice_end(src, k + 2)
            pieces.append(("str", src[lit:k + 2]))
            pieces += segments(src[k + 2:close - 1])
            k = lit = close - 1
        else:
            k += 1
    pieces.append(("str", src[lit:end]))
    return [p for p in pieces if p[1]]


def segments(src: str) -> list[tuple[str, str]]:
    """[(kind, text)] with kind code / str / comment, concatenating back to `src`."""
    out: list[tuple[str, str]] = []
    code_start, i, n = 0, 0, len(src)

    def flush(upto: int):
        if upto > code_start:
            out.append(("code", src[code_start:upto]))

    while i < n:
        c = src[i]
        if src.startswith("//", i):
            flush(i)
            j = src.find("\n", i)
            j = n if j < 0 else j
            out.append(("comment", src[i:j]))
            i = code_start = j
        elif src.startswith("/*", i):
            flush(i)
            depth, j = 1, i + 2
            while depth:
                if j >= n:
                    raise Unlexable("unterminated comment")
                if src.startswith("/*", j):
                    depth, j = depth + 1, j + 2
                elif src.startswith("*/", j):
                    depth, j = depth - 1, j + 2
                else:
                    j += 1
            out.append(("comment", src[i:j]))
            i = code_start = j
        elif c == '"':
            start = i
            while start > code_start and _is_ident(src[start - 1]):
                start -= 1
            interpolated = start < i
            flush(start)
            j = string_end(src, i, interpolated)
            out += interpolated_pieces(src, start, j) if interpolated else [("str", src[start:j])]
            i = code_start = j
        elif c == "'" and (m := re.match(r"'(\\u[0-9a-fA-F]{4}|\\.|[^\\'\n])'", src[i:i + 8])):
            flush(i)
            out.append(("str", m.group(0)))
            i = code_start = i + len(m.group(0))
        elif c == "`":
            j = src.find("`", i + 1)
            i = n if j < 0 else j + 1
        else:
            i += 1
    flush(n)
    return out


# ── the moves ────────────────────────────────────────────────────────────────────────────────────────────────────────

PACKAGE_CLAUSE = re.compile(r"^\s*package\s+(?!object\b)([\w.]+)\s*;?\s*$")
TOP_LEVEL = re.compile(
    r"^(?:(?:private|protected)(?:\[\w+\])?\s+|final\s+|sealed\s+|abstract\s+|case\s+|implicit\s+|open\s+|opaque\s+"
    r"|transparent\s+|inline\s+|@\w+(?:\([^)]*\))?\s+)*(?:class|object|trait|enum|type|given)\s+([A-Za-z_]\w*)")


def is_scala(path: str) -> bool:
    return path.endswith(".scala") or path.endswith(".sbt")


def is_twirl(path: str) -> bool:
    return path.endswith(".scala.html")


def is_production(path: str) -> bool:
    return bool(re.match(r"^[^/]+/src/main/", path)) or path.startswith("conf/") \
        or (path.startswith("infra/") and not path.startswith("infra/test/"))


REFERENCE_LISTS = re.compile(
    r"(^|/)[^/]*\.md$|^docs/|^\.claude/|(^|/)aot-classes\.txt$"
    r"|^test/resources/(coupling|complexity|duplication|length)-baseline\.tsv$")


def code_text(src: str) -> str:
    """`src` with its comments blanked (a block comment to a space, keeping its line ends), literals kept."""
    parts = []
    for kind, text in segments(src):
        parts.append(re.sub(r"[^\n]", "", text) or " " if kind == "comment" and text.startswith("/*") else
                     "" if kind == "comment" else text)
    return "".join(parts)


def packages_of(src: str) -> list[str]:
    """The package clauses of a Scala source, in order (chained clauses each one)."""
    try:
        text = code_text(src)
    except Unlexable:
        text = src
    return [m.group(1) for line in text.splitlines() if (m := PACKAGE_CLAUSE.match(line))]


def top_level_names(src: str) -> list[str]:
    try:
        text = code_text(src)
    except Unlexable:
        text = src
    return [m.group(1) for line in text.splitlines() if (m := TOP_LEVEL.match(line))]


@dataclass
class Moves:
    files: dict[str, str] = field(default_factory=dict)       # old path -> new path
    added: set[str] = field(default_factory=set)              # head paths no base path became
    deleted: set[str] = field(default_factory=set)            # base paths no head path is
    modified: set[str] = field(default_factory=set)           # base paths changed in place
    classes: dict[str, str] = field(default_factory=dict)     # old FQN -> new FQN (changed only)
    simple: dict[str, str] = field(default_factory=dict)      # old simple name -> new (class renames)
    packages: dict[str, str] = field(default_factory=dict)    # old package -> new (wholly moved)
    dirs: dict[str, str] = field(default_factory=dict)        # old directory/ -> new directory/ (wholly moved)
    base: str = ""
    head: str = ""
    blobs: "Blobs | None" = None
    trees: dict[str, list[str]] = field(default_factory=dict)
    members: dict[tuple[str, str], set[str]] = field(default_factory=dict)

    def path_pairs(self) -> list[tuple[str, str]]:
        """(old, new) text forms to map, longest first: file paths, directories, FQNs dotted and slashed, packages."""
        pairs = [(old, new) for old, new in self.files.items() if old != new] + list(self.dirs.items())
        for old, new in self.classes.items():
            pairs += [(old, new), (old.replace(".", "/"), new.replace(".", "/"))]
        for old, new in self.packages.items():
            pairs += [(old, new), (old.replace(".", "/"), new.replace(".", "/"))]
        return sorted(set(pairs), key=lambda p: -len(p[0]))

    def mentions(self) -> set[str]:
        """Every form a moved thing is named by, old and new: paths, directories, FQNs and their packages."""
        names = set()
        for old, new in list(self.files.items()) + list(self.dirs.items()):
            names |= {old, new}
        names |= self.added
        for old, new in self.classes.items():
            for fqn in (old, new):
                names |= {fqn, fqn.replace(".", "/")}
                if "." in fqn:
                    package_name = fqn.rsplit(".", 1)[0]
                    names |= {package_name, package_name.replace(".", "/")}
        return {m for m in names if len(m) > 3}

    def tree(self, rev: str) -> list[str]:
        if rev not in self.trees:
            self.trees[rev] = git("ls-tree", "-r", "--name-only", rev).splitlines()
        return self.trees[rev]

    def package_members(self, rev: str, package_name: str) -> set[str]:
        """The top-level names the Scala files of package `package_name` define at `rev` (its directories, by convention)."""
        key = (rev, package_name)
        if key not in self.members:
            rel = "/scala/" + package_name.replace(".", "/") + "/"
            names: set[str] = set()
            for path in self.tree(rev):
                if rel in path and "/" not in path.split(rel, 1)[1] and path.endswith(".scala"):
                    src = self.blobs.text(rev, path) if self.blobs else None
                    if src and ".".join(packages_of(src)) == package_name:
                        names |= set(top_level_names(src))
            self.members[key] = names
        return self.members[key]


def name_status(before: str, after: str) -> list[list[str]]:
    return [line.split("\t") for line in git("diff", "-M", "-C", "--name-status", "--no-color", before, after).splitlines() if line]


def find_moves(base: str, head: str, blobs: Blobs) -> Moves:
    hops = [base] + git("rev-list", "--reverse", "--first-parent", f"{base}..{head}").split()
    current = {}       # base path -> its name now (None: deleted)
    added: set[str] = set()
    origin: dict[str, str] = {}  # a path now -> the base path it is
    for before, after in zip(hops, hops[1:]):
        for row in name_status(before, after):
            status, paths = row[0][0], row[1:]
            if status == "R":
                old, new = paths
                if old in added:
                    added.discard(old)
                    added.add(new)
                else:
                    src = origin.get(old, old)
                    current[src] = new
                    origin.pop(old, None)
                    origin[new] = src
            elif status in "CA":
                added.add(paths[-1])
            elif status == "D":
                path = paths[0]
                if path in added:
                    added.discard(path)
                else:
                    src = origin.pop(path, path)
                    current[src] = None
            elif status in "MT":
                path = paths[0]
                if path not in added and path not in origin:
                    current.setdefault(path, path)
    moves = Moves(base=base, head=head, blobs=blobs)
    deleted = {p for p, now in current.items() if now is None}
    # what the hops saw as a delete and an add, the direct diff may pair
    for row in name_status(base, head):
        if row[0][0] == "R" and row[1] in deleted and row[2] in added:
            current[row[1]] = row[2]
            deleted.discard(row[1])
            added.discard(row[2])
    for old, now in current.items():
        if now is None:
            continue
        if now != old:
            moves.files[old] = now
        elif blobs.get(base, old) != blobs.get(head, now):
            moves.modified.add(old)
    for old, now in moves.files.items():
        if blobs.get(base, old) is None or blobs.get(head, now) is None:
            raise Undecidable(f"{old} -> {now}: a side git cannot read")
    moves.added, moves.deleted = added, deleted
    # the class and package maps, from the moved Scala files
    moved_from: dict[str, set[str]] = {}
    for old, new in moves.files.items():
        if not is_scala(old) or not is_scala(new):
            continue
        old_src, new_src = blobs.text(base, old) or "", blobs.text(head, new) or ""
        old_package, new_package = ".".join(packages_of(old_src)), ".".join(packages_of(new_src))
        old_names, new_names = top_level_names(old_src), set(top_level_names(new_src))
        old_base, new_base = old.rsplit("/", 1)[-1][:-len(".scala")], new.rsplit("/", 1)[-1][:-len(".scala")]
        for name in old_names:
            new_name = name if name in new_names else new_base if name == old_base and new_base in new_names else None
            if new_name is None:
                continue
            old_fqn, new_fqn = f"{old_package}.{name}" if old_package else name, f"{new_package}.{new_name}" if new_package else new_name
            if old_fqn != new_fqn:
                moves.classes[old_fqn] = new_fqn
            if new_name != name:
                moves.simple[name] = new_name
        if old_package != new_package:
            moved_from.setdefault(old_package, set()).add(new_package)
    # a package moved whole: every file under its directories at the base moved, all to one package
    base_files = moves.tree(base)
    head_files = set(moves.tree(head))
    # a directory moved whole: every file directly in it at the base moved, all into one directory
    by_dir: dict[str, set[str]] = {}
    for old, new in moves.files.items():
        by_dir.setdefault(old.rsplit("/", 1)[0] + "/", set()).add(new.rsplit("/", 1)[0] + "/")
    for old_dir, targets in by_dir.items():
        children = [p for p in base_files if p.startswith(old_dir) and "/" not in p[len(old_dir):]]
        if len(targets) == 1 and all(p in moves.files for p in children):
            target = next(iter(targets))
            if target != old_dir:
                moves.dirs[old_dir] = target
    for old_package, targets in moved_from.items():
        if len(targets) != 1 or not old_package:
            continue
        rel = "/scala/" + old_package.replace(".", "/") + "/"
        members = [p for p in base_files if rel in p and "/" not in p.split(rel, 1)[1] and is_scala(p)]
        if members and all(p in moves.files and moves.files[p] != p for p in members) \
                and not any(rel in p and "/" not in p.split(rel, 1)[1] and is_scala(p) for p in head_files):
            moves.packages[old_package] = next(iter(targets))
    return moves


# ── normalisation ────────────────────────────────────────────────────────────────────────────────────────────────────

@functools.lru_cache(maxsize=None)
def boundary(old: str) -> re.Pattern:
    return re.compile(r"(?:(?<=_root_\.)|(?<![\w.$]))" + re.escape(old) + r"(?![\w$])")


@dataclass
class Normalised:
    lines: list[str]
    imports: set[str]
    packages: list[str]


def parse_imports(stmt: str) -> list[str]:
    """`import a.b.{C, D => E}, f.G` -> ['a.b.C', 'a.b.D => E', 'f.G']"""
    body = stmt.strip()
    body = re.sub(r"^@?import\s+", "", body).rstrip(";").strip()
    parts, depth, cur = [], 0, ""
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
    out = []
    for part in parts:
        part = re.sub(r"\s+", " ", part.strip())
        m = re.match(r"^([\w.`]+)\.\{(.*)\}$", part)
        if m:
            out += [f"{m.group(1)}.{s.strip()}" for s in m.group(2).split(",") if s.strip()]
        elif part:
            out.append(part)
    return out


def map_name(entry: str, moves: Moves) -> str:
    """An import entry's target mapped: a moved class, else a moved package prefix."""
    target, _, rest = entry.partition(" ")
    for old, new in sorted(moves.classes.items(), key=lambda p: -len(p[0])):
        if target == old or target.startswith(old + "."):
            return (new + target[len(old):] + (" " + rest if rest else "")).strip()
    for old, new in sorted(moves.packages.items(), key=lambda p: -len(p[0])):
        if target == old or target.startswith(old + "."):
            return (new + target[len(old):] + (" " + rest if rest else "")).strip()
    return entry


@functools.lru_cache(maxsize=64)
def _longest_first(names: frozenset) -> tuple[str, ...]:
    return tuple(sorted(names, key=lambda name: (-len(name), name)))


@functools.lru_cache(maxsize=64)
def _alternation(prefix: str, olds: tuple[str, ...], suffix: str) -> re.Pattern:
    return re.compile(prefix + "(" + "|".join(map(re.escape, olds)) + ")" + suffix)


def map_text(text: str, pairs: list[tuple[str, str]], simple: dict[str, str]) -> str:
    """Every old name in `text` as its new one, in one pass (the longest old name first at each place)."""
    if pairs:
        table = dict(pairs)
        olds = _longest_first(frozenset(table))
        text = _alternation(r"(?:(?<=_root_\.)|(?<![\w.$]))", olds, r"(?![\w$])").sub(lambda m: table[m.group(1)], text)
    if simple:
        text = _alternation(r"\b", _longest_first(frozenset(simple)), r"\b").sub(
            lambda m: simple[m.group(1)], text)
    return text


IMPORT_LINE = re.compile(r"^\s*@?import\s")


def unqualify(code: str, moves: Moves) -> str:
    """A moved class named by its new fully-qualified name, as its simple name: a file that named it bare while it
    shared its package names it qualified now, and both name the one class (a clash is the compiler's to refuse)."""
    fqns = _longest_first(frozenset(f for f in moves.classes.values() if "." in f))
    if not fqns:
        return code
    return _alternation(r"(?<![\w.$])(?:_root_\.)?", fqns, r"(?![\w$])").sub(lambda m: m.group(1).rsplit(".", 1)[1], code)


NOWARN = re.compile(r"@nowarn(\([^)]*\))?\s*")


def relative_pairs(src: str, moves: Moves) -> list[tuple[str, str]]:
    """A moved class named relative to a package the old file sits in (`adapters.TmdbStore` in package services.identity,
    or under the chained clauses `package services.identity` / `package adapters`) -> its new fully-qualified name."""
    clauses = packages_of(src)
    visible = [".".join(clauses[:k]) for k in range(1, len(clauses) + 1)]
    pairs = []
    for old, new in moves.classes.items():
        for visible_package in visible:
            if old.startswith(visible_package + ".") and "." in old[len(visible_package) + 1:]:
                pairs.append((old[len(visible_package) + 1:], new))
    return pairs


def normalise_scala(src: str, path: str, old_side: bool, moves: Moves, twirl: bool = False) -> Normalised:
    pairs, production = moves.path_pairs(), is_production(path)
    if old_side and not twirl:
        pairs = sorted(set(pairs + relative_pairs(src, moves)), key=lambda p: -len(p[0]))
    if twirl:
        pieces = [("code", src)]
    else:
        pieces = segments(src)
    parts = []
    for kind, text in pieces:
        if kind == "comment":
            parts.append(re.sub(r"[^\n]", "", text) or " " if text.startswith("/*") else "")
            continue
        if old_side and (kind == "code" or not production):
            text = map_text(text, pairs, moves.simple)
        if kind == "code":
            text = "\n".join(part if IMPORT_LINE.match(part) else unqualify(part, moves) for part in text.split("\n"))
            # a compiler-warning filter compiles to nothing: a move's `@nowarn("msg=unused import")` scope is no code
            text = NOWARN.sub("", text)
            text = re.sub(r"(?<=\S)[ \t]+", " ", text)
        parts.append(text)
    text = "".join(parts)
    lines, imports, packages = [], set(), []
    raw = text.splitlines()
    i = 0
    while i < len(raw):
        line = raw[i].rstrip()
        stripped = line.strip()
        if PACKAGE_CLAUSE.match(line):
            packages.append(PACKAGE_CLAUSE.match(line).group(1))
            i += 1
            continue
        if re.match(r"^@?import\s", stripped):
            stmt = stripped
            while stmt.count("{") > stmt.count("}") and i + 1 < len(raw):
                i += 1
                stmt += " " + raw[i].strip()
            imports.update(e for e in parse_imports(stmt) if e != "scala.annotation.nowarn")
            i += 1
            continue
        if stripped:
            lines.append(line)
        i += 1
    return Normalised(lines, imports, packages)


QUALIFIER = re.compile(r"\b(private|protected)\[\w+\]")
IMPLICIT_LINE = re.compile(r"\b(implicit|given|using|extension)\b")
DECLARED = re.compile(r"\b(?:def|val|var|lazy val|class|object|trait|type)\s+([A-Za-z_]\w*)")


@dataclass
class Verdict:
    moved: list[tuple[str, str]] = field(default_factory=list)
    references: list[str] = field(default_factory=list)
    visibility: list[str] = field(default_factory=list)
    hunks: list[str] = field(default_factory=list)
    risks: list[str] = field(default_factory=list)


def diff_lines(a: list[str], b: list[str], old: str, new: str, limit: int = 12) -> str:
    out = [line for line in difflib.unified_diff(a, b, old, new, n=0, lineterm="") if not line.startswith("@@")]
    shown = out[2:2 + limit]
    more = len(out) - 2 - len(shown)
    return "\n".join("      " + line for line in shown) + (f"\n      ... {more} more line(s)" if more > 0 else "")


def wildcard_risk(package_name: str, new_package: str, visible_before: list[str], old_wildcards: set[str], moves: Moves,
                  used: str) -> str:
    """Why an added `import package_name.*` is no move, or "". Two are: the file's old package, which it moved out of (its names
    were in scope; none of them may shadow a name of the file's new package it uses, as the inner package did), and a
    package holding only moved classes, each from a package the file saw or wildcard-imported before."""
    if package_name in visible_before:
        clash = moves.package_members(moves.head, package_name) & moves.package_members(moves.head, new_package) if new_package else set()
        used_clash = sorted(n for n in clash if re.search(rf"\b{re.escape(n)}\b", used))
        return f"{package_name}.* now shadows {', '.join(used_clash)} of {new_package}" if used_clash else ""
    members = moves.package_members(moves.head, package_name)
    moved_here = {new.rsplit(".", 1)[1]: old.rsplit(".", 1)[0] for old, new in moves.classes.items() if new.rsplit(".", 1)[0] == package_name}
    stayed = sorted(set(members) - set(moved_here))
    if not members or stayed:
        return f"a wildcard bringing names that did not move: {', '.join(stayed[:3]) or 'an empty package'}"
    unseen = sorted({p for p in moved_here.values() if p not in visible_before and p not in old_wildcards})
    return f"a wildcard of names the file did not see before (from {', '.join(unseen)})" if unseen else ""


def import_findings(old: Normalised, new: Normalised, old_path: str, new_path: str, moves: Moves,
                    body: list[str]) -> list[str]:
    old_imports = {map_name(e, moves) for e in old.imports}
    visible_before = [".".join(old.packages[:k]) for k in range(1, len(old.packages) + 1)]
    new_package = ".".join(new.packages)
    new_targets = set(moves.classes.values()) | set(moves.packages.values())
    used = " ".join(body)
    found = []
    old_wildcards = {e.rsplit(".", 1)[0] for e in old_imports if e.endswith((".*", "._"))}
    for entry in sorted(new.imports - old_imports):
        target = entry.split(" ")[0]
        package_name, _, name = target.rpartition(".")
        if "=>" in entry or " as " in entry or name == "given" or entry.endswith(" given"):
            found.append(f"import added: {entry} (a given or renaming import)")
        elif name in ("_", "*"):
            why = wildcard_risk(package_name, new_package, visible_before, old_wildcards, moves, used)
            if why:
                found.append(f"import added: {entry} ({why})")
        elif package_name in visible_before or target in new_targets:
            continue
        else:
            found.append(f"import added: {entry} (neither from the file's old package nor a moved name)")
    wildcards = {e.rsplit(".", 1)[0] for e in new.imports if e.endswith((".*", "._"))}
    for entry in sorted(old_imports - new.imports):
        target = entry.split(" ")[0]
        package_name, _, name = target.rpartition(".")
        if (package_name == new_package and new_package) or (package_name in wildcards and "=>" not in entry):
            continue
        if name not in ("_", "*", "given") and "=>" not in entry and not re.search(rf"\b{re.escape(name)}\b", used):
            continue
        # a wildcard the move left with nothing to bring: the file names none of the package's members at the head (what
        # it named of it moved, and is imported by its new name above), so dropping it -- as the unused-import lint
        # requires -- resolves no name otherwise
        if name in ("_", "*") and not any(re.search(rf"\b{re.escape(m)}\b", used)
                                          for m in moves.package_members(moves.head, package_name)):
            continue
        found.append(f"import removed: {entry}")
    return found


def compare_scala(old_path: str, new_path: str, old_src: str, new_src: str, moves: Moves, verdict: Verdict):
    twirl = is_twirl(new_path)
    try:
        old = normalise_scala(old_src, old_path, True, moves, twirl)
        new = normalise_scala(new_src, new_path, False, moves, twirl)
    except Unlexable as e:
        raise Undecidable(f"{new_path}: cannot lex ({e})")
    label = new_path if old_path == new_path else f"{old_path} -> {new_path}"
    findings = import_findings(old, new, old_path, new_path, moves, new.lines)
    if old.lines != new.lines:
        wild = lambda ls: [QUALIFIER.sub(r"\1[_]", line) for line in ls]  # noqa: E731
        if wild(old.lines) == wild(new.lines):
            changed = [b for a, b in zip(old.lines, new.lines) if a != b]
            names = [m.group(1) for line in changed for m in [DECLARED.search(line)] if m]
            overloaded = [n for n in names if len(re.findall(rf"\bdef\s+{re.escape(n)}\b", "\n".join(new.lines))) > 1]
            unsafe = [line for line in changed if IMPLICIT_LINE.search(line)]
            if unsafe or overloaded:
                why = "an implicit/given/extension line" if unsafe else f"overloaded {', '.join(sorted(set(overloaded)))}"
                findings.append(f"visibility changed on {why}:\n" + diff_lines(old.lines, new.lines, "old", "new"))
            else:
                verdict.visibility.append(f"{label}: " + "; ".join(
                    f"{QUALIFIER.search(a).group(0) if QUALIFIER.search(a) else '?'} -> "
                    f"{QUALIFIER.search(b).group(0) if QUALIFIER.search(b) else '?'}"
                    + (f" ({DECLARED.search(b).group(1)})" if DECLARED.search(b) else "")
                    for a, b in zip(old.lines, new.lines) if a != b))
        else:
            findings.append("code differs:\n" + diff_lines(old.lines, new.lines, old_path, new_path))
    verdict.hunks += [f"{label}: {f}" for f in findings]


def compare_text(old_path: str, new_path: str, old_bytes: bytes, new_bytes: bytes, moves: Moves, verdict: Verdict):
    label = new_path if old_path == new_path else f"{old_path} -> {new_path}"
    if REFERENCE_LISTS.search(new_path):
        verdict.references.append(f"{label} (reference list)")
        return
    try:
        old_t, new_t = old_bytes.decode("utf-8"), new_bytes.decode("utf-8")
    except UnicodeDecodeError:
        verdict.hunks.append(f"{label}: binary content differs")
        return
    mapped = map_text(old_t, moves.path_pairs(), {})
    if mapped == new_t:
        if old_t != new_t:
            verdict.references.append(f"{label} (names the moved paths)")
        return
    if not is_production(new_path):
        # a tool's path list or comment: every line it changes names a moved path, package or class
        changed = [line[1:] for line in difflib.unified_diff(mapped.splitlines(), new_t.splitlines(), n=0, lineterm="")
                   if line[:1] in "+-" and not line.startswith(("+++", "---"))]
        mentions = moves.mentions()
        if all(any(m in line for m in mentions) for line in changed):
            verdict.references.append(f"{label} (path lists: {len(changed)} line(s) naming the moved paths)")
            return
    verdict.hunks.append(f"{label}: differs beyond the moved names:\n"
                   + diff_lines(mapped.splitlines(), new_t.splitlines(), old_path, new_path))


# ── name risk ────────────────────────────────────────────────────────────────────────────────────────────────────────

def literal_hits(src: str, names: set[str]) -> list[tuple[int, str, str]]:
    """(line, name, the line) for each of `names` inside a string literal of the Scala source `src`."""
    try:
        pieces = segments(src)
    except Unlexable:
        pieces = [("str", src)]
    hits, offset, lines = [], 0, src.splitlines()
    for kind, text in pieces:
        if kind == "str":
            for nm in sorted(names):
                for m in boundary(nm).finditer(text):
                    no = src.count("\n", 0, offset + m.start()) + 1
                    hits.append((no, nm, lines[no - 1].strip()[:160] if no <= len(lines) else ""))
        offset += len(text)
    return hits


CLASS_NAME_READ = re.compile(r"\b(getName|getCanonicalName|getTypeName|Class\.forName)\b")


def name_risks(base: str, head: str, moves: Moves, blobs: Blobs) -> list[str]:
    names = set()
    for old in list(moves.classes) + list(moves.packages):
        if "." in old:
            names |= {old, old.replace(".", "/")}
    risks: list[str] = []
    scanned: set[tuple[str, ...]] = set()
    for rev, side in ((base, "base"), (head, "head")):
        if names:
            args = ["grep", "-n", "-F"] + [x for nm in sorted(names) for x in ("-e", nm)] + [rev, "--"]
            hits = git(*args, ok=(0, 1)).splitlines()
            for hit in hits:
                _, path, line_no, text = hit.split(":", 3)
                if not is_production(path) or REFERENCE_LISTS.search(path) or is_twirl(path):
                    continue
                if is_scala(path):
                    if (rev, path) in scanned:
                        continue
                    scanned.add((rev, path))
                    risks += [f"{side} {path}:{no}: a string literal names {nm}: {line}"
                              for no, nm, line in literal_hits(blobs.text(rev, path) or "", names)]
                else:
                    hit_names = [nm for nm in names if boundary(nm).search(text)]
                    if hit_names:
                        risks.append(f"{side} {path}:{line_no}: names {', '.join(sorted(hit_names))}: {text.strip()[:160]}")
    # class-name reads: in a moved production file, or naming a moved class anywhere in production
    moved_simple = {c.rsplit(".", 1)[-1] for c in moves.classes}
    for old, new in moves.files.items():
        if not is_production(new) or not is_scala(new):
            continue
        src = blobs.text(head, new) or ""
        for no, line in enumerate(src.splitlines(), 1):
            if CLASS_NAME_READ.search(line) and not line.strip().startswith(("//", "*", "/*")):
                risks.append(f"head {new}:{no}: reads a class name in a moved file: {line.strip()[:160]}")
    if moved_simple:
        pattern = r"(classOf\[(" + "|".join(map(re.escape, sorted(moved_simple))) + r")\][^\n]*\b(getName|getCanonicalName|getTypeName)" \
                  r"|\b(" + "|".join(map(re.escape, sorted(moved_simple))) + r")\.getClass\.(getName|getCanonicalName|getTypeName))"
        for hit in git("grep", "-n", "-E", "-e", pattern, head, "--", "*.scala", ok=(0, 1)).splitlines():
            _, path, line_no, text = hit.split(":", 3)
            if is_production(path) and path not in moves.files.values():
                risks.append(f"head {path}:{line_no}: reads a moved class's name: {text.strip()[:160]}")
    # a renamed class's old simple name in a production literal
    for old in moves.simple:
        for hit in git("grep", "-new", "-w", "-F", "-e", old, base, "--", "*.scala", ok=(0, 1)).splitlines():
            _, path, line_no, text = hit.split(":", 3)
            if not is_production(path):
                continue
            if (base, path, old) in scanned:
                continue
            scanned.add((base, path, old))
            risks += [f"base {path}:{no}: a string literal names the renamed class {old}: {line}"
                      for no, _, line in literal_hits(blobs.text(base, path) or "", {old})]
    return sorted(set(risks))


# ── the verdict ──────────────────────────────────────────────────────────────────────────────────────────────────────

def check(base: str, head: str) -> tuple[Verdict, Moves]:
    blobs = Blobs()
    for ref in (base, head):
        git("rev-parse", "--verify", f"{ref}^{{commit}}")
    if subprocess.run(["git", "merge-base", "--is-ancestor", base, head]).returncode != 0:
        raise Undecidable(f"{base} is not an ancestor of {head}")
    moves = find_moves(base, head, blobs)
    verdict = Verdict(moved=sorted(moves.files.items()))
    pairs = sorted(moves.files.items()) + [(p, p) for p in sorted(moves.modified)]
    for old, new in pairs:
        old_bytes, new_bytes = blobs.get(base, old), blobs.get(head, new)
        if old_bytes == new_bytes:
            continue
        if (is_scala(old) and is_scala(new)) or (is_twirl(old) and is_twirl(new)):
            old_s, new_s = blobs.text(base, old), blobs.text(head, new)
            if old_s is None or new_s is None:
                raise Undecidable(f"{new}: not UTF-8")
            compare_scala(old, new, old_s, new_s, moves, verdict)
        else:
            compare_text(old, new, old_bytes, new_bytes, moves, verdict)
    for path in sorted(moves.added):
        (verdict.references.append(f"{path} (reference list, added)") if REFERENCE_LISTS.search(path)
         else verdict.hunks.append(f"{path}: added (no file it was moved from)"))
    for path in sorted(moves.deleted):
        (verdict.references.append(f"{path} (reference list, deleted)") if REFERENCE_LISTS.search(path)
         else verdict.hunks.append(f"{path}: deleted (no file it was moved to)"))
    if not moves.files:
        verdict.hunks.append("(the range) moves no file: not a move")
    verdict.risks = name_risks(base, head, moves, blobs)
    return verdict, moves


def report(rng: str, verdict: Verdict, moves: Moves, quiet: bool) -> int:
    pure = not verdict.hunks and not verdict.risks
    say = print
    if not quiet or not pure:
        say(f"pure-move-check {rng}: {len(verdict.moved)} file(s) moved, {len(moves.classes)} class name(s), "
            f"{len(moves.packages)} package(s) wholly moved")
        for old, new in sorted(moves.packages.items()):
            say(f"  package {old} -> {new}")
        if verdict.references:
            say(f"  reference edits (pure, {len(verdict.references)}):")
            for r in verdict.references:
                say(f"    {r}")
        if verdict.visibility:
            say(f"  visibility only (pure, {len(verdict.visibility)} file(s)):")
            for r in verdict.visibility:
                say(f"    {r}")
        if verdict.hunks:
            say(f"  NON-MOVE HUNKS ({len(verdict.hunks)}):")
            for h in verdict.hunks:
                say(f"    {h}")
        if verdict.risks:
            say(f"  NAME RISK ({len(verdict.risks)}):")
            for r in verdict.risks:
                say(f"    {r}")
    if pure:
        say("PURE MOVE")
        return 0
    say(f"NOT PURE: {len(verdict.hunks)} non-move hunk(s), {len(verdict.risks)} name risk(s)")
    return 1


def main(argv: list[str]) -> int:
    quiet = "--quiet" in argv
    args = [a for a in argv if a != "--quiet"]
    if len(args) != 1 or ".." not in args[0] or args[0].startswith("-"):
        print(__doc__.split("usage: ")[1].splitlines()[0], file=sys.stderr)
        return 2
    base, head = args[0].split("..", 1)
    try:
        verdict, moves = check(base, head or "HEAD")
    except Undecidable as e:
        print(f"pure-move-check: UNDECIDABLE: {e}", file=sys.stderr)
        return 2
    except Exception as e:  # noqa: BLE001 -- a check that broke decides nothing: undecidable, never pure
        print(f"pure-move-check: UNDECIDABLE: the check failed: {type(e).__name__}: {e}", file=sys.stderr)
        return 2
    return report(args[0], verdict, moves, quiet)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
