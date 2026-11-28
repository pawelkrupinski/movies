#!/usr/bin/env python3
# Purpose: The drift gate's `retypes_only`: whether a commit changes a Scala file only in comments and provably-same type names
"""WHETHER A COMMIT ONLY RETYPES A FILE (split out of commit_rules.py): its comments and the names of the types it declares
that provably name the same types, with their imports. Used by the gauges-before-behaviour rule."""

from __future__ import annotations

import re
import subprocess

import tool_git
from commit_facts import Commit

# A declared type in a Scala line: `name: Type` (generics, dots, `&`/`|` unions included), as a parameter or a member.
TYPE_ANNOTATION = re.compile(r"\b(\w+)\s*:\s*([A-Z][\w.]*(?:\[[^\]=]*\])?(?:\s*[&|]\s*[A-Z][\w.]*(?:\[[^\]=]*\])?)*)")
# A line whose types steer an implicit search: a changed given, implicit or using clause may select another instance
# even through an alias, which no diff decides (the owner, 2026-11-18).
RETYPE_UNSAFE = re.compile(r"\b(implicit|given|using|summon|implicitly)\b")
IDENTIFIER = re.compile(r"[A-Za-z_]\w*")
ALIAS_OPAQUE = re.compile(r"\bopaque\b")


def code_lines(c: Commit, path: str) -> tuple[list[str], list[str]]:
    """The (removed, added) lines of `c` in `path`, blank and comment lines dropped, stripped, in diff order."""
    removed: list[str] = []
    added: list[str] = []
    for line in tool_git.Git.here().text("diff", "-U0", "--no-color", c.parent, c.sha, "--", path).splitlines():
        if line.startswith(("---", "+++")) or not line[:1] in "+-":
            continue
        body = line[1:].strip()
        if body and not body.startswith(("//", "/*", "*")):
            (removed if line[0] == "-" else added).append(body)
    return removed, added


def type_renames(old: str, new: str) -> list[tuple[str, str]] | None:
    """The (old, new) names that differ between two declared types of one shape (`Queue` / `Producer`), None when the
    types are of another shape (`A` to `A & B`, `List[Int]` to `Map[K, V]`)."""
    if IDENTIFIER.sub("#", old).replace(" ", "") != IDENTIFIER.sub("#", new).replace(" ", ""):
        return None
    return [(o, n) for o, n in zip(IDENTIFIER.findall(old), IDENTIFIER.findall(new)) if o != n]


def declared_types(line: str) -> list[str]:
    return [m.group(2) for m in TYPE_ANNOTATION.finditer(line)]


def renamed_types(removed: list[str], added: list[str]) -> dict[str, str] | None:
    """{old type name: new} that turns every removed line into an added one, the lines differing only in the names of
    their declared types (paired by their shape, `name: _`, in diff order); None when they differ otherwise -- another
    statement, another order, or a type of another shape (`A` to `A & B`, `List[Int]` to `Map[K, V]`)."""
    shape = lambda line: TYPE_ANNOTATION.sub(r"\1: _", line)  # noqa: E731
    if sorted(map(shape, removed)) != sorted(map(shape, added)):
        return None
    pending: dict[str, list[str]] = {}
    for line in added:
        pending.setdefault(shape(line), []).append(line)
    renamed: dict[str, str] = {}
    for line in removed:
        new_line = pending[shape(line)].pop(0)
        for old, new in zip(declared_types(line), declared_types(new_line)):
            pairs = type_renames(old, new)
            if pairs is None or any(renamed.setdefault(o, n) != n for o, n in pairs):
                return None
    return renamed


def explicit_import_of(line: str, names: set[str]) -> bool:
    """`line` imports by name only some of `names` (`import a.b.Name`, `import a.b.{X, Y}`): never a wildcard (`_`,
    `*`), a given import or a rename (`X => Y`, `X as Y`), each of which may put another implicit or name in scope."""
    m = re.fullmatch(r"import\s+([\w.]+?)(?:\.\{([^}]*)\}|\.(\w+))\s*;?", line)
    if not m or re.search(r"=>|\bas\b|\bgiven\b|[*]|\b_\b", line):
        return False
    imported = [n.strip() for n in (m.group(2) or m.group(3)).split(",") if n.strip()]
    return bool(imported) and all(n in names for n in imported)


def defines(rev: str, name: str) -> list[str]:
    """The Scala lines at `rev` defining type `name` (a class, trait, object, enum or type), as `git grep` prints them."""
    done = subprocess.run(["git", "grep", "-hwF", "-e", name, rev, "--", "*.scala"], capture_output=True, text=True)
    definition = re.compile(rf"\b(class|trait|object|enum|type)\s+{re.escape(name)}\b")
    return [line for line in done.stdout.splitlines() if definition.search(line)]


def same_type(c: Commit, old: str, new: str) -> bool:
    """`new` provably names the type `old` named: an alias of it at `c` (`type New = Old` or `type Old = New`, never an
    opaque type), or the type renamed by `c` (defined as `old` and not `new` before, as `new` and not `old` after)."""
    for line in defines(c.sha, new) + defines(c.sha, old):
        if ALIAS_OPAQUE.search(line):
            continue
        if re.search(rf"\btype\s+({re.escape(new)}\s*=\s*{re.escape(old)}|{re.escape(old)}\s*=\s*{re.escape(new)})\s*$", line):
            return True
    return bool(defines(c.parent, old)) and not defines(c.parent, new) and bool(defines(c.sha, new)) \
        and not defines(c.sha, old)


def split_imports(lines: list[str]) -> tuple[list[str], list[str]]:
    """`lines` as (their import lines, the others)."""
    return [line for line in lines if line.startswith("import ")], [line for line in lines if not line.startswith("import ")]


def names_provable(c: Commit, imports: list[str], renamed: dict[str, str]) -> bool:
    """Whether the imports name only the renamed types and each rename provably names the same type."""
    names = set(renamed) | set(renamed.values())
    return all(explicit_import_of(line, names) for line in imports) \
        and all(same_type(c, old, new) for old, new in renamed.items())


def retypes_only(c: Commit, path: str) -> bool:
    """Whether `c` changes `path` only in its comments and in names of the types it declares that provably name the
    same types -- a rename of the type, or an alias of it (same_type) -- with the imports of those names. Only then do
    the same lines run the same code. Any other type change may select other code (Int to Long picks other overloads
    and arithmetic; a role trait narrowed from a class may dispatch otherwise; a changed implicit, given or using
    clause, or a given/wildcard import, may select another instance), and none of that is decided statically here,
    so it is behaviour (the owner, 2026-11-18; the narrowed VenuePageWait queue of 2026-11-17 was exempted as "a
    retype" before)."""
    removed, added = code_lines(c, path)
    imports_removed, code_removed = split_imports(removed)
    imports_added, code_added = split_imports(added)
    if any(RETYPE_UNSAFE.search(line) for line in code_removed + code_added):
        return False
    renamed = renamed_types(code_removed, code_added)
    return renamed is not None and names_provable(c, imports_removed + imports_added, renamed)
