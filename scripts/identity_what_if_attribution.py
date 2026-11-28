"""WHICH RULES A CHANGE CAN REACH (the SELECT half of scripts/identity_what_if.py, whose header says how the mapping reads):
the changed lines of a `git diff -U0`, each mapped to the definition holding it, walked up its callers (the identity sources
and the worker's, at the base and the head) until every path ends in a rule root of scripts/identity-what-if-roots.tsv, and
the families whose traces name those rules. Imported by identity_what_if.py, which has put scripts/ and scripts/lib on sys.path."""
from __future__ import annotations

import fnmatch
import os
import re
import subprocess
from typing import Dict, List, Optional, Sequence, Set, Tuple

import tool_git  # noqa: E402

IDENTITY_SOURCES = "identity/src/main/scala/"
WORKER_SOURCES = "worker/src/main/scala/"
AGREEMENT = "identity/src/main/scala/services/identity/agreement/"
CODE_PREFIXES = ("geodata/src/main/", "base/src/main/", "http/src/main/", "common/src/main/", "identity/src/main/", "scrapers/src/main/", "worker/src/main/",
                 "testkit/src/main/", "project/", "conf/")
CODE_FILES = ("build.sbt",)
MAX_CLOSURE = 400
# a changed line: (path, side "-" base or "+" head, its line number there, its text); a root: {definition: rule-id patterns}
Change = Tuple[str, str, int, str]
Roots = Dict[str, List[str]]

DEFINITION = re.compile(r"^(\s*)(?:@\w+(?:\([^)]*\))?\s+)*(?:(?:private|protected)(?:\[\w+\])?\s+|override\s+|final\s+|implicit\s+|"
                        r"lazy\s+|inline\s+|sealed\s+|abstract\s+|case\s+|transparent\s+|opaque\s+)*"
                        r"(def|val|var|object|class|trait|enum|type|given)\s+`?([A-Za-z_][\w$]*)")


def inert_path(path: str) -> bool:
    """A file whose change cannot move a take: not code, or a test tree."""
    if any(f"/src/{tree}/" in path for tree in ("test", "it", "fixtures", "pagetest", "e2e")):
        return True
    return not (path.startswith(CODE_PREFIXES) or path in CODE_FILES)


def comment_or_blank(line: str) -> bool:
    """A line that changes no behaviour by itself: blank, a comment, an import, or closing brackets alone (a definition
    added or removed changes its other lines too)."""
    text = line.strip()
    return not text.strip("{}()[] ") or text.startswith(("*", "/*", "//", "*/", "import ", "package "))


TYPES = ("object", "class", "trait", "enum", "given")


def _indent(line: str) -> int:
    return len(line) - len(line.lstrip())


def _enclosing(lines: List[str], index: int) -> List[Tuple[str, str]]:
    """(name, kind) of every definition enclosing line `index` of `lines`, the line's own first, innermost to outermost.
    Scopes by indentation: a definition holds the lines below it up to the next line indented no deeper than it."""
    own = DEFINITION.match(lines[index])
    chain = [(own.group(3), own.group(2))] if own else []
    floor = len(own.group(1)) if own else _indent(lines[index])
    for line in reversed(lines[:index]):
        if (comment_or_blank(line) and not DEFINITION.match(line)) or _indent(line) >= floor:
            continue
        floor = _indent(line)
        m = DEFINITION.match(line)
        if m:
            chain.append((m.group(3), m.group(2)))
    return chain


def holder_at(lines: List[str], index: int) -> Optional[Tuple[str, str]]:
    """(name, kind) of the MEMBER holding line `index` (0-based) of `lines`: the outermost definition (a def, val, var,
    type) enclosing it under its object, class or trait -- a local def or val inside a member is the member's -- or that
    type itself for a line in its body outside every member; None at the top of a file."""
    chain = _enclosing(lines, index)
    if not chain:
        return None
    # the outermost member below the innermost type: the definition the rest of the code names
    innermost_type = next((i for i, c in enumerate(chain) if c[1] in TYPES), len(chain))
    inside = [c for c in chain[:innermost_type] if c[1] not in TYPES]
    return inside[-1] if inside else chain[innermost_type]


def definition_at(lines: List[str], index: int) -> Optional[str]:
    holder = holder_at(lines, index)
    return holder and holder[0]


def diff_path(header: str) -> str:
    """The head-side path a `diff --git` header names: `a/x b/x`, or each side quoted (`"a/x y" "b/x y"`, a name git
    quotes: a space, a quote, a non-ASCII byte as an octal escape) -- unquoted and its escapes decoded."""
    if header.endswith('"') and ' "b/' in header:
        quoted = header[header.rindex(' "b/') + 4:-1]
        return quoted.encode("latin-1", "backslashreplace").decode("unicode_escape").encode("latin-1").decode("utf-8", "replace")
    return header.split(" b/", 1)[1]


def hunks(diff_text: str) -> List[Change]:
    """[(path, side, line numbers)] of a `git diff -U0`: each changed line's number on the base ('-') or head ('+') side."""
    out, path, old, new = [], None, None, 0
    for line in diff_text.splitlines():
        if line.startswith("diff --git "):
            path = diff_path(line)
        elif line.startswith("@@"):
            old, new = map(int, re.findall(r"@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@", line)[0])
        elif old is not None and line.startswith("-") and not line.startswith("---"):
            out.append((path, "-", old, line[1:]))
            old += 1
        elif old is not None and line.startswith("+") and not line.startswith("+++"):
            out.append((path, "+", new, line[1:]))
            new += 1
    return out


class Sources:
    """The identity sources at the base and the head, read once: what the caller walk greps."""

    def __init__(self, repo: str, base: str, head: str) -> None:
        self.repo, self.sides = repo, {"-": base, "+": head}
        self.cache = {}

    def files(self, side: str, roots: Sequence[str] = (IDENTITY_SOURCES,)) -> List[str]:
        ref = self.sides[side]
        if ref == ".":
            out = tool_git.Git.here(self.repo).text("ls-files", "--cached", "--others", "--exclude-standard", "--", *roots)
        else:
            out = tool_git.Git.here(self.repo).text("ls-tree", "-r", "--name-only", ref, "--", *roots)
        return [p for p in out.splitlines() if p.endswith(".scala")]

    def lines(self, side: str, path: str) -> List[str]:
        key = (side, path)
        if key not in self.cache:
            ref = self.sides[side]
            try:
                if ref == ".":
                    with open(os.path.join(self.repo, path), encoding="utf-8") as f:
                        text = f.read()
                else:
                    text = tool_git.Git.here(self.repo).text("show", f"{ref}:{path}")
            except (OSError, subprocess.CalledProcessError):
                text = ""
            self.cache[key] = text.splitlines()
        return self.cache[key]

    def callers(self, name: str) -> Set[str]:
        """{definition holding a line that names `name`} over the identity sources and the worker's (the model's callers),
        at both sides, its own definition lines aside; a worker definition is named `worker:<name>`."""
        return {qualified(path, holder) for side in ("-", "+")
                for path in self.files(side, (IDENTITY_SOURCES, WORKER_SOURCES))
                for holder in holders_naming(self.lines(side, path), name)}


def holders_naming(lines: List[str], name: str) -> Set[str]:
    """The definitions holding a line of `lines` that names `name`, its own definition lines aside."""
    word = re.compile(rf"\b{re.escape(name)}\b")
    holders = set()
    for i, line in enumerate(lines):
        if comment_or_blank(line) or not word.search(line):
            continue
        m = DEFINITION.match(line)
        if m and m.group(3) == name:
            continue
        holder = definition_at(lines, i)
        if holder and holder != name:
            holders.add(holder)
    return holders


def qualified(path: str, holder: str) -> str:
    """The holder named by where it lives: `agreement:<name>` (the final take's stage), `worker:<name>`, or bare."""
    if path.startswith(AGREEMENT):
        return "agreement:" + holder
    return holder if path.startswith(IDENTITY_SOURCES) else "worker:" + holder


def read_roots(path: str) -> Roots:
    """{definition: [rule-id patterns]} of the roots file (`#` comments; columns definition, patterns, why)."""
    roots = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            if not line.strip() or line.startswith("#") or line.startswith("definition\t"):
                continue
            cols = line.rstrip("\n").split("\t")
            roots[cols[0]] = cols[1].split()
    return roots


def _definition_of_change(sources: Sources, side: str, path: str, number: int) -> Tuple[Optional[str], Optional[str]]:
    """(the definition holding changed line `number` of `path`, None), or (None, why) when no rule can own the change."""
    if path.startswith(AGREEMENT):
        return None, f"{path} changed: the agreement stage, whose final take of every family is settled again"
    if not path.startswith(IDENTITY_SOURCES):
        return None, f"{path} changed: code outside the identity model's sources (lookups, normalizer, build, resources)"
    lines = sources.lines(side, path)
    holder = holder_at(lines, number - 1) if 0 < number <= len(lines) else None
    if holder is None:
        return None, f"{path}:{number} changed outside any definition"
    if holder[1] in TYPES:
        return None, f"{path}:{number} changed in {holder[1]} {holder[0]}'s own body, outside its members"
    return holder[0], None


def _changed_definitions(changes: Sequence[Change], sources: Sources) -> Tuple[Optional[Dict[str, str]], Optional[str]]:
    """({definition: its first changed line}, None), or (None, why) when some change attributes to no rule."""
    changed = {}
    for path, side, number, text in changes:
        if inert_path(path) or comment_or_blank(text):
            continue
        name, why = _definition_of_change(sources, side, path, number)
        if why:
            return None, why
        changed.setdefault(name, f"{path}:{number}")
    return changed, None


def _callers_refusal(callers: Set[str], where: str, name: str, current: str) -> Optional[str]:
    """Why a walk that reached the worker's or the agreement stage's definition attributes nothing, or None."""
    outside = sorted(c for c in callers if c.startswith("worker:"))
    if outside:
        return f"{where} ({name}) reaches the worker's {outside[0][7:]} through {current}, past every rule root"
    stage = sorted(c for c in callers if c.startswith("agreement:"))
    if stage:
        return f"{where} ({name}) reaches the agreement stage's {stage[0][10:]} through {current}: every family's final take"
    return None


def _patterns_reached(name: str, where: str, sources: Sources, roots: Roots) -> Tuple[Optional[Set[str]], Optional[str]]:
    """(the rule-id patterns of the roots `name`'s callers reach, None), or (None, why) when a path reaches none."""
    patterns, seen, frontier = set(), {name}, [name]
    while frontier:
        current = frontier.pop()
        if current in roots:
            patterns.update(roots[current])
            continue
        callers = sources.callers(current)
        why = _callers_refusal(callers, where, name, current)
        if why:
            return None, why
        # nothing in the model or the worker names it: dead in production (a spec's), and this path moves no take
        new = callers - seen
        seen |= new
        frontier.extend(new)
        if len(seen) > MAX_CLOSURE:
            return None, f"{where} ({name}) reaches over {MAX_CLOSURE} definitions: shared resolver code"
    return patterns, None


def attribute(changes: Sequence[Change], sources: Sources, roots: Roots) -> Tuple[Optional[Set[str]], Optional[str]]:
    """(patterns, None) -- the rule ids the changed lines can move -- or (None, why) when some change attributes to no
    rule. `changes`: (path, side, line number, text) of every changed line."""
    changed, why = _changed_definitions(changes, sources)
    if changed is None:
        return None, why
    patterns = set()
    for name, where in sorted(changed.items()):
        reached, why = _patterns_reached(name, where, sources, roots)
        if reached is None:
            return None, why
        patterns |= reached
    return patterns, None


def matches(rule_ids: Sequence[str], patterns: Sequence[str]) -> bool:
    return any(fnmatch.fnmatchcase(rule, pattern) for rule in rule_ids for pattern in patterns)


def select(rows: Sequence[Dict[str, str]], patterns: Sequence[str]) -> Tuple[List[str], int, int]:
    """The keys of every listing of a family any member of which a pattern names, and the families picked."""
    families = {}
    for r in rows:
        families.setdefault(r["family"], []).append(r)
    picked = [f for f, members in families.items()
              if any(matches(r["rules"].split("\x1f") if r["rules"] else [], patterns) for r in members)]
    return sorted(r["key"] for f in picked for r in families[f]), len(picked), len(families)


def changes_between(repo: str, base: str, head: str) -> List[Change]:
    if head == ".":
        text = tool_git.Git.here(repo).text("diff", "-U0", "--no-color", "--no-ext-diff", base)
        for path in tool_git.Git.here(repo).text("ls-files", "--others", "--exclude-standard").splitlines():
            if path.endswith(".scala") or not inert_path(path):
                with open(os.path.join(repo, path), encoding="utf-8", errors="replace") as f:
                    text += f"\ndiff --git a/{path} b/{path}\n@@ -0,0 +1 @@\n" + "".join("+" + l for l in f.read().splitlines(True))
        return hunks(text)
    return hunks(tool_git.Git.here(repo).text("diff", "-U0", "--no-color", "--no-ext-diff", base, head))
