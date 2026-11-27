"""WHAT A --changed RUN SELECTS (scripts/run-script-tests): the paths changed since the base, and every self-test they
reach -- its own file or its tool's (the same name without -test/_test), a file whose name it mentions, a glob it
declares it reads (READS), and, however deep, what a changed file is SOURCED or IMPORTED by and what RUNS it: the
DERIVED READS (14eb62591d: stall-watch.sh changed, and pre-push-tools-test.sh, which runs the hook, which runs the
watchdog, was never selected). A script names a script it runs or sources in its code -- `"$here/../stall-watch.sh"
watch`, `exec python3 "$(dirname "$0")/stall_watch.py"`, `os.path.join(ROOT, "scripts/hooks/pre-push")` -- so every
script naming another in a line of code (not a comment) is that one's user, as a source or an import makes it. A
`# script-test-reads:` line stays, for what no name says (every workflow, every .py). Files and git only, through the
`git` it is handed."""
from __future__ import annotations

import os
import re
from typing import Dict, Iterable, List, Optional, Protocol, Sequence, Set, Tuple

from script_tests_found import declared_reads, glob_matches, header

SCRIPT = re.compile(r"\.(sh|py)$|(^|/)[^./]+$")
USES = re.compile(r"^\s*(\.|source)\s|shellcheck\s+source=|^\s*(import|from)\s+[A-Za-z_]")
IMPORT = re.compile(r"^\s*(?:import|from)\s+([A-Za-z_][A-Za-z0-9_]*)")
SOURCED = re.compile(r"[A-Za-z0-9_.-]+\.sh")
PATHLIKE = re.compile(r"[A-Za-z0-9_./-]+")
WORD = re.compile(r"[A-Za-z0-9_.-]+")


class Git(Protocol):
    """git in the checkout: (status, stdout) of a git command (the runner's, or a test's fake)."""

    def __call__(self, args: Sequence[str]) -> Tuple[int, str]: ...


def lines_of(path: str) -> List[str]:
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read().splitlines()
    except OSError:
        return []


def sorted_unique(paths: Iterable[str]) -> List[str]:
    return sorted({p for p in paths if p}, key=lambda p: p.encode())


def changed_paths(git: Git, base: str) -> Optional[List[str]]:
    """Committed since leaving `base`, plus uncommitted and untracked; None when `base` is no commit."""
    if git(["rev-parse", "-q", "--verify", base + "^{commit}"])[0] != 0:
        return None
    out: List[str] = []
    for args in (["diff", "--name-only", base + "...HEAD"], ["diff", "--name-only", "HEAD"],
                 ["ls-files", "--others", "--exclude-standard"]):
        out += git(args)[1].splitlines()
    return sorted_unique(out)


def script_index(git: Git, roots: Sequence[str]) -> List[str]:
    """The checkout's scripts under the roots, tracked or new: what a source, an import or a run can name."""
    listed = git(["ls-files", "--", *roots])[1].splitlines()
    listed += git(["ls-files", "--others", "--exclude-standard", "--", *roots])[1].splitlines()
    return sorted_unique(p for p in listed if SCRIPT.search(p))


def dir_of(path: str) -> str:
    return path.rsplit("/", 1)[0] if "/" in path else "."


class Index:
    """The scripts, by path, by file name, and the extensionless ones by `<directory>/<name>` (`hooks/pre-push`)."""

    def __init__(self, paths: Sequence[str]) -> None:
        self.paths = set(paths)
        self.named: Dict[str, List[str]] = {}
        self.bare: Dict[str, List[str]] = {}
        for p in paths:
            name = p.rsplit("/", 1)[-1]
            self.named.setdefault(name, []).append(p)
            if "." not in name and "/" in p:
                self.bare.setdefault(dir_of(p).rsplit("/", 1)[-1] + "/" + name, []).append(p)

    def resolve(self, importer: str, name: str) -> List[str]:
        """What `importer` means by a file name: the one beside it, else every script of that name."""
        beside = dir_of(importer) + "/" + name
        if beside in self.paths:
            return [beside]
        return [p for p in self.named.get(name, []) if p != importer]


def add(users: Dict[str, Set[str]], used: Iterable[str], user: str) -> None:
    for u in used:
        users.setdefault(u, set()).add(user)


def source_users(root: str, index: Index) -> Dict[str, Set[str]]:
    """file -> the scripts sourcing or importing it (bash `. f` / `source f` / `# shellcheck source=f`; Python
    `import m` / `from m import`)."""
    users: Dict[str, Set[str]] = {}
    for importer in sorted(index.paths):
        for line in lines_of(os.path.join(root, importer)):
            if USES.search(line):
                add(users, used_by(index, importer, line), importer)
    return users


def used_by(index: Index, importer: str, line: str) -> List[str]:
    """What one source or import line of `importer` names (a shell file imports nothing)."""
    imported = IMPORT.match(line)
    if imported:
        return [] if importer.endswith(".sh") else index.resolve(importer, imported.group(1) + ".py")
    return [p for name in SOURCED.findall(line) for p in index.resolve(importer, name)]


def named_in_code(index: Index, user: str, line: str) -> List[str]:
    """The scripts a line of code names: a file name with its extension, or an extensionless tool by its directory
    and name."""
    named: List[str] = []
    for run in PATHLIKE.findall(line):
        parts = run.split("/")
        named += [p for part in parts if part.endswith((".sh", ".py")) for p in index.resolve(user, part)]
        named += [p for a, b in zip(parts, parts[1:]) if b and "." not in b for p in index.bare.get(f"{a}/{b}", [])
                  if p != user]
    return named


def run_users(root: str, index: Index) -> Dict[str, Set[str]]:
    """THE DERIVED READS: file -> the scripts naming it in a line of code (what runs or sources it)."""
    users: Dict[str, Set[str]] = {}
    for user in sorted(index.paths):
        for line in lines_of(os.path.join(root, user)):
            if not line.lstrip().startswith("#"):
                add(users, named_in_code(index, user, line), user)
    return users


def reached(changed: Sequence[str], users: Dict[str, Set[str]]) -> List[str]:
    """The changed paths and every script using one, transitively, in the order found."""
    seen = set(changed)
    queue = list(dict.fromkeys(changed))
    for path in queue:
        for user in sorted(users.get(path, ())):
            if user not in seen:
                seen.add(user)
                queue.append(user)
    return queue


def stem(name: str) -> str:
    """The tool name a test or tool shares: - and _ alike, the extension and -test/_test dropped."""
    n = name.rsplit(".", 1)[0] if "." in name else name
    for suffix in ("-test", "_test"):
        n = n[: -len(suffix)] if n.endswith(suffix) else n
    return n.replace("_", "-")


def name_key(path: str) -> Optional[str]:
    """What a test naming `path` names: its file name, or an extensionless one's directory and name."""
    directory, name = dir_of(path), path.rsplit("/", 1)[-1]
    if "." not in name:
        if directory == ".":
            return None
        name = directory.rsplit("/", 1)[-1] + "/" + name
    return name


def with_dots(word: str) -> List[str]:
    """A word and each prefix of it ending before a `.` (a name followed by `.` still names it: `x.sh.` in prose)."""
    return [word] + [word[:i] for i, c in enumerate(word) if c == "."]


def names_in(text: str) -> Set[str]:
    """Every name a text holds as a whole word (no letter, digit, `_`, `.` or `-` before it, none but a `.` after it):
    file names, and `<directory>/<name>` pairs."""
    words = list(WORD.finditer(text))
    out: Set[str] = set()
    for i, word in enumerate(words):
        out.update(with_dots(word.group(0)))
        if i + 1 < len(words) and words[i + 1].start() == word.end() + 1 and text[word.end()] == "/":
            out.update(word.group(0) + "/" + w for w in with_dots(words[i + 1].group(0)))
    return out


def as_read(root: str, paths: Sequence[str]) -> List[str]:
    """The paths as a declared glob sees them: each, and an extensionless Python tool as a .py too (0544a8f758)."""
    out: List[str] = []
    for path in paths:
        out.append(path)
        name = path.rsplit("/", 1)[-1]
        if "." not in name and os.path.isfile(os.path.join(root, path)):
            first = header(os.path.join(root, path), 1)
            if first and re.match(r"^#!.*python", first[0]):
                out.append(path + ".py")
    return out


def forward(users: Dict[str, Set[str]]) -> Dict[str, Set[str]]:
    """user -> what it uses, from used -> its users."""
    out: Dict[str, Set[str]] = {}
    for used, by in users.items():
        for user in by:
            out.setdefault(user, set()).add(used)
    return out


class Derived:
    """THE DERIVED READS of each test: what it names in its code and, one step further, what those name -- a test
    runs its tool, the tool runs the next (pre-push-tools-test.sh -> scripts/hooks/pre-push -> stall-watch.sh) --
    each with all it sources or imports, never through another self-test. Two steps: a third doubled what a change
    selects again (mean 5.4 -> 9.9 tests a changed script over this checkout, 2026-11-27) for chains no red has taken."""
    DEPTH = 2

    def __init__(self, root: str, index: Index, tests: Sequence[str]) -> None:
        self.sources = forward(source_users(root, index))
        self.names = forward(run_users(root, index))
        self.tests = set(tests)

    def with_sources(self, path: str, seen: Set[str]) -> List[str]:
        """`path` and what it sources or imports, transitively, not yet in `seen` (added to it)."""
        found, stack = [path], [path]
        seen.add(path)
        while stack:
            for used in self.sources.get(stack.pop(), ()):
                if used not in seen:
                    seen.add(used)
                    found.append(used)
                    stack.append(used)
        return found

    def reads(self, test: str) -> Set[str]:
        seen: Set[str] = set()
        front = self.with_sources(test, seen)
        for _ in range(self.DEPTH):
            named = [n for p in front for n in self.names.get(p, ()) if n not in seen]
            front = [q for n in dict.fromkeys(named) if n not in seen for q in self.with_sources(n, seen)
                     if q not in self.tests]
        return seen


class Selector:
    """Whether a --changed run selects a test, over the paths the change reaches."""

    def __init__(self, root: str, tests: Sequence[str], changed: Sequence[str], floor_scope: bool = False,
                 readers: bool = False, derived: Optional[Derived] = None) -> None:
        self.changed, self.floor_scope, self.readers, self.derived = set(changed), floor_scope, readers, derived
        self.keys = {(dir_of(p), stem(p.rsplit("/", 1)[-1])) for p in changed}
        self.names = {k for k in (name_key(c) for c in changed) if k}
        self.reads = declared_reads(root, tests)
        self.read_paths = as_read(root, changed)
        self.root = root

    def reads_changed(self, test: str) -> bool:
        return any(glob_matches(p, g) for g in self.reads.get(test, []) for p in self.read_paths)

    def names_changed(self, test: str) -> bool:
        if not self.names:
            return False
        try:
            with open(os.path.join(self.root, test), encoding="utf-8", errors="replace") as f:
                return bool(self.names & names_in(f.read()))
        except OSError:
            return False

    def touched(self, test: str) -> bool:
        """The test, its tool, a file whose name it mentions, or one it reads (--readers: a declared glob alone)."""
        if self.readers:
            return self.reads_changed(test)
        if test in self.changed or (dir_of(test), stem(test.rsplit("/", 1)[-1])) in self.keys:
            return True
        if self.floor_scope:
            return False
        if self.names_changed(test) or self.reads_changed(test):
            return True
        return self.derived is not None and bool(self.changed & self.derived.reads(test))
