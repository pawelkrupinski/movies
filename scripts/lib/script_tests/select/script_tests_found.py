"""THE SELF-TESTS scripts/run-script-tests finds, and what each says of itself in its first 40 lines: a `# script-test:
<tag> <why>` keeping it out of the default run, the globs `# script-test-reads:` declares it reads, the width
`# script-test-width: N` it fans out to, and `# script-test-stress:` marking it concurrency-heavy, and the python package `# script-test-requires: <requirements.txt>
<module>` it needs. Files only: no
process, clock or environment."""
from __future__ import annotations

import fnmatch
import os
import re
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence

HEADER_LINES = 40
PRUNED = ("node_modules", "build", ".build", "target")
TAG = re.compile(r"^#\s*script-test:\s*(.*)$")
READS = re.compile(r"^#\s*script-test-reads:\s*(.*)$")
WIDTH = re.compile(r"^# script-test-width: *([0-9]+)")
STRESS = re.compile(r"^#\s*script-test-stress:")
REQUIRES = re.compile(r"^#\s*script-test-requires:\s*(\S+)\s+(\S+)\s*$")


@dataclass(frozen=True)
class Found:
    """A self-test: its path from the root, its tag ("-" for none) and the tag's reason."""
    path: str
    tag: str = "-"
    reason: str = ""

    def row(self) -> str:
        return f"{self.path}\t{self.tag}\t{self.reason}"


def header(path: str, lines: int = HEADER_LINES) -> List[str]:
    """A file's first `lines` lines, without their line ends (none for a file that cannot be read)."""
    out: List[str] = []
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            for line in f:
                out.append(line.rstrip("\n"))
                if len(out) >= lines:
                    break
    except OSError:
        pass
    return out


def is_self_test(name: str) -> bool:
    return name.endswith("-test.sh") or name.endswith("_test.py")


def under(root: str, top: str) -> List[str]:
    """Every self-test under `top` (a root, relative to `root`), its directories named in PRUNED passed over."""
    found: List[str] = []
    for directory, dirs, files in os.walk(os.path.join(root, top)):
        dirs[:] = [d for d in dirs if d not in PRUNED]
        relative = os.path.relpath(directory, root)
        for name in files:
            if is_self_test(name) and not os.path.islink(os.path.join(directory, name)):
                found.append(os.path.join(relative, name))
    return found


def tagged(root: str, path: str) -> Found:
    for line in header(os.path.join(root, path)):
        marker = TAG.match(line)
        if marker:
            text = marker.group(1)
            tag = text.split()[0] if text.split() else ""
            return Found(path, tag, text[len(tag):].lstrip()) if tag else Found(path)
    return Found(path)


def discover(root: str, roots: Sequence[str]) -> List[Found]:
    """Every self-test under the roots, sorted as bytes, each with its tag."""
    paths = {p for top in roots if os.path.isdir(os.path.join(root, top)) for p in under(root, top)}
    return [tagged(root, p) for p in sorted(paths, key=lambda p: p.encode())]


def declared_reads(root: str, paths: Iterable[str]) -> Dict[str, List[str]]:
    """Each test's declared globs (READS), in its first 40 lines; a test declaring none has no entry."""
    out: Dict[str, List[str]] = {}
    for path in paths:
        for line in header(os.path.join(root, path)):
            declared = READS.match(line)
            if declared:
                out.setdefault(path, []).extend(declared.group(1).split())
    return out


def declared_width(path: str) -> int:
    """The width a test's `# script-test-width: N` declares, 1 without one."""
    for line in header(path):
        width = WIDTH.match(line)
        if width:
            return max(1, int(width.group(1)))
    return 1


@dataclass(frozen=True)
class Requirement:
    requirements: str  # the pinned requirements file, relative to the root
    module: str


def declared_requirement(path: str) -> Optional[Requirement]:
    """What a test's `# script-test-requires:` line (first 40 lines) names; None for a test without one."""
    for line in header(path):
        declared = REQUIRES.match(line)
        if declared:
            return Requirement(declared.group(1), declared.group(2))
    return None


def concurrency_heavy(path: str) -> bool:
    return any(STRESS.match(line) for line in header(path))


def matches_filter(path: str, filters: Sequence[str]) -> bool:
    """No filters, or one names a directory above `path`, a substring of it, or a glob matching it."""
    if not filters:
        return True
    for given in filters:
        f = given[:-1] if given.endswith("/") else given
        if path == f or path.startswith(f + "/") or f in path or fnmatch.fnmatchcase(path, f):
            return True
    return False


def glob_matches(path: str, glob: str) -> bool:
    """A declared glob matching a path as a shell `case` pattern does: `*` crosses `/`."""
    return fnmatch.fnmatchcase(path, glob)
