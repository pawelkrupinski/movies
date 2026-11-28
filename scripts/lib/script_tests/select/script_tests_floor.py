"""THE FLOOR'S STATIC HALF (scripts/run-script-tests --bash / --floor with --changed): every bash file the change
touches (a .sh, or an extensionless file with a bash shebang) parsed by the floor bash (`-n`), and its ADDED lines held
to the constructs bash 3.2 lacks (scripts/floor-constructs.tsv: an ERE and what to say, a row; a line's comment is no
code), each finding a `FLOOR <file>:<line>: <construct>` row. A file whose first 40 lines say `# bash-floor: <why>`
names them as DATA: parsed, its lines never held. Files, and git and the floor bash through the commands handed in."""
from __future__ import annotations

import os
import re
from dataclasses import dataclass
from typing import Iterable, List, Pattern, Protocol, Sequence, Tuple

from script_tests.select.script_tests_changed import Git
from script_tests.select.script_tests_found import header

BASH_SHEBANG = re.compile(r"^#!.*[/ ]bash(\s|$)")
DATA_MARK = re.compile(r"^#\s*bash-floor:")
HUNK = re.compile(r"^@@ \S+ \+(\d+)")


class Parse(Protocol):
    """The floor bash's parse of one file (`bash -n`): its status and stderr."""

    def __call__(self, path: str) -> Tuple[int, str]: ...


@dataclass(frozen=True)
class Construct:
    pattern: Pattern[str]
    why: str


def constructs(table: str) -> List[Construct]:
    """The table's rows, each ERE read as Python's (`[[:space:]]` its one POSIX class)."""
    out: List[Construct] = []
    with open(table, encoding="utf-8") as f:
        for line in f.read().splitlines():
            fields = line.split("\t")
            if line.startswith("#") or len(fields) < 2 or not fields[0]:
                continue
            out.append(Construct(re.compile(fields[0].replace("[[:space:]]", r"[ \t\n\r\f\v]")), fields[1]))
    return out


def bash_sources(root: str, paths: Iterable[str]) -> List[str]:
    """Those of `paths` that are bash: a .sh, or an extensionless file with a bash shebang."""
    out: List[str] = []
    for path in paths:
        full = os.path.join(root, path)
        name = path.rsplit("/", 1)[-1]
        if not os.path.isfile(full):
            continue
        if name.endswith(".sh"):
            out.append(path)
        elif "." not in name:
            first = header(full, 1)
            if first and BASH_SHEBANG.match(first[0]):
                out.append(path)
    return out


def added(diff: str) -> List[Tuple[str, int, str]]:
    """(path, line, text) of each line a unified diff (-U0) adds."""
    out: List[Tuple[str, int, str]] = []
    path, line = "", 0
    for row in diff.splitlines():
        hunk = HUNK.match(row)
        if row.startswith("+++ "):
            path = row[4:][2:] if row[4:].startswith("b/") else row[4:]
        elif hunk:
            line = int(hunk.group(1))
        elif row.startswith("+") and path:
            out.append((path, line, row[1:]))
            line += 1
    return out


def added_lines(git: Git, base: str, files: Sequence[str]) -> List[Tuple[str, int, str]]:
    """The lines the change adds to `files` as they are now: the working tree against where it left the base,
    untracked files whole."""
    if not files:
        return []
    code, since = git(["merge-base", base, "HEAD"])
    if code != 0:
        return []
    diff = git(["diff", "-U0", "--no-color", "--no-ext-diff", since.strip(), "--", *files])[1]
    for path in files:
        if git(["ls-files", "--error-unmatch", "--", path])[0] != 0:
            diff += git(["diff", "-U0", "--no-color", "--no-index", "/dev/null", path])[1]
    return added(diff)


def code_of(text: str) -> str:
    """A line without its comment: a whole-line one, or one after a blank."""
    if re.match(r"^\s*#", text):
        return ""
    return re.sub(r"\s#.*$", "", text)


def unparsed(parse: Parse, files: Sequence[str]) -> List[str]:
    out: List[str] = []
    for path in files:
        code, err = parse(path)
        if code != 0:
            said = "".join(line + " " for line in err.splitlines()[:3])
            out.append(f"FLOOR   {path}: bash 3.2 cannot parse it: {said}")
    return out


def findings(root: str, parse: Parse, git: Git, base: str, files: Sequence[str],
             table: Sequence[Construct]) -> List[str]:
    """A `FLOOR` row for each thing the floor bash cannot run."""
    out = unparsed(parse, files)
    held = [p for p in files if not any(DATA_MARK.match(line) for line in header(os.path.join(root, p)))]
    for path, line, text in added_lines(git, base, held):
        hit = next((c for c in table if c.pattern.search(code_of(text))), None)
        if hit:
            out.append(f"FLOOR   {path}:{line}: {hit.why}: {text}")
    return out
