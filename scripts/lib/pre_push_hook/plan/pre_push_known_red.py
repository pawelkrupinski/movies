"""THE KNOWN-RED BASELINE (scripts/hooks/pre-push's header): a failing check is held against the push only when it fails
in a way plain main does not -- every one of its failure lines (the strong markers, else every line; normalized: no
colour, timestamps, temp paths, shas or numbers) one of the same check's failure on the base, in `<dir>/<base sha>`
(the hook's own output of the same checks on the base's tree, its first line `pre-push baseline <sha>`)."""
from __future__ import annotations

import re
from typing import List, Optional

ESCAPE = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
STRONG = re.compile(r"\*\*\* FAILED|\[error\]|::error::|[Aa]ssert(ion)?|Error:|ENOENT|[Ee]xit code [1-9]|FAILED")
STAMP = re.compile(r"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z ")
TEMP = re.compile(r"/(private/)?(tmp|var/folders)/\S*")
VERDICT = re.compile(r"pre-push [^ ]+ (ok|FAILED) \([0-9]+s\)| — for [0-9]+ changed path")


def untemp(line: str, root: str) -> str:
    """Each word from `root` on, `<tmp>` (awk's untmp: `root` up to the next blank)."""
    if len(root) < 3:
        return line
    while root in line:
        at = line.index(root)
        rest = re.sub(r"^[^ \t]*", "", line[at + len(root):])
        line = line[:at] + "<tmp>" + rest
    return line


def normalized(line: str, roots: List[str]) -> str:
    line = TEMP.sub("<tmp>", STAMP.sub("", line))
    for root in roots:
        line = untemp(line, root)
    line = re.sub(r"[0-9]+(\.[0-9]+)?", "N", re.sub(r"[0-9a-f]{7,40}", "H", line.lower()))
    return re.sub(r"[ \t\n\r\f\v]+", " ", line).strip(" ")[:160]


def failure_lines(text: str, roots: List[str]) -> List[str]:
    """A check's failure lines from its output, normalized, sorted (bytewise), unique. `roots`: where this host's
    temporary files go (the hook's temp dir's parent, $TMPDIR)."""
    every = [line for line in ESCAPE.sub("", text).split("\n") if line.strip()]
    strong = [line for line in every if STRONG.search(line)] or every
    return sorted({normalized(line, roots) for line in strong}, key=lambda s: s.encode())


def baseline_failure(check: str, baseline: str) -> Optional[str]:
    """The output the baseline's run printed for `check` before its `<check> FAILED` line; None when it did not fail."""
    segment, found, out = "", False, ""
    lines = ESCAPE.sub("", baseline).split("\n")
    if baseline.endswith("\n"):
        lines.pop()
    for line in lines:
        if VERDICT.search(line):
            if f"pre-push {check} FAILED (" in line:
                out, found = out + segment, True
            segment = ""
            continue
        segment += line + "\n"
    return out if found else None


def known_red(check: str, output: str, baseline: str, roots: List[str]) -> bool:
    """`check` failed (its `output`) only in ways it fails on the base."""
    base = baseline_failure(check, baseline)
    if base is None:
        return False
    mine = failure_lines(output, roots)
    return bool(mine) and set(mine) <= set(failure_lines(base, roots))
