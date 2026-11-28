#!/usr/bin/env python3
"""THE SHELL MIGRATION GATE (the pre-push `shellgate` check; CLAUDE.md, "Design rules": every language but bash is
ratcheted, bash is MEASURED only to force its migration -- the owner, 2026-11-28). Every shell tool of the repository
(a `*.sh`, or an extension-less file with a shell shebang; self-tests and the vendored android/gradlew aside) is
measured by shell_measure.py against the design rules table's shell limits (test/resources/design-rules.tsv):

  shell-lines           <file>              its code lines                      150
  shell-decisions       <file>:<body>       a function's or the top level's     10 (the cyclomatic limit)
  shell-function-lines  <file>:<function>   a function's code lines             50 (the method-lines limit)
  shell-nesting         <file>:<body>       its deepest block nest              4 (a cognitive 1+2+3+4 = 10)

and every workflow's and composite action's `run:` block (workflow_runs.py), keyed `<file>#<job>/<step>`, is shell too:
the same decisions, function and nesting limits, and a block's code lines against its own `run-lines` limit (15): a `run:`
past a limit moves to a script, Python by the migration rule.

A NEW script past a limit, or a frozen one GROWING past its row, is refused: migrate it to Python
(scripts/shell-py-progress/migrate-tools.tsv: the same path, CLI and output, plus a black-box `<tool>_test.py`). Today's
scripts past a limit are frozen in test/resources/shell-baseline.tsv, rows that ONLY fall: no trailer admits a rise.
A fall (or a script migrated: its rows gone) is recorded in the push making it (THE FALL LOCK, baseline_ratchet.py).
THE MIGRATION QUEUE is the frozen scripts less the deliberate keeps (test/resources/shell-keeps.tsv: `<path><TAB><added
YYYY-MM-DD><TAB><why>`, the why 40+ characters; a keep of a script within every limit is stale, and refused).

  shell_gate.py check [--range A..B]...   exit 1 on a refusal, 2 on an error (run in the checkout's root)
  shell_gate.py update                    the baseline rewritten from the measure (falls and migrations recorded)
  shell_gate.py queue                     the migration queue, the largest first, then the keeps"""
from __future__ import annotations

import argparse
import os
import re
import sys
from dataclasses import dataclass
from typing import Dict, List, Sequence, Set, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path[:0] = [HERE, os.path.join(ROOT, "scripts", "ci"), os.path.join(ROOT, "scripts", "lib")]
import baseline_ratchet  # noqa: E402
import rules_table  # noqa: E402
import shell_measure  # noqa: E402
import workflow_runs  # noqa: E402
from seams import tool_files  # noqa: E402
import tool_git  # noqa: E402
import trailers  # noqa: E402

BASELINE = "test/resources/shell-baseline.tsv"
KEEPS = "test/resources/shell-keeps.tsv"
KINDS = ("shell-lines", "shell-decisions", "shell-function-lines", "shell-nesting", "run-lines")
SHELL_SHEBANG = re.compile(r"^#!.*[/ ](ba|da|k|z)?sh( |$)")
TEST = re.compile(r"(-test\.sh|_test\.sh)$|(^|/)test_[^/]*$|(^|/)tests?/")
NOT_MEASURED = re.compile(r"(^|/)(fixtures|node_modules)/|^android/gradlew$")
ISO_DATE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
MIGRATE = ("migrate it to Python (scripts/shell-py-progress/migrate-tools.tsv: the same path, CLI and output, plus a"
           " black-box `<tool>_test.py`; a workflow's `run:` block moves to such a script) -- a shell script or run block"
           " past a limit is never admitted, and no trailer raises a row")
HEADER = """# THE SHELL BASELINE (scripts/design-rules/shell_gate.py update writes it; the pre-push `shellgate` check holds every
# push touching a shell tool to it). Every shell tool past a limit of test/resources/design-rules.tsv, frozen -- the
# migration queue (shell_gate.py queue). A row ONLY falls: a new row or a rise is refused whatever a trailer says
# (migrate the script to Python instead), and a fall is recorded in the push making it."""
Rows = Dict[Tuple[str, str], float]


def held(part: str) -> bool:
    """A key's file is a shell tool's: a self-test's is measured, never held."""
    return not TEST.search(part.split(":", 1)[0])


SPEC = baseline_ratchet.Spec(
    title="SHELL", tool="shell gate", baseline=BASELINE, header=HEADER, hint=MIGRATE,
    update_command="scripts/design-rules/shell_gate.py update", held_rows="shell tools' rows past a limit, frozen",
    trailers={"Shell": KINDS}, held=held)


@dataclass(frozen=True)
class Keep:
    path: str
    added: str
    why: str


def candidate(path: str) -> bool:
    """A path that may hold a shell tool: a `.sh` / `.bash`, or a name with no extension (its shebang decides)."""
    name = os.path.basename(path)
    return not NOT_MEASURED.search(path) and (name.endswith((".sh", ".bash")) or "." not in name)


def is_shell(path: str, text: str) -> bool:
    return path.endswith((".sh", ".bash")) or bool(SHELL_SHEBANG.match(text.split("\n", 1)[0]))


def read_or_empty(files: tool_files.Files, path: str) -> str:
    """The file's text; "" for one that is no UTF-8 text (a binary with no extension) or no file (a submodule)."""
    try:
        return files.read_text(path)
    except (UnicodeDecodeError, OSError):
        return ""


def shell_files(git: tool_git.Git, files: tool_files.Files) -> Dict[str, str]:
    """Every shell file of the checkout, path -> text."""
    texts = {p: read_or_empty(files, p) for p in git.lines("ls-files") if candidate(p)}
    return {p: t for p, t in texts.items() if is_shell(p, t)}


def run_blocks(git: tool_git.Git, files: tool_files.Files) -> Dict[str, str]:
    """Every `run:` block of the workflows and composite actions, `<file>#<job>/<step>` -> its shell text."""
    return {f"{path}#{label}": text for path in git.lines("ls-files") if workflow_runs.holds_runs(path)
            for label, text in workflow_runs.run_blocks(read_or_empty(files, path)).items()}


def run_rows(path: str, text: str) -> List[Tuple[str, str, int]]:
    """A run block's rows: the shell measure's, its code lines under `run-lines`."""
    return [("run-lines" if kind == "shell-lines" else kind, key, value) for kind, key, value in shell_measure.rows(path, text)]


def measure(sources: Dict[str, str], runs: Dict[str, str], table: rules_table.Rules) -> Rows:
    """The held rows past their limits, and every kind's limit."""
    current: Rows = {("limit", kind): table.limit(kind) for kind in KINDS}
    for rows in ([row for path, text in sorted(sources.items()) for row in shell_measure.rows(path, text)],
                 [row for path, text in sorted(runs.items()) for row in run_rows(path, text)]):
        current.update({(kind, key): float(value) for kind, key, value in rows
                        if value > table.limit(kind) and held(key)})
    return current


def parse_keeps(text: str) -> Tuple[List[Keep], List[str]]:
    keeps: List[Keep] = []
    problems: List[str] = []
    for line in text.splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        cells = line.split("\t")
        if len(cells) != 3 or not ISO_DATE.match(cells[1]) or len(cells[2].strip()) < 40:
            problems.append(f"{KEEPS}: not `<path><TAB><added YYYY-MM-DD><TAB><why, 40+ characters>`: {line!r}")
            continue
        keeps.append(Keep(*cells))
    return keeps, problems


def frozen_files(rows: Rows) -> Set[str]:
    """Each file with a row past a limit."""
    return {key.split(":", 1)[0] for kind, key in rows if kind in KINDS}


def stale_keeps(keeps: Sequence[Keep], current: Rows) -> List[str]:
    frozen = frozen_files(current)
    return [f"{KEEPS}: {k.path} is within every shell limit now (or gone): strike its keep" for k in keeps
            if k.path not in frozen]


def grown(baseline: Rows, old_text: str, renames: baseline_ratchet.Renames) -> List[str]:
    """How the baseline grew from `old_text` (a row added or raised, a limit raised): refused, trailer or not."""
    return baseline_ratchet.growth(SPEC, KINDS, baseline_ratchet.parse(old_text), baseline, renames) if old_text else []


def range_growth(ranges: Sequence[str], baseline: Rows, renames: baseline_ratchet.Renames) -> List[str]:
    return [f"{rng}: {g}" for rng in ranges for g in grown(baseline, trailers.text_at(rng.split("..")[0], BASELINE), renames)]


def say(title: str, lines: Sequence[str], hint: str = "") -> bool:
    if lines:
        print(title)
        print("\n".join(f"  - {line}" for line in lines))
        if hint:
            print(f"  {hint}")
    return bool(lines)


def check(current: Rows, baseline: Rows, keeps_text: str, ranges: Sequence[str]) -> bool:
    """Whether the measure passes: nothing past a row or a new row past a limit, no grown baseline, no unrecorded fall
    and no stale or malformed keep -- each refusal said."""
    renames = baseline_ratchet.Renames()
    keeps, problems = parse_keeps(keeps_text)
    refused = [
        say("SHELL PAST A LIMIT -- migrate to Python:", baseline_ratchet.violations(SPEC, current, baseline, renames), MIGRATE),
        say(f"THE SHELL BASELINE GREW ({BASELINE} only falls; no trailer admits a rise):",
            range_growth(ranges, baseline, renames), MIGRATE),
        say("THE SHELL KEEPS REFUSED:", problems + stale_keeps(keeps, current)),
        baseline_ratchet.refused_falls(SPEC.title, BASELINE, SPEC.update_command,
                                       baseline_ratchet.falls(SPEC, current, baseline)),
    ]
    if not any(refused):
        print(f"shell gate: within {BASELINE} ({len(frozen_files(current))} shell tools frozen, the migration queue)")
    return not any(refused)


def queue_lines(baseline: Rows, keeps_text: str) -> List[str]:
    """The migration queue: each frozen script with its rows, the most rows past a limit first; then the keeps."""
    kept = {k.path: k for k in parse_keeps(keeps_text)[0]}
    rows: Dict[str, List[str]] = {}
    for (kind, key), value in sorted(baseline.items()):
        if kind in KINDS:
            rows.setdefault(key.split(":", 1)[0], []).append(f"{kind} {key.partition(':')[2] or key} {baseline_ratchet.show(value)}")
    order = sorted(rows, key=lambda p: (-len(rows[p]), p))
    out = [f"{p}\t{'; '.join(rows[p])}" for p in order if p not in kept]
    return out + [f"KEEP {p}\t{kept[p].added}\t{kept[p].why}" for p in order if p in kept]


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", choices=("check", "update", "queue"))
    parser.add_argument("--range", action="append", default=[])
    args = parser.parse_args(argv)
    files, git = tool_files.Files.here(), tool_git.Git.here()  # the composition root: the checkout it runs in
    try:
        current = measure(shell_files(git, files), run_blocks(git, files), rules_table.load("."))
        baseline = baseline_ratchet.parse(files.read_text_or(BASELINE, ""))
        keeps_text = files.read_text_or(KEEPS, "")
        if args.command == "update":
            baseline_ratchet.update(SPEC, current)
            return 0
        if args.command == "queue":
            print("\n".join(queue_lines(baseline, keeps_text)))
            return 0
        return 0 if check(current, baseline, keeps_text, args.range) else 1
    except (OSError, ValueError, KeyError, tool_git.Failed) as e:
        print(f"shell gate: {e}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
