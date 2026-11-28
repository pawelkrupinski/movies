"""WHAT A RUN SAYS (scripts/run-script-tests): a row a test run -- `<STATUS> <seconds>s  <path>[ (run i/N)][ (floor)]`,
PASS / FAIL / STALLED / TIMEOUT / POLLUTE, sorted by path (agentctl's queuefailure reads them) --, each failure's block
(its failing cases from the whole log, its last 60 lines, where the whole log is kept), STRESS's verdict a test, and
the times each PASS took, merged into $RUN_SCRIPT_TESTS_DURATIONS for the next run's longest-first order."""
from __future__ import annotations

import os
import re
import tempfile
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Sequence, Tuple

from script_tests.script_tests_pool import TAIL, Result, failing_cases, kept_log

FIELD3 = re.compile(r"^\s*\S+\s+\S+(.*)$")


@dataclass
class Report:
    paths: Sequence[str]
    runs: Sequence[str]
    floor_tests: Sequence[str]
    log_dir: str
    stress: int
    stamp: Callable[[], str]
    rows: List[str] = field(default_factory=lambda: [])
    failures: List[str] = field(default_factory=lambda: [])
    seconds: Dict[str, int] = field(default_factory=lambda: {})
    failed_paths: List[str] = field(default_factory=lambda: [])
    failed: int = 0

    def add(self, index: int, result: Result, log: str) -> None:
        status, path = result.status(), self.paths[index]
        floor = " (floor)" if path in self.floor_tests else ""
        self.rows.append("%-7s %4ss  %s%s%s" % (status, result.seconds, path, self.runs[index], floor))
        if status == "PASS":
            if not self.stress:
                self.seconds[path] = result.seconds
            return
        self.failed += 1
        self.failed_paths.append(path)
        self.failures.append(f"\n── {status} {path}{self.runs[index]} (exit {result.code}) ──")
        with open(log, encoding="utf-8", errors="replace") as f:
            lines = f.read().splitlines()
        self.failures += failing_cases(lines) + ["    " + line for line in lines[-TAIL:]]
        kept = kept_log(log, self.log_dir, path, self.stamp())
        if kept:
            self.failures.append(f"    (whole log: {kept})")

    def pollute(self, path: str, lines: Sequence[str]) -> None:
        """A test that left paths in the checkout: its row says so, and a block names each path."""
        self.failed += 1
        for i, row in enumerate(self.rows):
            fields = row.split()
            if len(fields) >= 3 and fields[2] == path:
                self.rows[i] = "POLLUTE" + row[7:] if row.startswith("PASS   ") else row + " (and POLLUTE)"
        self.failures.append(f"\n── POLLUTE {path}: left in the checkout, and made again when it ran alone ──")
        self.failures += ["    " + line for line in lines]
        self.failures.append("    (a test writes only under its own mktemp scratch, and installs its tools outside the "
                             "tree: ~/.cache/kinowo/<tool>/<lock hash>)")

    def table(self) -> List[str]:
        """The rows sorted from their third field on (`sort -k3`), then whole."""
        def key(row: str) -> Tuple[bytes, bytes]:
            m = FIELD3.match(row)
            return ((m.group(1) if m else row).encode(), row.encode())
        return sorted(self.rows, key=key)

    def stress_verdicts(self, tests: int) -> List[str]:
        return [f"STRESS  {p}: {self.failed_paths.count(p)} of {self.stress} run(s) failed" for p in self.paths[:tests]]


def recorded(durations: str) -> Dict[str, int]:
    out: Dict[str, int] = {}
    try:
        with open(durations) as f:
            for line in f.read().splitlines():
                path, _, secs = line.partition("\t")
                if secs.isdigit():
                    out[path] = int(secs)
    except OSError:
        pass
    return out


def longest_first(root: str, paths: Sequence[str], durations: str) -> List[str]:
    """The slowest (by its last PASS) first; one with no record before them all, the larger file first."""
    secs = recorded(durations)

    def key(path: str) -> Tuple[int, int, bytes]:
        if path in secs:
            return (1, -secs[path], path.encode())
        return (0, -os.path.getsize(os.path.join(root, path)), path.encode())
    return sorted(paths, key=key)


def keep_times(durations: str, seconds: Dict[str, int]) -> None:
    """This run's times kept: the cache's other rows stay, these replace theirs; written aside and renamed in."""
    if not seconds:
        return
    try:
        os.makedirs(os.path.dirname(durations) or ".", exist_ok=True)
        rows = [f"{p}\t{s}" for p, s in seconds.items()]
        try:
            with open(durations) as f:
                rows += [line for line in f.read().splitlines() if line.partition("\t")[0] not in seconds]
        except OSError:
            pass
        handle, temporary = tempfile.mkstemp(prefix=os.path.basename(durations) + ".",
                                             dir=os.path.dirname(durations) or ".")
        with os.fdopen(handle, "w") as f:
            f.write("".join(row + "\n" for row in rows))
        os.replace(temporary, durations)
    except OSError:
        pass


def floor_block(findings: Sequence[str], bash: str) -> List[str]:
    """The floor's findings (they fail the run whatever its tests say): said, none when there are none."""
    if not findings:
        return []
    return ["", *findings, f"run-script-tests: {len(findings)} thing(s) the floor bash {bash} cannot run (FLOOR)"]


def heavy_hint(heavy: Sequence[str]) -> str:
    names = "".join(" " + h for h in heavy)
    return (f"run-script-tests: concurrency-heavy, selected:{names} -- `scripts/run-script-tests --stress 4{names}` "
            "(PREPUSH_STRESS=4 at a push) runs each 4 times at once under CPU load (STRESS)")
