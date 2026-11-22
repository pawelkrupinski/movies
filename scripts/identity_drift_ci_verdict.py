"""identity-drift-ci's verdict (scripts/identity_drift_ci.py): each requested row's own verdict, with its report's lines,
and -- with a baseline (--for-red, --against) -- held against that run's verdict for the same row. The last line is the
verdict, the return the exit (0 clean, 1 red, 2 no verdict)."""
from __future__ import annotations

import os
import re
import shutil
from dataclasses import dataclass, field
from typing import List, Tuple

import identity_drift_ci_runs as runs_
from identity_drift_ci_runs import Row, open_findings
import tool_countries

REPORT_LINE = re.compile(r"^## |^  [0-9]+ listing|^      ")


def text(path: str, otherwise: str) -> str:
    try:
        with open(path) as f:
            return f.read().rstrip("\n")
    except OSError:
        return otherwise


@dataclass
class Tally:
    red: List[str] = field(default_factory=lambda: [])
    unjudged: List[str] = field(default_factory=lambda: [])
    better: List[str] = field(default_factory=lambda: [])
    same: List[str] = field(default_factory=lambda: [])
    worse: List[str] = field(default_factory=lambda: [])
    unknown_base: List[str] = field(default_factory=lambda: [])


@dataclass(frozen=True)
class Request:
    run: str
    out: str
    expected: List[str]  # empty: --run, the rows the run itself has
    base_ids: List[str]
    base_label: str
    against: str
    for_red: str
    wall: int


class Verdict:
    def __init__(self, reads: runs_.Runs, request: Request) -> None:
        self.reads = reads
        self.r = request
        self.base_rows: List[Tuple[Row, str]] = []
        self.tally = Tally()

    def collect(self) -> None:
        shutil.rmtree(self.r.out, ignore_errors=True)
        os.makedirs(self.r.out)
        if not self.reads.download(self.r.run, self.r.out):
            runs_.say(f"could not download run {self.r.run}'s identity-drift artifacts (each row's verdict still stands)")
        for run in self.r.base_ids:
            rows = self.reads.drift_jobs(run)
            if rows is None:
                runs_.say(f"could not read baseline run {run}'s jobs")
            self.base_rows += [(row, run) for row in rows or []]
            shutil.copytree(self.reads.base(run), os.path.join(self.r.out, f"base-{run}"))

    def was_open(self, name: str, artifact: str) -> str:
        """The baseline's open findings for the row: 0 where its job passed, its verdict's count where it failed,
        nothing where unknown."""
        found = [(row, run) for row, run in self.base_rows if row.name == name and row.conclusion in ("success", "failure")]
        if not found:
            return ""
        row, run = found[0]
        return "0" if row.conclusion == "success" else open_findings(os.path.join(self.r.out, f"base-{run}", artifact,
                                                                                    "verdict.txt"))

    def judge(self, rows: List[Row]) -> int:
        for row in rows:
            if row.name and self._requested(row):
                self._row(row)
        names = {row.name for row in rows}
        missing = sorted(set(self.r.expected) - names) if self.r.expected else []
        print(f"run {self.r.run}, {self.r.wall} min from dispatch; artifacts in {self.r.out}")
        judged = len([row for row in rows if row.conclusion in ("success", "failure")])
        line, code = conclude(self.tally, missing, self.r, judged)
        print(line)
        return code

    def _requested(self, row: Row) -> bool:
        if self.r.expected:
            return row.name in self.r.expected
        return row.conclusion != "skipped"  # --run: a country a drift-only dispatch left out judges nothing

    def _row(self, row: Row) -> None:
        artifact = f"identity-drift-{tool_countries.code(row.country)}-takes-drift-{row.number}"
        verdict = self._said(row, os.path.join(self.r.out, artifact))
        if row.conclusion == "failure":
            self.tally.red.append(row.name)
        elif row.conclusion != "success":
            self.tally.unjudged.append(f"{row.name} ({row.conclusion})")
        if self.r.base_ids and row.conclusion in ("success", "failure"):
            now = "0" if row.conclusion == "success" else open_findings(verdict)
            self._held(row, now, self.was_open(row.name, artifact))

    @staticmethod
    def _said(row: Row, directory: str) -> str:
        """The row's verdict, reproducer and report lines printed: its verdict.txt's path."""
        verdict = os.path.join(directory, "verdict.txt")
        print(f"{row.name} [seed {text(os.path.join(directory, 'seed'), '?')}]: {row.conclusion} -- "
              f"{text(verdict, 'no verdict.txt')}")
        if row.conclusion == "failure" and os.path.isfile(os.path.join(directory, "reproduce.txt")):
            print(f"    reproduce: {text(os.path.join(directory, 'reproduce.txt'), '')}")
        report = text(os.path.join(directory, "report.txt"), "")
        for line in [line for line in report.split("\n") if REPORT_LINE.match(line)][:40]:
            print(f"    {line}")
        return verdict

    def _held(self, row: Row, now: str, was: str) -> None:
        """The row against the baseline's: better, the same, worse -- or unknown, a count unreadable on either side."""
        name = row.name
        if row.conclusion == "success":
            if was not in ("", "0"):
                self.tally.better.append(f"{name}: green")
            return
        if not now or not was:
            self.tally.unknown_base.append(name)
            return
        bucket = {-1: self.tally.better, 0: self.tally.same, 1: self.tally.worse}[(int(now) > int(was)) - (int(now) < int(was))]
        bucket.append(f"{name}: {was} -> {now} open")


def conclude(t: Tally, missing: List[str], r: Request, judged: int) -> Tuple[str, int]:
    if t.red and r.against:
        return against_verdict(t, missing, r.base_label)
    if t.red and r.for_red:
        return for_red_verdict(t, missing, r.for_red)
    if t.red:
        return f"DRIFT RED: {' '.join(t.red)}", 1
    if missing or t.unjudged:
        return f"NO VERDICT: {', '.join(t.unjudged + missing)} not judged", 2
    # (--run passes over skipped rows: a run whose every row was skipped -- its setup red, say -- judged nothing)
    if judged == 0:
        return f"NO VERDICT: run {r.run} has no takes-drift row that passed or failed", 2
    return "DRIFT CLEAN", 0


def unjudged_said(t: Tally, missing: List[str]) -> str:
    return f"{' '.join(t.unknown_base)}{' ' + ' '.join(t.unjudged) if t.unjudged else ''} {','.join(missing)}"


def against_verdict(t: Tally, missing: List[str], label: str) -> Tuple[str, int]:
    if t.worse:
        return f"DRIFT RED: worse than {label}: {' '.join(t.worse)}", 1
    if t.unknown_base or t.unjudged or missing:
        return f"NO VERDICT: not judged against {label}: {unjudged_said(t, missing)}", 2
    better = f" (better: {' '.join(t.better)})" if t.better else ""
    return f"DRIFT NOT WORSE than {label}: {' '.join(t.same)}{better}", 0


def for_red_verdict(t: Tally, missing: List[str], for_red: str) -> Tuple[str, int]:
    if t.worse or (not t.better and not t.unknown_base):
        worse = " ".join(t.worse) + " " if t.worse else ""
        return f"DRIFT RED: no better than run {for_red}: {worse}{' '.join(t.same)}", 1
    if t.unknown_base or t.unjudged or missing:
        return f"NO VERDICT: still red, and no baseline to tell a partial fix by: {unjudged_said(t, missing)}", 2
    return f"DRIFT PROGRESS: {'; '.join(t.better)} -- still red: {' '.join(t.red)}", 0

