"""identity-drift-ci's baseline and its wait (scripts/identity_drift_ci.py): with --for-red, the rows a red run failed --
their countries, and the red's OWN seeds pinned, so a row is held against the seed it failed on --; with --against, the
named run (or main's newest decided one) and its seeds; the rows the run must judge; and the poll until they have ended."""
from __future__ import annotations

import os
import re
import sys
import time
from typing import Any, Callable, List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
import drift_seeds  # noqa: E402
import identity_drift_ci_runs as runs_  # noqa: E402
from cli import tool_countries  # noqa: E402
from processes import machine  # noqa: E402
from vcs import tool_dispatch  # noqa: E402
from identity_drift_ci_runs import Row, say  # noqa: E402
from cli.tool_cli import UsageError  # noqa: E402

ROW_ORDER = ("pl", "de", "uk", "es", "us")  # the rows' order when no country is named


class Unread(Exception):
    """A read the tool cannot judge without: said, exit 2."""


def csv_add(items: List[str], item: str) -> None:
    if item not in items:
        items.append(item)


def expected_rows(countries: List[str], pins: str) -> List[str]:
    """`takes-drift-<row> (<country>)` for every row the country has (1, 2 and any further row its fixed seeds are
    spread over), or the rows the pins name (drift_seeds.py rows)."""
    rows: List[str] = []
    for cc in countries:
        try:
            numbers: List[int] = drift_seeds.rows_of(cc, None, pins)
        except (ValueError, KeyError, OSError):
            numbers = []
        rows += [f"takes-drift-{n} ({tool_countries.name(cc)})" for n in numbers]
    return rows


class Baseline:
    def __init__(self, reads: runs_.Runs, o: Any) -> None:
        self.reads = reads
        self.o = o
        self.ids: List[str] = []
        self.label = f"run {o.for_red}"

    def resolve(self) -> bool:
        """The baseline set (and the countries and pins it decides); False when a red has nothing to judge (said)."""
        if self.o.for_red and not self.for_red():
            print("DRIFT CLEAN (nothing to judge)")
            return False
        if self.o.against:
            self.against()
        return True

    def jobs(self, run: str) -> List[Row]:
        rows = self.reads.drift_jobs(run)
        if rows is None:
            raise Unread(f"could not read run {run}'s jobs")
        return rows

    def for_red(self) -> bool:
        red = self.o.for_red
        if not red.isdigit():
            raise UsageError(f"--for-red takes a run id or number, not {red}")
        listed = self.reads.listed(["--limit", "300", "--json", "databaseId,number"])
        if listed is None:
            raise Unread(f"could not list {self.reads.workflow}'s runs")
        ids = [str(r["databaseId"]) for r in listed if red in (str(r.get("databaseId")), str(r.get("number")))]
        if not ids:
            say(f"run {red} is no {self.reads.workflow} run among its newest 300: no drift row to judge")
            return False
        red_rows = self.failed_rows(ids)
        if not red_rows:
            say(f"run {red} failed no takes-drift row: nothing for the drift check to judge")
            return False
        self.pin_the_red(red_rows)
        return True

    def failed_rows(self, ids: List[str]) -> List[str]:
        """The takes-drift rows the runs failed, sorted; each run that failed one a baseline."""
        red_rows: List[str] = []
        for run in ids:
            failed = [row.name for row in self.jobs(run) if row.conclusion == "failure"]
            if failed:
                self.ids.append(run)
            red_rows += failed
        return sorted(set(red_rows))

    def pin_the_red(self, red_rows: List[str]) -> None:
        numbers: List[str] = []
        countries: List[str] = []
        for name in red_rows:
            row = Row(name, "", "")
            csv_add(numbers, row.number)
            csv_add(countries, tool_countries.code(row.country))
        self.o.countries = countries
        extra = next((n for n in numbers if re.fullmatch(r"[3-9]|[1-9][0-9]+", n)), None)
        if extra is not None:
            # a pin reaches rows 1 and 2 alone (drift_seeds.py rows_of): a red extra row replays its FIXED seeds,
            # every row of its country with it -- the seeds the red replayed, since the set is fixed
            self.o.pins = ""
            say(f"run {self.o.for_red}'s red rows include takes-drift-{extra}, which no pin reaches: every row replays "
                "its fixed seeds")
        else:
            self.o.pins = self.reads.pins_of(self.ids[0], numbers)
        self.o.seeds = self.o.pins
        say(f"run {self.o.for_red}'s red drift rows: {','.join(red_rows)} -> countries {','.join(countries)}, seeds "
            f"{self.o.pins} (the red's own)")

    def against(self) -> None:
        against = self.o.against
        if against == "main":
            self.ids = [self.main_decided()]
        elif against.isdigit():
            self.ids = [against]
        else:
            raise UsageError(f"--against takes a run id or main, not {against}")
        self.label = f"run {self.ids[0]}"
        # the baseline's own seeds, pinned (unless --seeds named rows): the rows replay what it replayed
        wanted = ["1", "2"]
        if self.o.pins:
            one, two = (self.o.pins.split(",", 1) + [""])[:2]
            wanted = (["1"] if one else []) + (["2"] if two else [])
        self.o.pins = self.reads.pins_of(self.ids[0], wanted)
        self.o.seeds = self.o.pins
        say(f"every row held against {self.label} ({against}), on its seeds {self.o.pins}")

    def main_decided(self) -> str:
        """main's newest decided run whose drift rows ran (a reused or cancelled run judged none)."""
        listed = self.reads.listed(["--branch", "main", "--limit", "10", "--json", "databaseId,status"]) or []
        for run in [str(r["databaseId"]) for r in listed if r.get("status") == "completed"]:
            rows = self.reads.drift_jobs(run) or []
            if any(row.conclusion in ("success", "failure") for row in rows):
                return run
        raise Unread(f"no decided run of {self.reads.workflow} on main judged its drift rows: nothing to hold this one against")


class Poll:
    """The wait with backoff (IDENTITY_DRIFT_FIRST_WAIT, 60 s, x1.5, at most DISPATCH_BACKOFF_CAP), a line a poll, and
    $KINOWO_PROGRESS_FILE touched, so the pre-push hook's no-progress watchdog sees it alive."""

    def __init__(self, reads: runs_.Runs, run: str, expected: List[str], clock: Callable[[], float], started: float,
                 env: Mapping[str, str], sleep: Callable[[float], None] = time.sleep) -> None:
        self.reads, self.run, self.expected, self.clock, self.started, self.env = reads, run, expected, clock, started, env
        self.sleep = sleep
        self.unread = 0

    def until_ended(self, minutes: int) -> Optional[List[Row]]:
        deadline, delay = self.clock() + minutes * 60, int(self.env.get("IDENTITY_DRIFT_FIRST_WAIT") or 60)
        cap = int(self.env.get("DISPATCH_BACKOFF_CAP") or 300)
        while True:
            self.progress()
            rows = self.poll()
            if rows is not None:
                return rows
            if self.clock() >= deadline:
                # --timeout only stops the WAITING: the run (and its branch, the run's to delete) goes on
                say(f"run {self.run}: the drift rows did not end within --timeout {minutes} minutes -- STILL RUNNING, "
                    "left running (no verdict)")
                say(f"collect it later with  scripts/identity-drift-ci --run {self.run}")
                return None
            self.sleep(delay)
            delay = tool_dispatch.backoff_next(delay, cap)

    def poll(self) -> Optional[List[Row]]:
        """The rows once every wanted one (or the run) has ended; None while not, or while the run cannot be read --
        ten unreadable reads in a row end the wait."""
        rows = self.reads.drift_jobs(self.run)
        status = self.reads.status(self.run) if rows is not None else None
        if rows is None or status is None:
            self.unread += 1
            if self.unread >= 10:
                raise Unread(f"run {self.run}: ten unreadable reads in a row")
            return None
        self.unread = 0
        return rows if self.said(rows, status) or status == "completed" else None

    def said(self, rows: List[Row], status: str) -> bool:
        """Whether every wanted row has ended, said either way."""
        want = self.expected or [row.name for row in rows if row.name]
        ended = {row.name for row in rows if row.status == "completed"}
        still = sorted(set(want) - ended)
        say(f"run {self.run}: {len(want) - len(still)}/{len(want)} drift row(s) ended, run {status}, "
            f"{int(self.clock() - self.started) // 60} min")
        return bool(want) and not still

    def progress(self) -> None:
        path = self.env.get("KINOWO_PROGRESS_FILE")
        if path:
            try:
                with open(path, "a") as f:
                    f.write(f"{int(machine.wall())}\n")
            except OSError:
                pass
