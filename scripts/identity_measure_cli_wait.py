"""WAITING for a measure run (scripts/identity_measure_cli.py): THE TAKES ROWS, NOT THE RUN (2026-10-22) -- the verdict is
the takes rows' artifacts, uploaded as each row ends, and the run's tail (request-bisect, timings) is a job hop apiece,
queued behind every other run's work for a runner (on the old self-hosted CI it added 32 minutes to run 790's 52). So
with IDENTITY_MEASURE_EARLY_COLLECT=1 the wait ends at whichever comes first: every measured country's takes row done,
or the run done (cancelled included). Off by default: this tool deletes the branch at exit, which would pull it from
under the run's tail. --wait says the run's ETA once and polls with ~/bin/agentctl wait's backoff (60 s, x1.5, at most
5 min); without it every IDENTITY_MEASURE_POLL seconds. --timeout only stops the WAITING: the run and its branch stay
for --run (exit 3), unless --cancel-on-timeout."""
from __future__ import annotations

import json
from typing import Dict, NoReturn, Optional, Tuple

from cli import tool_countries
from vcs import tool_dispatch
from identity_measure_cli_context import TAKES_ROW_JQ, Context, Die, Exit, say

ENDED = ("completed", "success", "failure", "error", "cancelled", "skipped")
BACKOFF = "waiting with backoff (60 s, x1.5, at most 5 min)"


class Waiter:
    def __init__(self, c: Context, dispatcher: tool_dispatch.Dispatcher, workflow: str) -> None:
        self.c = c
        self.workflow = workflow  # whose recent measure/ runs give the ETA
        self.dispatcher = dispatcher
        early = c.env.get("IDENTITY_MEASURE_EARLY_COLLECT") or ""
        self.early = early not in ("", "0")
        self.cap = int(c.env.get("DISPATCH_BACKOFF_CAP") or 300)
        self.unreadable = False
        self.unread = 0

    def for_takes(self, run: str, seconds: float) -> Optional[str]:
        """The run's conclusion, or `takes-done`; None at the deadline (or ten unreadable reads in a row)."""
        end, delay = self.c.clock() + seconds, int(self.c.env.get("IDENTITY_MEASURE_WAIT_FIRST") or 60)
        self.unread = 0
        while True:
            answer = self.answer(run)
            if answer != "":
                return answer
            if self.c.clock() >= end:
                return None
            delay = self.pause(delay)

    def answer(self, run: str) -> Optional[str]:
        """The run's conclusion, `takes-done`, "" while it runs, or None once it could not be read ten times in a row
        (as scripts/ci-wait's: one is a blip, ten are no answer)."""
        view = self.read(run)
        self.unread = 0 if view is not None else self.unread + 1
        if self.unread >= 10:
            say(f"run {run} could not be read ten times in a row")
            self.unreadable = True
            return None
        if view is not None and view[0] == "completed":
            return view[1] or "completed"
        return "takes-done" if self.early and self.takes_rows_done(run) else ""

    def pause(self, delay: int) -> int:
        """--wait's backoff, else a read every IDENTITY_MEASURE_POLL seconds: the next delay."""
        if not self.c.o.wait:
            self.c.sleep(self.c.poll)
            return delay
        self.c.sleep(delay)
        return tool_dispatch.backoff_next(delay, self.cap)

    def read(self, run: str) -> Optional[Tuple[str, str]]:
        out = self.c.gh(["run", "view", run, "--json", "status,conclusion"])
        try:
            answer = json.loads(out or "")
            return str(answer["status"]), str(answer.get("conclusion") or "")
        except (ValueError, KeyError, TypeError):
            return None

    def takes_rows_done(self, run: str) -> bool:
        rows = self.c.gh(["run", "view", run, "--json", "jobs", "--jq",
                          TAKES_ROW_JQ + '(.jobs // [])[] | (.name | takes_country) as $c | [$c, .status] | @tsv'])
        if rows is None:
            return False
        states: Dict[str, str] = {}
        for line in rows.splitlines():
            name, _, status = line.partition("\t")
            states.setdefault(name, status)
        return all(states.get(tool_countries.name(cc), "") in ENDED for cc in self.c.state.countries.split(","))

    def say_eta(self, run: str) -> None:
        """--wait: the run and its ETA said once, before the wait."""
        if not self.c.o.wait:
            return
        eta = self.dispatcher.eta(self.workflow, "measure/")
        say(f"run {run}: ETA {eta} -- {BACKOFF}" if eta else
            f"run {run}: no ETA (no recent measure run to time) -- {BACKOFF}")

    def default_timeout(self) -> None:
        """The default --timeout follows the ETA: max(120, 1.5 x ETA) minutes; an explicit one stands."""
        o = self.c.o
        if o.timeout_set:
            return
        figure = (self.dispatcher.eta(self.workflow, "measure/", quiet=True) or "").lstrip("~").split(" ", 1)[0]
        if figure.isdigit() and int(figure) * 3 // 2 > o.timeout_min:
            o.timeout_min = int(figure) * 3 // 2
            say(f"--timeout {o.timeout_min} minutes (1.5 x the ETA of {figure})")

    def timed_out(self, run: str) -> NoReturn:
        why = f"run {run} gave no conclusion within --timeout {self.c.o.timeout_min} minutes"
        if self.unreadable or self.c.o.cancel_on_timeout:  # the clean-up cancels the run and deletes the branch
            raise Die(why)
        s = self.c.state
        s.keep = s.finished = True
        say(f"{why} -- STILL RUNNING, left running (no verdict)")
        say(f"collect it later with  scripts/identity-measure --run {run}   (it deletes {s.branch or 'its branch'}); "
            "--cancel-on-timeout cancels instead")
        raise Exit(3)
