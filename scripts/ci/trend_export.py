"""The collector (scripts/ci/ci_trends.py `export`, run on ci-runner every few minutes): every completed run of the
repository created since --since and not yet in the state file, as events POSTed to VictoriaLogs (or printed with
--dry-run). Idempotent and resumable: a run enters the state only once its events were accepted; one run's failure is
logged and the rest go on.

    from trend_export import command_export
"""
from __future__ import annotations

import argparse
import json
import os
import sys
from typing import Callable, Dict, List, Sequence, Tuple, TextIO

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import atomic  # noqa: E402

from trend_events import commit_change, events_of, post_events
from trend_github import GitHub, job_windows, record_of
from trend_measures import run_window, saturation
from trend_records import Record, iso, parse_when
from trend_source import Source

State = Dict[str, Dict[str, int]]
Post = Callable[[str, List[Record]], None]


def load_state(path: str) -> State:
    try:
        with open(path, encoding="utf-8") as handle:
            return json.load(handle)
    except (OSError, ValueError):
        return {"done": {}}


def save_state(path: str, state: State) -> None:
    atomic.write_json(path, state)


def _key(run: Record) -> str:
    return "%s/%s" % (run["id"], run.get("run_attempt") or 1)


def _log(message: str) -> None:
    print("ci-timings export: " + message, file=sys.stderr, flush=True)


class _Collector:
    """One pass: every job's window first (for each run's slot saturation), then each run read, sent and recorded."""

    def __init__(self, args: argparse.Namespace, github: GitHub, state: State, post: Post, out: TextIO) -> None:
        self.args = args
        self.github = github
        self.state = state
        self.post = post
        self.out = out
        self.windows: List[Tuple[float, float]] = []

    def read_windows(self, runs: Sequence[Record]) -> None:
        """Every job's window, one cached call per run."""
        completed = [run for run in runs if run.get("status") == "completed"]
        for index, run in enumerate(completed, 1):
            try:
                self.windows += job_windows(self.github, run)
            except Exception:  # noqa: BLE001 -- saturation is context; the run itself is read again below
                pass
            if index % 50 == 0 or index == len(completed):
                _log("job windows: %d of %d runs, %d API calls" % (index, len(completed), self.github.calls))

    def export_run(self, run: Record) -> int:
        """Read one run, send (or print) its events; how many there were."""
        record = record_of(self.github, run)
        if record["jobs"]:
            record["sat"] = saturation(run_window(record), self.windows, self.args.slots)
        record["change"] = commit_change(self.github, record.get("sha"))
        events = events_of(record)
        if self.args.dry_run:
            for event in events:
                self.out.write(json.dumps(event, separators=(",", ":"), default=str) + "\n")
        elif events:
            self.post(self.args.vl_url, events)
        return len(events)

    def save(self) -> None:
        if self.args.state and not self.args.dry_run:
            save_state(self.args.state, self.state)

    def send_all(self, pending: Sequence[Record]) -> Tuple[int, int]:
        """Each pending run sent and recorded in the state ON ITS OWN, so a pass cut short (a budget floor, a
        timeout) keeps what it sent and the journal shows it moving (2026-11-01: the first pass read every run before
        sending any, ~15 min of silence). (sent, failed)."""
        sent = failed = 0
        started = parse_when("now")
        for index, run in enumerate(sorted(pending, key=lambda run: run.get("created_at") or ""), 1):
            try:
                count = self.export_run(run)
            except Exception as error:  # noqa: BLE001 -- see the module docstring
                failed += 1
                _log("run %s not sent: %s" % (run["id"], error))
                continue
            sent += 1
            self.state["done"][_key(run)] = int(parse_when("now"))
            self.save()
            if index % 10 == 0 or index == len(pending):
                _log("sent %d of %d (%s %s, %d events), %d failed, %d API calls, %.0f s" % (
                    index, len(pending), run.get("name"), run["id"], count, failed, self.github.calls,
                    parse_when("now") - started))
        return sent, failed


def command_export(args: argparse.Namespace, source: Source, out: TextIO, post: Post = post_events) -> None:
    """The collector: every completed run of the repository (every workflow) created since --since and not yet in the
    state file, as events, POSTed to VictoriaLogs (or printed with --dry-run)."""
    until = parse_when(args.until or "now")
    since = parse_when(args.since or "2d")
    github = source.github
    state: State = load_state(args.state) if args.state else {"done": {}}
    runs = github.runs(created="%s..%s" % (iso(since)[:10], iso(until)[:10]))
    pending = [run for run in runs if run.get("status") == "completed" and _key(run) not in state["done"]]
    _log("%d runs in the window, %d to send, %d already in" % (len(runs), len(pending), len(runs) - len(pending)))
    collector = _Collector(args, github, state, post, out)
    collector.read_windows(runs)
    sent, failed = collector.send_all(pending)
    horizon = parse_when("100d")
    state["done"] = {k: v for k, v in state["done"].items() if v >= horizon}
    collector.save()
    _log("%d runs sent, %d failed, %d already in, %d API calls" % (sent, failed, len(runs) - len(pending), github.calls))
