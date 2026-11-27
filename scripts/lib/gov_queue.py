"""WHERE A RUN STANDS IN THE GOVERNOR'S QUEUE, said while it waits (scripts/run-script-tests; jvm_gate.JvmGate's
`governor_output`): a governed run that waits for its ticket looked hung, and two lanes killed their own queued runs.
Every `interval` seconds, until the run is admitted, one line on stderr from `agentctl gov status` and `gov stats`:

  run-script-tests: queued at the governor, 2nd of 3 waiting (5 admitted, 15/18 cores charged); the agent class's
                    admissions wait 0s at the median, 7s at the 90th percentile; killing it loses the place

Stdlib only; the commands, the sleep and the line's destination are injected."""
from __future__ import annotations

import contextlib
import re
import threading
from typing import Callable, Iterator, List, Optional, Tuple

WAITER = re.compile(r"^\s*WAITING\s+(\S+)\s.*\bpid\s+(\d+)\s*$")
ADMITTED = re.compile(r"^governor:\s*(\d+) admitted,.*?([\d.]+/\d+) cores charged")
CLASS_WAITS = re.compile(r"^\s*(\S+)\s+\d+\s+(\S+)\s+(\S+)\s+\d+\s*$")
Output = Callable[[List[str]], Optional[str]]


def ordinal(n: int) -> str:
    return f"{n}{'th' if 10 <= n % 100 <= 20 else {1: 'st', 2: 'nd', 3: 'rd'}.get(n % 10, 'th')}"


def position(status: str, pid: int) -> Optional[Tuple[int, int]]:
    """(this pid's 1-based place among the waiting, how many wait); None when it is not waiting."""
    waiting = [int(m.group(2)) for m in map(WAITER.match, status.splitlines()) if m]
    return (waiting.index(pid) + 1, len(waiting)) if pid in waiting else None


def charged(status: str) -> str:
    for line in status.splitlines():
        found = ADMITTED.match(line)
        if found:
            return f" ({found.group(1)} admitted, {found.group(2)} cores charged)"
    return ""


def class_waits(stats: str, governor_class: str) -> str:
    """The class's median and 90th percentile admission wait, as a sentence; "" when `gov stats` has no row for it."""
    for line in stats.splitlines():
        found = CLASS_WAITS.match(line)
        if found and found.group(1) == governor_class:
            return f"; the {governor_class} class's admissions wait {found.group(2)} at the median, {found.group(3)} at the 90th percentile"
    return ""


def describe(tool: str, status: str, stats: str, pid: int, governor_class: str) -> Optional[str]:
    """The line for a run that waits; None when it is not in the queue (admitted, or the governor does not list it)."""
    place = position(status, pid)
    if place is None:
        return None
    return (f"{tool}: queued at the governor, {ordinal(place[0])} of {place[1]} waiting{charged(status)}"
            f"{class_waits(stats, governor_class)}; ending it now loses its place")


class QueueReport:
    def __init__(self, tool: str, run: Output, pid: int, governor_class: str, say: Callable[[str], None],
                 interval: float) -> None:
        self.tool, self.run, self.pid, self.governor_class, self.say, self.interval = tool, run, pid, governor_class, say, interval
        self.stopped = threading.Event()

    def report_once(self, last: Optional[str]) -> Optional[str]:
        status = self.run(["status"])
        line = describe(self.tool, status, self.run(["stats"]) or "", self.pid, self.governor_class) if status else None
        if line and line != last:
            self.say(line)
        return line or last

    def watch(self) -> None:
        last: Optional[str] = None
        while not self.stopped.wait(self.interval):
            last = self.report_once(last)


@contextlib.contextmanager
def reporting(tool: str, run: Output, pid: int, governor_class: str, say: Callable[[str], None],
              interval: float) -> Iterator[QueueReport]:
    """While the body waits for its ticket, the queue line is said every `interval` seconds when it changes."""
    report = QueueReport(tool, run, pid, governor_class, say, interval)
    thread = threading.Thread(target=report.watch, daemon=True)
    thread.start()
    try:
        yield report
    finally:
        report.stopped.set()
        thread.join(5)
