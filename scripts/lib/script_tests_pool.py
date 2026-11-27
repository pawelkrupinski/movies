"""RUNNING THE SELF-TESTS (scripts/run-script-tests): each from the checkout's root, `bash` for .sh and `python3` for
.py, stdin closed, descriptor 7 closed, under tool_bounded.py (a process group of its own: the whole group ended at a
--timeout, and a Ctrl-C reaching it) and the no-progress watchdog (stall_watch.py, KINOWO_STALL_GROUPS=1: a test that
has gone --stall seconds with no output, no CPU and nothing runnable is ended, STALLED, saying what it was stuck on).
At most --jobs at once, the longest (by the seconds its last PASS took) first; each one's log, status, seconds and
window kept for the report and the tree check."""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
from dataclasses import dataclass, field
from typing import Callable, Dict, List, MutableMapping, Optional, Sequence

import machine
import tool_python
from script_tests_found import declared_width

TAIL = 60
CASE = ("✗", "FAIL: ", "ERROR: ")


@dataclass(frozen=True)
class Tools:
    """Where the runner's helpers are: tool_bounded.py, stall_watch.py, stall-report.sh."""
    bounded: str
    watch: str
    report: str


@dataclass
class Plan:
    """What a run runs: the tests (a path a run, stress copies repeated), each run's label, and how."""
    root: str
    scratch: str
    paths: List[str]
    runs: List[str]
    jobs: int
    timeout: int
    stall: int
    floor_tests: Sequence[str] = ()
    floor_bin: str = ""
    test_width: Optional[int] = None
    wide_tests: bool = False
    budget: int = 0  # cores the tests running at once may add up to (0: no budget, only `jobs`)
    more: int = 0  # cores beyond `jobs` (or the budget) the run still wants, taken as the governor frees them


@dataclass
class Result:
    code: int = 0
    seconds: int = 0
    start: int = 0
    end: int = 0
    stalled: bool = False

    def status(self) -> str:
        if self.code == 0:
            return "PASS"
        if self.stalled:
            return "STALLED"
        return "TIMEOUT" if self.code in (124, 137) else "FAIL"


@dataclass
class Running:
    key: str
    root: "subprocess.Popen[bytes]"
    watch: "subprocess.Popen[bytes]"
    began: float
    start: int


class Widening:
    """THE RUN WIDENED AS THE LAPTOP FREES: a grant is the cores free at admission, and a run admitted at load 28 was
    one wide for its 40 minutes. While tests wait, one more core is asked of the governor (`take`, never waiting; it
    answers None while none is free) every RETRY seconds, and each one taken lets one more test run; all are given
    back (`give`) once nothing waits."""
    RETRY = 10.0

    def __init__(self, take: Callable[[], Optional[object]], give: Callable[[object], None], more: int,
                 clock: Callable[[], float]) -> None:
        self.take, self.give, self.more, self.clock = take, give, more, clock
        self.held: List[object] = []
        self.next_try = 0.0

    @property
    def extra(self) -> int:
        return len(self.held)

    def tick(self, waiting: bool) -> None:
        if not waiting:
            self.release()
        elif len(self.held) < self.more and self.clock() >= self.next_try:
            core = self.take()
            if core is not None:
                self.held.append(core)
            self.next_try = self.clock() + (0.0 if core is not None else self.RETRY)

    def release(self) -> None:
        while self.held:
            self.give(self.held.pop())


@dataclass
class Pool:
    """The run: its plan, the environment its tests get, and each finished run's result."""
    plan: Plan
    env: MutableMapping[str, str]
    tools: Tools
    sleep: Callable[[float], None]
    results: Dict[int, Result] = field(default_factory=lambda: {})
    widening: Optional[Widening] = None

    def file(self, index: object, kind: str) -> str:
        return os.path.join(self.plan.scratch, f"{index}.{kind}")

    def test_env(self, path: str) -> Dict[str, str]:
        env = dict(self.env)
        width = declared_width(os.path.join(self.plan.root, path)) if self.plan.wide_tests else self.plan.test_width
        if width is not None:
            env["AGENTCTL_GOV_WIDTH"] = str(width)
        else:
            env.pop("AGENTCTL_GOV_WIDTH", None)
        if path in self.plan.floor_tests:
            env["PATH"] = self.plan.floor_bin + os.pathsep + env.get("PATH", "")
        env["BOUNDED_ON_TIMEOUT"] = self.tools.report
        return env

    def interpreter(self, path: str) -> str:
        if path.endswith(".py"):
            return "python3"
        return os.path.join(self.plan.floor_bin, "bash") if path in self.plan.floor_tests else "bash"

    def start(self, key: str, path: str) -> Running:
        log = open(self.file(key, "log"), "w")
        argv = tool_python.isolated(self.tools.bounded, str(self.plan.timeout), self.interpreter(path), path,
                                    python=sys.executable)
        root = machine.spawn(argv, machine.Ran(cwd=self.plan.root, env=self.test_env(path), out=log, err=log))
        stall_env = dict(self.env, KINOWO_STALL_GROUPS="1",
                         KINOWO_STALL_POLL_SECONDS=self.env.get("KINOWO_STALL_POLL_SECONDS") or str(self.stall_poll()))
        watch = machine.spawn(tool_python.isolated(self.tools.watch, "watch", str(root.pid), str(self.plan.stall),
                               path, self.file(key, "stalled"), self.file(key, "log"), python=sys.executable),
                              machine.Ran(cwd=self.plan.root, env=stall_env, out=log, err=log))
        log.close()
        return Running(key, root, watch, machine.monotonic(), int(machine.wall()))

    def stall_poll(self) -> int:
        return min(30, max(1, self.plan.stall // 10))

    def finish(self, r: Running) -> Result:
        machine.send(r.watch.pid, signal.SIGTERM)
        r.watch.wait()
        code = r.root.returncode if r.root.returncode is not None else r.root.wait()
        marks = self.file(r.key, "stalled")
        stalled = os.path.exists(marks) and os.path.getsize(marks) > 0
        return Result(128 - code if code < 0 else code, int(machine.monotonic() - r.began), r.start, int(machine.wall()),
                      stalled)

    def cost(self, index: int) -> int:
        """The cores a planned run holds of the budget: its declared width, or one."""
        path = self.plan.paths[index]
        return min(declared_width(os.path.join(self.plan.root, path)), self.plan.budget) if self.plan.wide_tests else 1

    def fits(self, running: Sequence[Running], index: int) -> bool:
        """Whether the next run fits the budget beside those running (one always runs: it cannot wait on nothing)."""
        if not self.plan.budget or not running:
            return True
        return sum(self.cost(int(r.key)) for r in running) + self.cost(index) <= self.plan.budget + self.extra()

    def extra(self) -> int:
        return self.widening.extra if self.widening else 0

    def room(self, running: Sequence[Running], upcoming: Sequence[int]) -> bool:
        """Whether the next planned run may start now: a place among `jobs` (and the cores taken beyond them), the budget."""
        cap = self.plan.jobs + (0 if self.plan.budget else self.extra())
        return bool(upcoming) and len(running) < cap and self.fits(running, upcoming[0])

    def reap(self, running: List[Running], report: Callable[[int, Result], None]) -> None:
        """Each run that has ended: out of `running`, its result kept and reported."""
        for r in [r for r in running if r.root.poll() is not None]:
            running.remove(r)
            self.results[int(r.key)] = self.finish(r)
            report(int(r.key), self.results[int(r.key)])

    def run_all(self, report: Callable[[int, Result], None]) -> None:
        """Every planned run, at most `jobs` at once, each reported as it ends."""
        running: List[Running] = []
        upcoming = list(range(len(self.plan.paths)))
        while running or upcoming:
            if self.widening:
                self.widening.tick(bool(upcoming))
            while self.room(running, upcoming):
                i = upcoming.pop(0)
                running.append(self.start(str(i), self.plan.paths[i]))
            self.reap(running, report)
            if running:
                self.sleep(0.2)
        if self.widening:
            self.widening.release()

    def run_alone(self, index: int) -> int:
        """One planned run again, alone (the tree check's): its seconds."""
        r = self.start(f"again-{index}", self.plan.paths[index])
        r.root.wait()
        machine.send(r.watch.pid, signal.SIGTERM)
        r.watch.wait()
        return int(machine.monotonic() - r.began)


def failing_cases(lines: Sequence[str]) -> List[str]:
    """Each failing case of a log longer than the report's tail: a shell spec's `✗` line and the three after it, a
    unittest `FAIL:` / `ERROR:` header, at most 80 lines, under a heading."""
    if len(lines) <= TAIL:
        return []
    cases: List[str] = []
    left = 0
    for line in lines:
        if "✗" in line or line.startswith(CASE[1:]):
            left = 4
        if left > 0:
            cases.append(line)
            left -= 1
    if not cases:
        return []
    return ["    failing cases (from the whole log):", *("    " + c for c in cases[:80]), "    last 60 lines:"]


def kept_log(log: str, log_dir: str, path: str, stamp: str) -> Optional[str]:
    """The whole log kept under `log_dir`: its path, None when it cannot be."""
    kept = os.path.join(log_dir, f"{stamp}-{os.getpid()}-{os.path.basename(path)}.log")
    try:
        os.makedirs(log_dir, exist_ok=True)
        shutil.copyfile(log, kept)
    except OSError:
        return None
    return kept
