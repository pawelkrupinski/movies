"""THE CPU METER (scripts/jvm-gate.sh's cpu_meter_start / cpu_meter_stop, for the Python tools): the CPU time of the
processes a lister names and of all their descendants, sampled every KINOWO_CPU_METER_POLL seconds (2) while a call
runs, a process that ended counted to its last sample. Sampled, not `ps -S`: macOS's ps adds no exited child's CPU to
its parent's, so a server's forked test JVMs went uncounted (2026-11-25). The sampler is a daemon thread of the caller:
it ends with it, however it ends."""
from __future__ import annotations

import threading
from typing import Callable, Dict, List, Optional

import machine


def centis(clock: str) -> int:
    """ps's `time` -- [[dd-]hh:]mm:ss[.cc] -- in centiseconds, truncated (the shell meter's awk)."""
    days, _, rest = clock.rpartition("-")
    seconds = 0.0
    for part in rest.split(":"):
        seconds = seconds * 60 + float(part)
    return int((seconds + (int(days) * 86400 if days else 0)) * 100)


def table() -> str:
    return machine.ps("-axo", "pid=,ppid=,time=")


def sample(roots: List[int], listing: Callable[[], str] = table) -> Dict[int, int]:
    """{pid: CPU centiseconds} for each of `roots` and its descendants, from one ps listing."""
    if not roots:
        return {}
    cpu: Dict[int, int] = {}
    kids: Dict[int, List[int]] = {}
    for line in listing().splitlines():
        fields = line.split()
        if len(fields) == 3 and fields[0].isdigit() and fields[1].isdigit():
            cpu[int(fields[0])] = centis(fields[2])
            kids.setdefault(int(fields[1]), []).append(int(fields[0]))
    found: Dict[int, int] = {}
    queue = list(roots)
    for pid in queue:
        if pid in cpu and pid not in found:
            found[pid] = cpu[pid]
            queue.extend(kids.get(pid, []))
    return found


def all_alive(pids: List[int], alive: Callable[[int], bool]) -> bool:
    return bool(pids) and all(alive(p) for p in pids)


class CpuMeter:
    def __init__(self, lister: Callable[[], List[int]], poll: float, alive: Callable[[int], bool],
                 sampler: Callable[[List[int]], Dict[int, int]] = sample) -> None:
        self.lister, self.poll, self.alive, self.sampler = lister, poll, alive, sampler
        self.base: Dict[int, int] = {}
        self.top: Dict[int, int] = {}
        self.done = threading.Event()
        self.thread: Optional[threading.Thread] = None
        self.lock = threading.Lock()

    def start(self) -> None:
        self.base = self.sampler(self.lister())
        self.top = dict(self.base)
        self.thread = threading.Thread(target=self._run, daemon=True)
        self.thread.start()

    def _seen(self, found: Dict[int, int]) -> None:
        with self.lock:
            for pid, used in found.items():
                self.top[pid] = max(used, self.top.get(pid, used))

    def _run(self) -> None:
        roots: List[int] = []
        while not self.done.wait(self.poll):
            if not all_alive(roots, self.alive):
                roots = self.lister()
            self._seen(self.sampler(roots))

    def stop(self) -> int:
        """The CPU seconds the trees used since start (0 when it never started)."""
        if self.thread is None:
            return 0
        self.done.set()
        self.thread.join()
        self._seen(self.sampler(self.lister()))
        used = sum(max(0, top - self.base.get(pid, 0)) for pid, top in self.top.items())
        self.thread = None
        return used // 100
