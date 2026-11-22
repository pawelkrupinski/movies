"""THE MIRROR a what-if reads (scripts/identity_what_if_cli.py; its header's "A MIRROR CHANGING UNDER THE RUN"): its
generation (scripts/local-mirror/freshness --generation: the seed generation and the tail position's digest), pinned
before the base and compared after each side; THE FENCE holding its tailer for the run (scripts/local-mirror/
fence-rule.js: `<expiry> run|linger|drain` in `<fence dir>/what-if-<pid>`, written whole: a `.part` renamed), lingering
once the sides are read; and THE DRAIN of a mirror far behind before it is fenced."""
from __future__ import annotations

import hashlib
import os
import re
import sys
import time
from typing import List, Optional, Tuple

from identity_what_if_cli_context import Context, Exit, alive
import tool_process  # (scripts/lib, on the path the context put it on)

DIGITS = re.compile(r"^[0-9]+$")


def field_of(report: str, kind: str, cc: str, column: int) -> str:
    """awk's `$1 == kind && $2 == cc { print $column }` over the report."""
    found = [f[column] if len(f) > column else "" for f in (line.split() for line in report.split("\n"))
             if len(f) > 1 and f[0] == kind and f[1] == cc]
    return "\n".join(found)


class Mirror:
    def __init__(self, ctx: Context) -> None:
        self.ctx = ctx
        self.root = ctx.env.get("KINOWO_MIRROR_FENCE_DIR") or os.path.join(ctx.env.get("HOME", ""), ".cache", "kinowo",
                                                                           "mirror-fence")
        self.linger_minutes = int(ctx.knob("KINOWO_WHAT_IF_FENCE_LINGER_MINUTES", "20"))
        self.drain_minutes = int(ctx.knob("KINOWO_WHAT_IF_DRAIN_MINUTES", "20"))
        self.drain_wait = int(ctx.knob("KINOWO_WHAT_IF_DRAIN_WAIT_SECONDS", "900"))
        self.drain_poll = float(ctx.knob("KINOWO_WHAT_IF_DRAIN_POLL_SECONDS", "10"))
        self.fences: List[str] = []

    def freshness(self, cc: str, what: str) -> Tuple[int, str, str]:
        argv = [os.path.join(self.ctx.repo, "scripts/local-mirror/freshness"), cc, "--quiet", what]
        done = self.ctx.runner.capture(tool_process.Command(argv, self.ctx.env))
        return done.code, done.stdout, done.stderr

    def generation(self, cc: str) -> str:
        """`<seed generation>-tail<digest of the tail position>`, or the bare `seeding` / `reseed-due`; exit 2 when the
        mirror does not answer or reports no generation."""
        code, report, err = self.freshness(cc, "--generation")
        sys.stderr.write(err)
        sys.stderr.write("".join(line + "\n" for line in report.rstrip("\n").split("\n")
                                 if line and not re.match(r"^(GENERATION|TAIL) ", line)))
        if code == 2:
            self.ctx.log(f"{cc}: the local prod mirror does not answer (scripts/local-mirror/freshness)")
            raise Exit(2)
        gen, tail = field_of(report, "GENERATION", cc, 2), field_of(report, "TAIL", cc, 2)
        if not gen:
            self.ctx.harness_error(f"{cc}: mirror not ready: freshness reported no seed generation")
            raise Exit(2)
        if gen in ("seeding", "reseed-due"):
            return gen
        if not tail:
            self.ctx.harness_error(f"{cc}: mirror not ready: freshness reported no tail position")
            raise Exit(2)
        return f"{gen}-tail{hashlib.sha1(tail.encode()).hexdigest()[:12]}"

    def ready(self, cc: str) -> str:
        """The generation a run of `cc` pins itself to; exit 2 while a seed is in flight or due."""
        gen = self.generation(cc)
        if gen == "seeding":
            self.ctx.harness_error(f"{cc}: mirror not ready: a seed is in flight (or died, leaving a torn copy): its "
                                   "collections are fragments until it finishes -- wait for scripts/local-mirror/"
                                   f"freshness {cc} to stop warning, then run again")
            raise Exit(2)
        if gen == "reseed-due":
            self.ctx.harness_error(f"{cc}: mirror not ready: its stream state is gone and a re-seed is due -- wait for "
                                   f"it to finish (scripts/local-mirror/freshness {cc}), then run again")
            raise Exit(2)
        return gen

    def unchanged(self, cc: str, pinned: str, when: str) -> bool:
        """Whether the mirror is still at `pinned`; said as a HARNESS ERROR when it re-seeded or tailed a write since."""
        now = self.generation(cc)
        if now == pinned:
            return True
        if now.split("-tail")[0] == pinned.split("-tail")[0]:
            self.ctx.harness_error(
                f"{cc}: the mirror tailed a write during the run (generation {pinned} before the base, {now} {when}): "
                "the sides read different corpora, so no take is reported moved and nothing is recorded -- its fence "
                "did not hold (is the mirror service running a checkout with scripts/local-mirror/fence-rule.js? a "
                "fence older than its limits?); run again")
        else:
            self.ctx.harness_error(
                f"{cc}: the mirror was re-seeded during the run (generation {pinned} before the base, {now} {when}): "
                "the sides read different corpora, so no take is reported moved and nothing is recorded -- run again "
                f"once scripts/local-mirror/freshness {cc} is quiet")
        return False

    def tail_state(self, cc: str) -> Optional[Tuple[str, str]]:
        """(tail position, seconds behind or `?`) from freshness's report whatever its exit (a warning still reports)."""
        _, report, _ = self.freshness(cc, "--tail")
        for line in report.split("\n"):
            f = line.split() + ["", ""]
            if f[0] == "TAIL" and f[1] == cc:
                return f[2], behind_of(f[3], int(time.time()))
        return None

    def fence_dir(self, cc: str) -> str:
        return os.path.join(self.root, "kinowo" if cc == "pl" else f"kinowo_{cc}")

    def put(self, path: str, expiry: int, kind: str) -> None:
        with open(path + ".part", "w") as f:
            f.write(f"{expiry} {kind}\n")
        os.replace(path + ".part", path)

    def fence(self, cc: str) -> None:
        """`cc`'s tailer held for this run, after the drain when the mirror is far behind."""
        directory = self.fence_dir(cc)
        os.makedirs(directory, exist_ok=True)
        self.other_runs(directory)
        Drain(self, cc, directory).run()
        path = os.path.join(directory, f"what-if-{self.ctx.pid}")
        self.put(path, int(time.time()) + 4 * 3600, "run")
        self.fences.append(path)

    def linger(self) -> None:
        """Every fence of this run turned into a `linger` one: a rerun within it reads the same copy."""
        for path in self.fences:
            if os.path.isfile(path):
                self.put(path, int(time.time()) + self.linger_minutes * 60, "linger")
        self.fences = []

    def other_runs(self, directory: str) -> List[str]:
        """The live `run` fences in `directory` but this run's, a dead run's and every expired fence removed."""
        others: List[str] = []
        for name in sorted(os.listdir(directory)):
            path = os.path.join(directory, name)
            if not name.startswith("what-if-") or not os.path.isfile(path):
                continue
            expiry, kind = fence_line(path)
            pid = name[len("what-if-"):]
            if expired(expiry) or (kind != "linger" and not alive(pid)):
                remove(path)
            elif kind == "run" and pid != str(self.ctx.pid):
                others.append(name)
        return others


def behind_of(synced: str, now: int) -> str:
    """The seconds the tail's last change (ms since the epoch) lies behind `now`, `?` without one."""
    return str(int(now - int(synced) / 1000)) if DIGITS.match(synced) else "?"


def fence_line(path: str) -> Tuple[str, str]:
    try:
        with open(path) as f:
            parts = f.readline().split(None, 1)
    except OSError:
        return "", ""
    return (parts[0] if parts else ""), (parts[1].strip() if len(parts) > 1 else "")


def expired(expiry: str) -> bool:
    """`[ "${expiry:-0}" -le now ]`: an expiry that is no number is no answer, never expired."""
    value = expiry or "0"
    return bool(re.match(r"^-?[0-9]+$", value)) and int(value) <= int(time.time())


def remove(path: str) -> None:
    try:
        os.remove(path)
    except OSError:
        pass


class Drain:
    """THE DRAIN of one country's mirror: past KINOWO_WHAT_IF_DRAIN_MINUTES behind, every other run's fence waited
    for, then -- this run's `drain` fence lifting every linger -- the tail's backlog; each wait bounded."""

    def __init__(self, mirror: Mirror, cc: str, directory: str) -> None:
        self.mirror, self.ctx, self.cc, self.directory = mirror, mirror.ctx, cc, directory
        self.started = self.ctx.seconds()

    def waited(self) -> int:
        return self.ctx.seconds() - self.started

    def run(self) -> None:
        state = self.mirror.tail_state(self.cc)
        behind = state[1] if state else ""
        if not DIGITS.match(behind) or int(behind) <= self.mirror.drain_minutes * 60:
            return
        self.ctx.log(f"{self.cc}: the mirror is {int(behind) // 60} min behind (past {self.mirror.drain_minutes}): "
                     "draining the tail's backlog before fencing")
        if self.others_gone():
            self.mirror.put(os.path.join(self.directory, f"what-if-{self.ctx.pid}"),
                            int(time.time()) + self.mirror.drain_wait + 60, "drain")
            self.backlog()

    def others_gone(self) -> bool:
        """Whether every other run's fence went within the wait (False: said, fenced undrained)."""
        others = self.mirror.other_runs(self.directory)
        while others:
            listed = "".join(o + " " for o in others)
            if self.waited() >= self.mirror.drain_wait:
                self.ctx.log(f"{self.cc}: other runs still hold the tail after {self.mirror.drain_wait}s ({listed}): "
                             "fencing an undrained copy")
                return False
            self.ctx.log(f"{self.cc}: waiting for the runs holding the tail to end: {listed}")
            self.ctx.sleep(self.mirror.drain_poll)
            others = self.mirror.other_runs(self.directory)
        return True

    def backlog(self) -> None:
        """Waited for until behind by under 2 min, or the tail's position standing still for 3 polls."""
        last, still = "", 0
        while True:
            state = self.mirror.tail_state(self.cc)
            position, behind = state if state else ("", "")
            still = still + 1 if position == last else 0
            if self.drained(behind, still):
                return
            last = position
            self.ctx.sleep(self.mirror.drain_poll)

    def drained(self, behind: str, still: int) -> bool:
        """Whether the backlog wait ends here: no tail to read, drained (said), or out of time (said)."""
        if not DIGITS.match(behind):
            return True
        if int(behind) < 120 or still >= 3:
            self.ctx.log(f"{self.cc}: drained ({behind}s behind) in {self.waited()}s")
            return True
        if self.waited() >= self.mirror.drain_wait:
            self.ctx.log(f"{self.cc}: the tail did not drain in {self.mirror.drain_wait}s ({behind}s behind): "
                         "fencing as it is")
            return True
        return False
