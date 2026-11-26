"""scripts/sbt-warm's servers and keepers (its header: ONE SERVER, THE STOPS LOG, ONE KEEPER PER CHECKOUT): finding a
checkout's sbt servers by their working directory, stopping them and their keeper by pid -- never `sbt --client
shutdown`, which reaches whatever server active.json names --, and recording every stop in $State/stops.log."""
from __future__ import annotations

import datetime
import glob
import os
import re
import signal
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, List, Optional

from jvm_gate import lock_busy

STOPS_KEPT, STOPS_CUT = 5000, 2000


def command_out(argv: List[str]) -> str:
    try:
        return subprocess.run(argv, capture_output=True, text=True, stdin=subprocess.DEVNULL).stdout
    except OSError:
        return ""


class Procs:
    """The process table, through ps, pgrep and lsof on the PATH (a case shims them), and signals."""

    def args(self, pid: int) -> str:
        return command_out(["ps", "-o", "args=", "-p", str(pid)]).strip()

    def parent(self, pid: int) -> int:
        out = command_out(["ps", "-o", "ppid=", "-p", str(pid)]).strip()
        return int(out) if out.isdigit() else 0

    def cwd(self, pid: int) -> str:
        out = command_out(["lsof", "-a", "-p", str(pid), "-d", "cwd", "-Fn"])
        return next((line[1:] for line in out.splitlines() if line.startswith("n")), "")

    def matching(self, pattern: str) -> List[int]:
        return [int(p) for p in command_out(["pgrep", "-f", pattern]).split() if p.isdigit()]

    def alive(self, pid: int) -> bool:
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except PermissionError:
            return True
        return True

    def signal(self, pid: int, signum: int, group: bool = False) -> bool:
        try:
            if group:
                os.killpg(pid, signum)
            else:
                os.kill(pid, signum)
        except OSError:
            return False
        return True


def own_args() -> str:
    """This process's command line as a shell tool's reads ("python3 <script> <args>"): ps names a framework Python by
    its long executable path, which the stops log's 100 characters would keep alone."""
    return " ".join([os.path.basename(sys.executable), *sys.argv])


def physical(path: str) -> str:
    try:
        return os.path.realpath(path) if os.path.isdir(path) else ""
    except OSError:
        return ""


@dataclass(frozen=True)
class Checkout:
    """A checkout and where its records live: <state>/<key>.*"""
    root: str
    key: str
    state: str

    @property
    def prefix(self) -> str:
        return os.path.join(self.state, self.key)

    def file(self, suffix: str) -> str:
        return self.prefix + suffix


def read(path: str) -> str:
    try:
        with open(path) as f:
            return f.read().strip()
    except OSError:
        return ""


def remove(*paths: str) -> None:
    for path in paths:
        try:
            os.remove(path)
        except OSError:
            pass


class Servers:
    def __init__(self, procs: Procs, state: str, say: Callable[[str], None], sleep: Callable[[float], None],
                 now: Callable[[], datetime.datetime]) -> None:
        self.procs, self.state, self.say, self.sleep, self.now = procs, state, say, sleep, now
        self.stops_log = os.path.join(state, "stops.log")

    def pids(self, root: str) -> List[int]:
        """Every sbt server (sbt --client's: --detach-stdio) whose working directory is `root`, oldest first."""
        found: List[int] = []
        for pid in self.procs.matching(r"xsbt\.boot\.Boot|sbt-launch"):
            if "--detach-stdio" in self.procs.args(pid):
                cwd = self.procs.cwd(pid)
                if cwd and physical(cwd) == root:
                    found.append(pid)
        return sorted(found)

    def pid(self, root: str) -> Optional[int]:
        pids = self.pids(root)
        return pids[-1] if pids else None

    def stopper(self) -> str:
        """This process and two parents, "pid:args <- ppid:args <- ..." (the stops log's by=)."""
        pid = os.getpid()
        out: List[str] = []
        for _ in range(3):
            out.append(f"{pid}:{(own_args() if pid == os.getpid() else self.procs.args(pid))[:100]}")
            pid = self.procs.parent(pid)
            if pid <= 1:
                break
        return " <- ".join(out)

    def record(self, what: str, pid: int, checkout: str, why: str) -> None:
        stamp = self.now().strftime("%Y-%m-%d %H:%M:%S")
        with open(self.stops_log, "a") as f:
            f.write(f'{stamp} {what} pid={pid} checkout={checkout} why="{why}" by={self.stopper()}\n')
        with open(self.stops_log) as f:
            lines = f.readlines()
        if len(lines) > STOPS_KEPT:
            with open(self.stops_log + ".tmp", "w") as f:
                f.writelines(lines[-STOPS_CUT:])
            os.replace(self.stops_log + ".tmp", self.stops_log)
        said = f"server pid {pid} of {checkout} vanished" if what == "vanished" else f"stopped the {what} of {checkout} (pid {pid})"
        self.say(f"{said}: {why}")

    def stop(self, pid: int, checkout: str, why: str, kill: bool = False) -> bool:
        """The server `pid` of `checkout` signalled (TERM, or KILL), recorded; false when it could not be."""
        signum, name = (signal.SIGKILL, "SIGKILL") if kill else (signal.SIGTERM, "SIGTERM")
        if not self.procs.signal(pid, signum):
            return False
        self.record("server", pid, checkout, f"{why} ({name})")
        return True

    def await_none(self, root: str, seconds: int) -> None:
        for _ in range(seconds):
            if not self.pids(root):
                return
            self.sleep(1)

    def stop_all(self, root: str, why: str, killed_why: str, seconds: int = 10) -> bool:
        """Every server of `root` stopped (TERM, then KILL `seconds` later); whether one was."""
        stopped = self.stop_servers_of(root, why)
        self.await_none(root, seconds)
        for pid in self.pids(root):
            self.stop(pid, root, killed_why, kill=True)
        return stopped

    def dedupe(self, root: str) -> None:
        """At most ONE server per checkout: several are all stopped (2026-10-15: 69 leaked servers)."""
        pids = self.pids(root)
        if len(pids) <= 1:
            return
        self.say(f"{len(pids)} servers run in {root} ({' '.join(map(str, pids))}): stopping all, starting one")
        for pid in pids:
            self.stop(pid, root, f"one of {len(pids)} servers in one checkout")
        self.await_none(root, 10)
        for pid in self.pids(root):
            self.stop(pid, root, "one of several servers in one checkout, alive 10 s after SIGTERM", kill=True)
        remove(os.path.join(root, "project/target/active.json"))

    def stop_servers_of(self, root: str, why: str) -> bool:
        stopped = False
        for pid in self.pids(root):
            stopped = self.stop(pid, root, why) or stopped
        return stopped

    def keeper_pid(self, keep: str) -> Optional[int]:
        """The keeper `keep` names while its lock is held, once that pid is a keeper (up to 10 s: one just forked
        carries the call's command line until it has exec'd, Main #1259); None when the lock is free."""
        for _ in range(100):
            if not lock_busy(keep):
                return None
            pid = read(keep)
            if pid.isdigit() and "sbt-warm --keep" in self.procs.args(int(pid)):
                return int(pid)
            self.sleep(0.1)
        self.say(f"{keep} is held, but by no keeper in 10 s (pid {read(keep)}): left alone")
        return None

    def await_free(self, keep: str, tenths: int) -> bool:
        for _ in range(tenths):
            if not lock_busy(keep):
                return True
            self.sleep(0.1)
        return not lock_busy(keep)

    def stop_keeper(self, checkout: Checkout, why: str) -> None:
        """The checkout's keeper stopped with its process group, waiting for its lock: its JVM slot free on return."""
        keep = checkout.file(".keep.flock")
        pid = self.keeper_pid(keep)
        if pid is None:
            return
        if not self.procs.signal(pid, signal.SIGTERM, group=True):
            self.procs.signal(pid, signal.SIGTERM)
        where = read(checkout.file(".root")) or "?"
        if self.await_free(keep, 100):
            self.record("keeper", pid, where, why)
            return
        if not self.procs.signal(pid, signal.SIGKILL, group=True):
            self.procs.signal(pid, signal.SIGKILL)
        self.await_free(keep, 20)
        self.record("keeper", pid, where, f"{why} (SIGKILL: alive 10 s after SIGTERM)")

    def stop_checkout(self, checkout: Checkout, root: str, why: str) -> bool:
        """The checkout forgotten: its keeper and servers stopped, its records removed -- never its lock files."""
        self.stop_keeper(checkout, why)
        stopped = self.stop_servers_of(root, why)
        remove(*(checkout.file(s) for s in (".root", ".pid", ".used", ".env", ".build", ".serving")))
        return stopped

    def of_removed_checkout(self, pid: int) -> bool:
        """Whether `pid` is an sbt server whose checkout directory is gone (a pid file outlives its process)."""
        if "--detach-stdio" not in self.procs.args(pid):
            return False
        cwd = self.procs.cwd(pid)
        return not cwd or not os.path.isdir(cwd)

    def reap(self, idle_minutes: Optional[float]) -> None:
        """The server of every registered checkout that is gone stopped (with `idle_minutes`, every one unused that
        long too)."""
        for mark in sorted(glob.glob(os.path.join(self.state, "*.root"))):
            prefix = mark[: -len(".root")]
            root = read(mark)
            if not os.path.isdir(root):
                pid = read(prefix + ".pid")
                if pid.isdigit() and self.of_removed_checkout(int(pid)):
                    self.stop(int(pid), root, "its checkout was removed (reap)")
                remove(*glob.glob(glob.escape(prefix) + ".*"))
            elif idle_minutes is not None and older_than(prefix + ".used", idle_minutes, self.now):
                checkout = Checkout(physical(root), os.path.basename(prefix), self.state)
                self.stop_checkout(checkout, checkout.root, f"idle past {idle_minutes:g} minutes (--reap)")

    def vanished(self, pid: int, root: str, reaper_logs: List[str]) -> None:
        """The server `pid` its keeper watched has ended: unless the stops log names its stop, a "vanished" line with
        what else might know (the JVM reaper's kill, a crash file, a heap dump)."""
        recent = read(self.stops_log).splitlines()[-200:]
        if any(f" pid={pid} checkout={root} " in line for line in recent):
            return
        hits = [line for path in reaper_logs for line in read(path).splitlines()
                if re.search(r" pid=%d |killed [a-z -]+ pid %d " % (pid, pid), line)]
        found = "".join(line + " " for line in hits[-2:])
        crashes = [os.path.join(root, f"hs_err_pid{pid}.log"), *sorted(glob.glob(os.path.join(root, "target/oom-*"))),
                   os.path.join(os.path.expanduser("~"), f"hs_err_pid{pid}.log")]
        found += "".join(path + " " for path in crashes if os.path.exists(path))
        self.record("vanished", pid, root, "ended without sbt-warm stopping it; " +
                    (found or "no reaper kill, crash file or heap dump names it"))


def older_than(path: str, minutes: float, now: Callable[[], datetime.datetime]) -> bool:
    """Whether `path` was last modified more than `minutes` ago (`find -mmin +N`: whole minutes, rounded up)."""
    try:
        age = now().timestamp() - os.stat(path).st_mtime
    except OSError:
        return False
    whole = -(-age // 60)
    return whole > minutes
