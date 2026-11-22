"""ONE REPLAY of the drift harness (identity_drift_check.py): scripts.IdentityDrift by sbt in a checkout, over the HEAD's
recording, beside THE MEMORY GUARD (scripts/ci/memory-guard.sh: a heartbeat every 5 minutes, and the replay ended with
its cause named before the runner runs out of memory -- Identity takes #40's US replay starved its runner and lost the
job's whole log) -- and, for a JVM that died in HotSpot, its crash report and compiler-replay data kept beside the
replay's output and named (Identity takes #17: SIGSEGV in C2, its hs_err file lost with the base's worktree)."""
from __future__ import annotations

import glob
import os
import shutil
import signal
import subprocess
import sys
from typing import List, Mapping, Optional, Protocol, Sequence


class Handle(Protocol):
    pid: int

    def wait(self) -> int: ...

    def poll(self) -> Optional[int]: ...

    def send_signal(self, sig: int) -> None: ...


class Processes(Protocol):
    def start(self, argv: Sequence[str], env: Mapping[str, str], cwd: str, log: Optional[str]) -> Handle: ...


class SubprocessProcesses:
    """The real one: a child process, its output into `log` (both streams), or inherited."""

    def start(self, argv: Sequence[str], env: Mapping[str, str], cwd: str, log: Optional[str]) -> Handle:
        sys.stdout.flush()
        sys.stderr.flush()
        if log is None:
            return subprocess.Popen(list(argv), env=dict(env), cwd=cwd)
        with open(log, "wb") as sink:
            return subprocess.Popen(list(argv), env=dict(env), cwd=cwd, stdout=sink, stderr=subprocess.STDOUT)


class Stop(Exception):
    """The check ends here with no verdict (exit 2); `said` went to the verdict and stdout already."""


def tail(path: str, n: int = 40) -> str:
    try:
        with open(path, errors="replace") as f:
            return "".join(f.readlines()[-n:])
    except OSError:
        return ""


def line_after(path: str, heading: str) -> str:
    """The line after the first `heading` line (`grep -A1 h | tail -1`), stripped of its leading `#`s and spaces."""
    with open(path, errors="replace") as f:
        lines = f.read().split("\n")
    for n, line in enumerate(lines):
        if line.startswith(heading):
            return lines[n + 1] if n + 1 < len(lines) else line
    return ""


class Replayer:
    def __init__(self, root: str, cc: str, heap: str, out: str, env: Mapping[str, str], processes: Processes) -> None:
        self.root = root
        self.cc = cc
        self.heap = heap
        self.out = out
        self.env = env
        self.processes = processes
        self.fixtures = os.path.join(root, "test/resources/fixtures")
        self.code = 0

    def command(self, seeds: str, to: str, options: Sequence[str]) -> List[str]:
        fixtures, cc = self.fixtures, self.cc
        main = (f"worker/Test/runMain scripts.IdentityDrift {cc} --recording {fixtures} --families "
                f"{fixtures}/identity-unmatched/{cc}.json.gz {' '.join(options)} --seed {seeds} --out {to}")
        return ["sbt", f"-J-Xmx{self.heap}", "set worker / Test / run / fork := false", main]

    def replay(self, checkout: str, to: str, seeds: str, options: Sequence[str] = ()) -> int:
        """One replay into `to`; its sbt exit. A replay the guard ended stops the check (Stop)."""
        os.makedirs(to, exist_ok=True)
        log, reason = os.path.join(to, "sbt.log"), os.path.join(to, "memory-guard.txt")
        sbt = self.processes.start(self.command(seeds, to, options), self.env, checkout, log)
        if os.path.exists(reason):
            os.remove(reason)
        guard_env = dict(self.env, MEMORY_GUARD_LABEL=f"identity drift ({self.cc}) memory guard")
        guard = self.processes.start([os.path.join(self.root, "scripts/ci/memory-guard.sh"), str(sbt.pid), reason, log],
                                     guard_env, self.root, None)
        self.code = sbt.wait()
        stop_guard(guard, reason)
        if os.path.isfile(reason) and os.path.getsize(reason) > 0:
            sys.stderr.write(tail(log))
            with open(reason) as f:
                said = (f"identity drift ({self.cc}): the replay was ended before the runner ran out of memory, so nothing"
                        f" was judged: {f.read().rstrip()}")
            self.say_verdict(said)
            raise Stop(said)
        if self.code != 0:
            self.jvm_crash(checkout, to)
        return self.code

    def say_verdict(self, said: str) -> None:
        print(said, flush=True)
        with open(os.path.join(self.out, "verdict.txt"), "w") as f:
            f.write(said + "\n")

    def jvm_crash(self, checkout: str, to: str) -> None:
        kept = ""
        for pattern in ("hs_err_pid*.log", "replay_pid*.log"):
            for path in sorted(glob.glob(os.path.join(checkout, pattern))):
                shutil.move(path, os.path.join(to, os.path.basename(path)))
                kept += " " + os.path.basename(path)
        if not kept:
            return
        for report in sorted(glob.glob(os.path.join(to, "hs_err_pid*.log"))):
            frame = line_after(report, "# Problematic frame:").lstrip("#").lstrip(" ")
            task = line_after(report, "Current CompileTask:")
            print(f"identity drift ({self.cc}): the JVM crashed (exit {self.code}) in {frame or 'an unnamed frame'}"
                  f"{'; compiling: ' + task if task else ''}; kept:{kept} in {to}", file=sys.stderr, flush=True)


def stop_guard(guard: Handle, reason: str) -> None:
    """memory-guard-stop.sh's memory_guard_stop: an idle guard is stopped now; one ending the tree (its reason written)
    finishes its KILL pass. Either way this succeeds."""
    if not (os.path.isfile(reason) and os.path.getsize(reason) > 0) and guard.poll() is None:
        try:
            guard.send_signal(signal.SIGTERM)
        except OSError:
            pass
    guard.wait()
