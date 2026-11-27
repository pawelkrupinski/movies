"""BOUNDED: a command ended once it outlasts <seconds> (exit 124), else its own status -- timeout(1)'s contract without
timeout(1), which stock macOS lacks; tool-conventions.sh's `bounded` (its shell shim runs this) and run-script-tests'
root of every test.

  python3 -I -B scripts/lib/tool_bounded.py <seconds> <command...>     (0 seconds: no bound)

The command runs in a process group of its own, ended whole (TERM, then KILL two seconds later: an sbt-forked JVM too);
unlike GNU timeout, which leaves the terminal's group, a Ctrl-C reaches it -- passed on to the group, then to the
caller as the signal. Run in a terminal's foreground with it on stdin, the command's group holds the foreground while
it runs, so one touching the terminal (stty, a prompt) is not stopped, and a Ctrl-C there reaches the caller too.
BOUNDED_ON_TIMEOUT=<executable>: run at the deadline with the group's id, BEFORE the group is ended, to say what it was
stuck on (scripts/stall-report.sh). This call's alone: the command does not inherit it. The command keeps this
process's descriptors (a governor ticket's 7 among them) and stdin."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
from types import FrameType
from typing import List, MutableMapping, Optional, Set

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))  # run as `python3 -I`: its own directory is not there
import machine  # noqa: E402

ENDING = {signal.SIGINT, signal.SIGTERM}  # what ends the command, passed on to the caller
GRACE_POLLS = 20  # a tenth of a second each: TERM, then KILL what is left two seconds later


class Interrupted(Exception):
    def __init__(self, signum: int) -> None:
        super().__init__(signum)
        self.signum = signum


def interrupted(signum: int, _frame: Optional[FrameType]) -> None:
    raise Interrupted(signum)


class Group:
    """The command's process group, and the terminal it was handed (taken back for the caller's group, `home`)."""

    def __init__(self, proc: "subprocess.Popen[bytes]", tty: Optional[int], home: int) -> None:
        self.proc, self.tty, self.home = proc, tty, home

    def take_back(self) -> None:
        if self.tty is not None:
            try:
                os.tcsetpgrp(self.tty, self.home)
            except OSError:
                pass

    def end(self) -> None:
        """TERM to the whole group, a moment to go, then KILL whatever is left of it."""
        machine.send(self.proc.pid, signal.SIGTERM, group=True)
        for _ in range(GRACE_POLLS):
            try:
                self.proc.wait(0.1)
            except subprocess.TimeoutExpired:
                pass
            if not machine.send(self.proc.pid, 0, group=True):
                break
        machine.send(self.proc.pid, signal.SIGKILL, group=True)
        self.take_back()

    def wait(self, seconds: float) -> Optional[int]:
        """The command's raw status, or None once it outlasts `seconds` (0: no bound)."""
        deadline = machine.monotonic() + seconds
        while True:
            left = deadline - machine.monotonic() if seconds > 0 else None
            if left is not None and left <= 0:
                return None
            try:
                return self.proc.wait(left)
            except subprocess.TimeoutExpired:
                continue


def status_of(code: int) -> int:
    """A shell's view of a status: a signal N is 128 + N."""
    return 128 - code if code < 0 else code


def foreground_tty() -> Optional[int]:
    """Descriptor 0 when it is a terminal whose foreground is this process's group."""
    try:
        return 0 if os.isatty(0) and os.tcgetpgrp(0) == os.getpgrp() else None
    except OSError:
        return None


def bounded(seconds: float, command: List[str], env: MutableMapping[str, str]) -> int:
    on_timeout = env.pop("BOUNDED_ON_TIMEOUT", None)
    home, tty = os.getpgrp(), foreground_tty()
    if tty is not None:
        signal.signal(signal.SIGTTOU, signal.SIG_IGN)  # a group out of the foreground may then move it, as a shell does
    for signum in ENDING:
        signal.signal(signum, interrupted)
    # held back until finish can end the group: a Ctrl-C landing while Popen returns raised past it (#665)
    held = signal.pthread_sigmask(signal.SIG_BLOCK, ENDING)
    try:
        group = Group(machine.spawn_group(command, env, tty, held), tty, home)
    except OSError:
        signal.pthread_sigmask(signal.SIG_SETMASK, held)
        return 127
    return finish(group, seconds, on_timeout, held)


def finish(group: Group, seconds: float, on_timeout: Optional[str], held: Set[int]) -> int:
    try:
        signal.pthread_sigmask(signal.SIG_SETMASK, held)  # an interrupt held back is raised here, inside the try
        if group.tty is not None:
            try:  # whichever of the two runs first
                os.setpgid(group.proc.pid, group.proc.pid)
                os.tcsetpgrp(group.tty, group.proc.pid)
            except OSError:
                pass
        code = group.wait(seconds)
    except Interrupted as stop:
        group.end()
        signal.signal(stop.signum, signal.SIG_DFL)  # passed on as the signal: the caller sees it interrupted
        machine.send(os.getpid(), stop.signum)
        return 128 + stop.signum
    if code is None:
        if on_timeout:
            machine.status([on_timeout, str(group.proc.pid)])
        group.end()
        return 124
    group.take_back()
    return passed_on(group, code)


def passed_on(group: Group, code: int) -> int:
    """A Ctrl-C at the terminal reached only the command, the foreground: what it left running is ended with it, and
    the signal passed on to the group this was called from, as the terminal would have sent it."""
    if group.tty is not None and code in (-signal.SIGINT, -signal.SIGQUIT):
        group.end()
        for signum in (signal.SIGINT, signal.SIGQUIT):
            signal.signal(signum, signal.SIG_DFL)
        machine.send(group.home, -code, group=True)
    return status_of(code)


def main(argv: List[str]) -> int:
    if len(argv) < 2:
        print("usage: tool_bounded.py <seconds> <command...>", file=sys.stderr)
        return 64
    try:
        seconds = float(argv[0])
    except ValueError:
        print(f"tool_bounded.py: {argv[0]} is no number of seconds", file=sys.stderr)
        return 64
    return bounded(seconds, argv[1:], dict(machine.environment()))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
