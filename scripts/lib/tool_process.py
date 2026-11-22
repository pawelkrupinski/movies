"""RUNNING ANOTHER COMMAND, the one way the Python tools do it (scripts/lib; the shell tools' `bounded` and `"$@" 2>&1 |
tee log`): a `Runner` is injected into every tool at its composition point, so a test hands in a fake that records the
commands, and the real one is `SubprocessRunner`. A `Command` says how it runs:

  Command(argv, env)                       inheriting this process's stdin, stdout and stderr
  Command(argv, env, log=PATH)             stdout and stderr into the file at PATH (appended with append=True)
  Command(argv, env, log=PATH, tee=True)   ... and to this process's stdout as they come (`2>&1 | tee PATH`)
  closed_stdin=True                        stdin from /dev/null (`< /dev/null`)
  cwd=DIR                                  run in DIR
  timeout=SECONDS                          ended past it, its whole process group -- TERM, a moment, then KILL --
                                           exit 124 (tool-conventions.sh's `bounded`), teed or not. Its group is a
                                           session of its own, out of reach of whatever ends this process (a caller's
                                           bound, a cancelled check, Ctrl-C), so, as `bounded` does, an INT or TERM
                                           this process gets while it runs ends its group first and is then passed
                                           on, and so does an exception: it never outlives the tool that ran it.

`run` returns the exit status; `capture` returns it with stdout and stderr as text (`$(cmd)`), the command's stderr
captured unless `log` takes both.

The exit status is the command's own; a command that cannot be found is 127 and one that cannot be executed 126, as a
shell says."""
from __future__ import annotations

import os
import select
import signal
import subprocess
import sys
import threading
import time
from dataclasses import dataclass
from types import FrameType, TracebackType
from typing import BinaryIO, Callable, Dict, Mapping, Optional, Protocol, Sequence, Union

NOT_FOUND, NOT_EXECUTABLE, TIMED_OUT = 127, 126, 124
GRACE = 2.0  # seconds a TERMed group has before it is KILLed
Handler = Union[Callable[[int, Optional[FrameType]], object], int, signal.Handlers, None]


@dataclass(frozen=True)
class Command:
    argv: Sequence[str]
    env: Mapping[str, str]
    log: Optional[str] = None
    append: bool = False
    tee: bool = False
    closed_stdin: bool = False
    timeout: Optional[float] = None
    cwd: Optional[str] = None  # the directory it runs in (this process's by default)


@dataclass(frozen=True)
class Captured:
    code: int
    stdout: str
    stderr: str


class Runner(Protocol):
    def run(self, command: Command) -> int: ...

    def capture(self, command: Command) -> Captured: ...



class SubprocessRunner:
    """The real runner: each command a child process of this one."""

    def __init__(self, stdout: Optional[BinaryIO] = None) -> None:
        self.stdout = stdout or sys.stdout.buffer

    def run(self, command: Command) -> int:
        sys.stdout.flush()
        try:
            with EndedWithThisProcess(command.timeout) as guard:
                return self._run(command, guard)
        except FileNotFoundError:
            return self._failed(command.argv, "command not found", NOT_FOUND)
        except PermissionError:
            return self._failed(command.argv, "permission denied", NOT_EXECUTABLE)

    def _run(self, command: Command, guard: "EndedWithThisProcess") -> int:
        if command.log is None:
            return self._wait(guard.started(self._start(command, None, None)), command.timeout)
        with open(command.log, "ab" if command.append else "wb") as sink:
            if command.tee:
                return self._tee(command, sink, guard)
            return self._wait(guard.started(self._start(command, sink, subprocess.STDOUT)), command.timeout)

    def capture(self, command: Command) -> Captured:
        sys.stdout.flush()
        with EndedWithThisProcess(command.timeout) as guard:
            try:
                proc = guard.started(self._start(command, subprocess.PIPE, subprocess.PIPE))
            except FileNotFoundError:
                return Captured(NOT_FOUND, "", f"{command.argv[0]}: command not found\n")
            except PermissionError:
                return Captured(NOT_EXECUTABLE, "", f"{command.argv[0]}: permission denied\n")
            try:
                out, err = proc.communicate(timeout=command.timeout)
                return Captured(proc.returncode, out.decode(errors="replace"), err.decode(errors="replace"))
            except subprocess.TimeoutExpired:
                end_group(proc)
                out, err = proc.communicate()
                return Captured(TIMED_OUT, out.decode(errors="replace"), err.decode(errors="replace"))

    def _start(self, command: Command, stdout: Union[int, BinaryIO, None],
               stderr: Optional[int]) -> "subprocess.Popen[bytes]":
        return subprocess.Popen(list(command.argv), env=dict(command.env), stdin=self.stdin(command), cwd=command.cwd,
                                stdout=stdout, stderr=stderr, start_new_session=command.timeout is not None)

    @staticmethod
    def _wait(proc: "subprocess.Popen[bytes]", timeout: Optional[float]) -> int:
        try:
            return proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            end_group(proc)
            return TIMED_OUT

    @staticmethod
    def stdin(command: Command) -> Optional[int]:
        return subprocess.DEVNULL if command.closed_stdin else None

    def _tee(self, command: Command, sink: BinaryIO, guard: "EndedWithThisProcess") -> int:
        proc = guard.started(self._start(command, subprocess.PIPE, subprocess.STDOUT))
        source = proc.stdout
        assert source is not None
        deadline = None if command.timeout is None else time.monotonic() + command.timeout
        with source:
            chunk = _read_by(source.fileno(), deadline)
            while chunk:
                self.stdout.write(chunk)
                self.stdout.flush()
                sink.write(chunk)
                chunk = _read_by(source.fileno(), deadline)
        if chunk is None:
            end_group(proc)
            proc.wait()
            return TIMED_OUT
        return self._wait(proc, None if deadline is None else max(deadline - time.monotonic(), 0.0))

    @staticmethod
    def _failed(argv: Sequence[str], why: str, code: int) -> int:
        print(f"{os.path.basename(argv[0]) if argv else '?'}: {why}", file=sys.stderr)
        return code


def _read_by(fd: int, deadline: Optional[float]) -> Optional[bytes]:
    """The next chunk the command wrote (b"" at its end), or None once `deadline` passed with nothing to read."""
    wait = None if deadline is None else max(deadline - time.monotonic(), 0.0)
    readable, _, _ = select.select([fd], [], [], wait)
    return os.read(fd, 65536) if readable else None


class EndedWithThisProcess:
    """A bounded command (one with a timeout) runs in a session of its own, out of reach of whatever ends this process,
    so for as long as this guard is held an INT or TERM to this process ends the command's group and is then this
    process's own again, passed on under the handler there was before -- as `bounded` does --, and an exception out
    of the wait ends it too. The handlers are set BEFORE the command starts: a signal arriving while it is being
    started is held until it has (`started`), then acted on. A signal this process ignores (a background job's INT)
    stays ignored; only the main thread may set a handler, so elsewhere, and for an unbounded command (in this
    process's own group, ended with it already), nothing is changed."""

    def __init__(self, timeout: Optional[float]) -> None:
        self.proc: "Optional[subprocess.Popen[bytes]]" = None
        self.pending: Optional[int] = None
        self.before: Dict[signal.Signals, Handler] = {}
        if timeout is not None and threading.current_thread() is threading.main_thread():
            self.before = {sig: signal.getsignal(sig) for sig in (signal.SIGINT, signal.SIGTERM)
                           if signal.getsignal(sig) != signal.SIG_IGN}

    def __enter__(self) -> "EndedWithThisProcess":
        for sig in self.before:
            signal.signal(sig, self._signalled)
        return self

    def started(self, proc: "subprocess.Popen[bytes]") -> "subprocess.Popen[bytes]":
        self.proc = proc
        if self.pending is not None:
            self._stop(self.pending)
        return proc

    def _signalled(self, signum: int, _frame: Optional[FrameType]) -> None:
        if self.proc is None:
            self.pending = signum
            return
        self._stop(signum)

    def _stop(self, signum: int) -> None:
        if self.proc is not None:
            end_group(self.proc)
        self._restore()
        os.kill(os.getpid(), signum)

    def _restore(self) -> None:
        for sig, handler in self.before.items():
            signal.signal(sig, handler or signal.SIG_DFL)

    def __exit__(self, kind: Optional[type], _value: Optional[BaseException], _trace: Optional[TracebackType]) -> None:
        if kind is not None and self.before and self.proc is not None and self.proc.poll() is None:
            end_group(self.proc)
        self._restore()


def end_group(proc: "subprocess.Popen[bytes]") -> None:
    """The command's whole process group ended: TERM, GRACE seconds to go, then KILL whatever is left of it."""
    for sig in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(proc.pid, sig)
        except OSError:
            return
        deadline = time.monotonic() + GRACE
        while time.monotonic() < deadline and proc.poll() is None:
            time.sleep(0.05)
        if sig == signal.SIGTERM and not group_alive(proc.pid):
            return


def group_alive(pgid: int) -> bool:
    try:
        os.killpg(pgid, 0)
    except OSError:
        return False
    return True
