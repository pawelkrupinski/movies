"""RUNNING ANOTHER COMMAND, the one way the Python tools do it (scripts/lib; the shell tools' `bounded` and `"$@" 2>&1 |
tee log`): a `Runner` is injected into every tool at its composition point, so a test hands in a fake that records the
commands, and the real one is `SubprocessRunner`. A `Command` says how it runs:

  Command(argv, env)                       inheriting this process's stdin, stdout and stderr
  Command(argv, env, log=PATH)             stdout and stderr into the file at PATH (appended with append=True)
  Command(argv, env, log=PATH, tee=True)   ... and to this process's stdout as they come (`2>&1 | tee PATH`)
  closed_stdin=True                        stdin from /dev/null (`< /dev/null`)
  cwd=DIR                                  run in DIR
  input=TEXT                               TEXT on its stdin (`<<< TEXT`; `capture` only)
  stderr_through=True                      `capture` leaves stderr this process's, as it comes (`$(cmd)`): only
                                           stdout is the answer
  stderr_log=PATH                          with log=PATH (not teed): stderr into the file at PATH instead, appended
                                           alike (`> log 2> PATH`)
  timeout=SECONDS                          ended past it, its whole process group -- TERM, a moment, then KILL --
                                           exit 124 (tool-conventions.sh's `bounded`), teed or not. Its group is a
                                           session of its own, out of reach of whatever ends this process (a caller's
                                           bound, a cancelled check, Ctrl-C), so, as `bounded` does, an INT or TERM
                                           this process gets while it runs ends its group first and is then passed
                                           on, from whichever thread it was started, and so do an exception and the
                                           exit: it never outlives the tool that ran it (LiveGroups).

`run` returns the exit status; `capture` returns it with stdout and stderr as text (`$(cmd)`), the command's stderr
captured unless `log` takes both.

The exit status is the command's own; a command that cannot be found is 127 and one that cannot be executed 126, as a
shell says."""
from __future__ import annotations

import atexit
import os
import select
import signal
import subprocess
import sys
import threading
import time
from dataclasses import dataclass
from types import FrameType, TracebackType
from typing import BinaryIO, Callable, Dict, Mapping, Optional, Protocol, Sequence, Set, Tuple, Union

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
    input: Optional[str] = None  # what its stdin reads (capture only)
    stderr_through: bool = False  # `capture`: stderr not captured but this process's
    stderr_log: Optional[str] = None  # with log: stderr into this file instead


@dataclass(frozen=True)
class Captured:
    code: int
    stdout: str
    stderr: str


class Runner(Protocol):
    def run(self, command: Command) -> int: ...

    def capture(self, command: Command) -> Captured: ...


def capture_loudly(runner: Runner, command: Command) -> Captured:
    """`capture`, the command's stderr passed on to this process's once it is said (`$(cmd)`, stderr not silenced)."""
    done = runner.capture(command)
    sys.stderr.write(done.stderr)
    sys.stderr.flush()
    return done



class SubprocessRunner:
    """The real runner: each command a child process of this one."""

    def __init__(self, stdout: Optional[BinaryIO] = None, groups: Optional[LiveGroups] = None) -> None:
        self.stdout = stdout or sys.stdout.buffer
        self.groups = groups or LiveGroups()

    def run(self, command: Command) -> int:
        sys.stdout.flush()
        try:
            with self.groups.guard(command.timeout) as guard:
                return self._run(command, guard)
        except FileNotFoundError:
            return self._failed(command.argv, "command not found", NOT_FOUND)
        except PermissionError:
            return self._failed(command.argv, "permission denied", NOT_EXECUTABLE)

    def _run(self, command: Command, guard: "Bounded") -> int:
        if command.log is None:
            return self._wait(guard.started(self._start(command, None, None)), command.timeout)
        with open(command.log, "ab" if command.append else "wb") as sink:
            if command.tee:
                return self._tee(command, sink, guard)
            if command.stderr_log is not None:
                with open(command.stderr_log, "ab" if command.append else "wb") as errors:
                    return self._wait(guard.started(self._start(command, sink, errors)), command.timeout)
            return self._wait(guard.started(self._start(command, sink, subprocess.STDOUT)), command.timeout)

    def capture(self, command: Command) -> Captured:
        sys.stdout.flush()
        with self.groups.guard(command.timeout) as guard:
            try:
                proc = guard.started(self._start(command, subprocess.PIPE,
                                                 None if command.stderr_through else subprocess.PIPE))
            except FileNotFoundError:
                return Captured(NOT_FOUND, "", f"{command.argv[0]}: command not found\n")
            except PermissionError:
                return Captured(NOT_EXECUTABLE, "", f"{command.argv[0]}: permission denied\n")
            try:
                text = None if command.input is None else command.input.encode()
                out, err = proc.communicate(text, timeout=command.timeout)
                return Captured(proc.returncode, out.decode(errors="replace"), (err or b"").decode(errors="replace"))
            except subprocess.TimeoutExpired:
                end_group(proc)
                out, err = _drained(proc)
                return Captured(TIMED_OUT, out.decode(errors="replace"), (err or b"").decode(errors="replace"))

    def _start(self, command: Command, stdout: Union[int, BinaryIO, None],
               stderr: Union[int, BinaryIO, None]) -> "subprocess.Popen[bytes]":
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
        if command.input is not None:
            return subprocess.PIPE
        return subprocess.DEVNULL if command.closed_stdin else None

    def _tee(self, command: Command, sink: BinaryIO, guard: "Bounded") -> int:
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


def _drained(proc: "subprocess.Popen[bytes]") -> Tuple[bytes, Optional[bytes]]:
    """What an ended group's pipes still hold, read for GRACE at most: a process that escaped into a session of its
    own may keep them open for ever (Tool self-tests #546), and the bound is the caller's, not that process's."""
    try:
        out, err = proc.communicate(timeout=GRACE)
        return out or b"", err
    except subprocess.TimeoutExpired as late:
        for pipe in (proc.stdout, proc.stderr):
            if pipe is not None:
                pipe.close()
        proc.wait()
        return late.stdout or b"", late.stderr


def _read_by(fd: int, deadline: Optional[float]) -> Optional[bytes]:
    """The next chunk the command wrote (b"" at its end), or None once `deadline` passed with nothing to read."""
    wait = None if deadline is None else max(deadline - time.monotonic(), 0.0)
    readable, _, _ = select.select([fd], [], [], wait)
    return os.read(fd, 65536) if readable else None


class LiveGroups:
    """The bounded commands a runner has running, each in a session of its own (out of reach of whatever ends this
    process): ended together when this process gets an INT or TERM -- which is then passed on to the handler there was
    before, as `bounded` does -- or exits, whichever thread started them. The handlers can only be set from the main
    thread, so a runner is made there (its composition point); one made elsewhere ends its groups on an exception or at
    exit only. A signal this process ignores (a background job's INT) stays ignored. A signal arriving while a command
    is being started waits for it (GRACE at most), else is held until it has (`started`)."""

    def __init__(self) -> None:
        self.lock = threading.RLock()  # (re-entered by the handler, which runs on the main thread)
        self.live: Set["subprocess.Popen[bytes]"] = set()
        self.starting = 0
        self.pending: Optional[int] = None
        self.before: Dict[int, Handler] = {}
        if threading.current_thread() is threading.main_thread():
            for sig in (signal.SIGINT, signal.SIGTERM):
                if signal.getsignal(sig) != signal.SIG_IGN:
                    self.before[sig] = signal.getsignal(sig)
                    signal.signal(sig, self._signalled)
        atexit.register(self.end_all)

    def end_all(self) -> None:
        with self.lock:
            procs = list(self.live)
        for proc in procs:
            if proc.poll() is None:
                end_group(proc)

    def _signalled(self, signum: int, frame: Optional[FrameType]) -> None:
        deadline = time.monotonic() + GRACE
        while self.starting and time.monotonic() < deadline:
            time.sleep(0.01)
        if self.starting:
            self.pending = signum  # the start under way is on this thread: acted on once it has (`started`)
            return
        self.end_all()
        self._pass_on(signum, frame)

    def _pass_on(self, signum: int, frame: Optional[FrameType]) -> None:
        before = self.before.get(signum)
        if callable(before):
            before(signum, frame)
            return
        signal.signal(signum, before or signal.SIG_DFL)
        os.kill(os.getpid(), signum)

    def guard(self, timeout: Optional[float]) -> "Bounded":
        return Bounded(self if timeout is not None else None)


class Bounded:
    """One command's hold on its runner's LiveGroups: counted as starting until `started`, then live until the wait
    ends; an exception out of the wait ends its group. An unbounded command (None: in this process's own group, ended
    with it already) is held by nothing."""

    def __init__(self, groups: Optional[LiveGroups]) -> None:
        self.groups = groups
        self.proc: "Optional[subprocess.Popen[bytes]]" = None

    def __enter__(self) -> "Bounded":
        if self.groups is not None:
            with self.groups.lock:
                self.groups.starting += 1
        return self

    def started(self, proc: "subprocess.Popen[bytes]") -> "subprocess.Popen[bytes]":
        self.proc = proc
        groups = self.groups
        if groups is not None:
            with groups.lock:
                groups.live.add(proc)
                groups.starting -= 1
            pending, groups.pending = groups.pending, None
            if pending is not None and not groups.starting:
                groups.end_all()
                groups._pass_on(pending, None)  # pyright: ignore[reportPrivateUsage]
        return proc

    def __exit__(self, kind: Optional[type], _value: Optional[BaseException], _trace: Optional[TracebackType]) -> None:
        groups, proc = self.groups, self.proc
        if groups is None:
            return
        if kind is not None and proc is not None and proc.poll() is None:
            end_group(proc)
        with groups.lock:
            if proc is None:
                groups.starting -= 1
            else:
                groups.live.discard(proc)


def end_group(proc: "subprocess.Popen[bytes]") -> None:
    """The command's whole process group ended: TERM, GRACE seconds for EVERY member to go, then KILL whatever is left
    of it. The grace is the group's, not its leader's: a member still ending what it started (`bounded`, its command
    in a group of its own) is KILLed only past it, never the moment the leader is gone (Tool self-tests #546)."""
    for sig in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(proc.pid, sig)
        except OSError:
            return
        deadline = time.monotonic() + GRACE
        # poll() reaps the leader, so its zombie keeps no group alive
        while time.monotonic() < deadline and (proc.poll() is None or group_alive(proc.pid)):
            time.sleep(0.05)
        if sig == signal.SIGTERM and not group_alive(proc.pid):
            return


def group_alive(pgid: int) -> bool:
    try:
        os.killpg(pgid, 0)
    except OSError:
        return False
    return True
