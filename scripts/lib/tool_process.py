"""RUNNING ANOTHER COMMAND, the one way the Python tools do it (scripts/lib; the shell tools' `bounded` and `"$@" 2>&1 |
tee log`): a `Runner` is injected into every tool at its composition point, so a test hands in a fake that records the
commands, and the real one is `SubprocessRunner`. A `Command` says how it runs:

  Command(argv, env)                       inheriting this process's stdin, stdout and stderr
  Command(argv, env, log=PATH)             stdout and stderr into the file at PATH (appended with append=True)
  Command(argv, env, log=PATH, tee=True)   ... and to this process's stdout as they come (`2>&1 | tee PATH`)
  closed_stdin=True                        stdin from /dev/null (`< /dev/null`)
  timeout=SECONDS                          ended past it, its whole process group -- TERM, a moment, then KILL --
                                           exit 124 (tool-conventions.sh's `bounded`)

`run` returns the exit status; `capture` returns it with stdout and stderr as text (`$(cmd)`), the command's stderr
captured unless `log` takes both.

The exit status is the command's own; a command that cannot be found is 127 and one that cannot be executed 126, as a
shell says."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import BinaryIO, Mapping, Optional, Protocol, Sequence, Union

NOT_FOUND, NOT_EXECUTABLE, TIMED_OUT = 127, 126, 124
GRACE = 2.0  # seconds a TERMed group has before it is KILLed


@dataclass(frozen=True)
class Command:
    argv: Sequence[str]
    env: Mapping[str, str]
    log: Optional[str] = None
    append: bool = False
    tee: bool = False
    closed_stdin: bool = False
    timeout: Optional[float] = None


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
            if command.log is None:
                return self._wait(self._start(command, None, None), command)
            with open(command.log, "ab" if command.append else "wb") as sink:
                if command.tee:
                    return self._tee(command, sink)
                return self._wait(self._start(command, sink, subprocess.STDOUT), command)
        except FileNotFoundError:
            return self._failed(command.argv, "command not found", NOT_FOUND)
        except PermissionError:
            return self._failed(command.argv, "permission denied", NOT_EXECUTABLE)

    def capture(self, command: Command) -> Captured:
        sys.stdout.flush()
        try:
            proc = self._start(command, subprocess.PIPE, subprocess.PIPE)
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
        return subprocess.Popen(list(command.argv), env=dict(command.env), stdin=self.stdin(command),
                                stdout=stdout, stderr=stderr, start_new_session=command.timeout is not None)

    @staticmethod
    def _wait(proc: "subprocess.Popen[bytes]", command: Command) -> int:
        try:
            return proc.wait(timeout=command.timeout)
        except subprocess.TimeoutExpired:
            end_group(proc)
            return TIMED_OUT

    @staticmethod
    def stdin(command: Command) -> Optional[int]:
        return subprocess.DEVNULL if command.closed_stdin else None

    def _tee(self, command: Command, sink: BinaryIO) -> int:
        proc = self._start(command, subprocess.PIPE, subprocess.STDOUT)
        source = proc.stdout
        assert source is not None
        chunk = os.read(source.fileno(), 65536)
        while chunk:
            self.stdout.write(chunk)
            self.stdout.flush()
            sink.write(chunk)
            chunk = os.read(source.fileno(), 65536)
        return proc.wait()

    @staticmethod
    def _failed(argv: Sequence[str], why: str, code: int) -> int:
        print(f"{os.path.basename(argv[0]) if argv else '?'}: {why}", file=sys.stderr)
        return code


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
