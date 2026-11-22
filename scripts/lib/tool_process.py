"""RUNNING ANOTHER COMMAND, the one way the Python tools do it (scripts/lib; the shell tools' `bounded` and `"$@" 2>&1 |
tee log`): a `Runner` is injected into every tool at its composition point, so a test hands in a fake that records the
commands, and the real one is `SubprocessRunner`. A `Command` says how it runs:

  Command(argv, env)                       inheriting this process's stdin, stdout and stderr
  Command(argv, env, log=PATH)             stdout and stderr into the file at PATH (appended with append=True)
  Command(argv, env, log=PATH, tee=True)   ... and to this process's stdout as they come (`2>&1 | tee PATH`)
  closed_stdin=True                        stdin from /dev/null (`< /dev/null`)

The exit status is the command's own; a command that cannot be found is 127 and one that cannot be executed 126, as a
shell says."""
from __future__ import annotations

import os
import subprocess
import sys
from dataclasses import dataclass
from typing import BinaryIO, Mapping, Optional, Protocol, Sequence

NOT_FOUND, NOT_EXECUTABLE = 127, 126


@dataclass(frozen=True)
class Command:
    argv: Sequence[str]
    env: Mapping[str, str]
    log: Optional[str] = None
    append: bool = False
    tee: bool = False
    closed_stdin: bool = False


class Runner(Protocol):
    def run(self, command: Command) -> int: ...


class SubprocessRunner:
    """The real runner: each command a child process of this one."""

    def __init__(self, stdout: Optional[BinaryIO] = None) -> None:
        self.stdout = stdout or sys.stdout.buffer

    def run(self, command: Command) -> int:
        sys.stdout.flush()
        try:
            if command.log is None:
                return subprocess.run(list(command.argv), env=dict(command.env), stdin=self.stdin(command)).returncode
            with open(command.log, "ab" if command.append else "wb") as sink:
                if command.tee:
                    return self._tee(command, sink)
                return subprocess.run(list(command.argv), env=dict(command.env), stdin=self.stdin(command), stdout=sink,
                                      stderr=subprocess.STDOUT).returncode
        except FileNotFoundError:
            return self._failed(command.argv, "command not found", NOT_FOUND)
        except PermissionError:
            return self._failed(command.argv, "permission denied", NOT_EXECUTABLE)

    @staticmethod
    def stdin(command: Command) -> Optional[int]:
        return subprocess.DEVNULL if command.closed_stdin else None

    def _tee(self, command: Command, sink: BinaryIO) -> int:
        proc = subprocess.Popen(list(command.argv), env=dict(command.env), stdin=self.stdin(command),
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
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
