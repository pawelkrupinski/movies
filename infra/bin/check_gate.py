"""The running gate of infra/bin/check: how a step's commands run (from infra/, nix's bin first on their PATH), what a
step prints, and whether anything has failed. Every command's output is read the way the shell tool read it -- `$(cmd
2>&1)`, trailing newlines dropped -- and every line printed is the shell tool's, byte for byte."""
from __future__ import annotations

import os
import sys
import tempfile
from typing import List, Mapping, Optional, Sequence, TextIO, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
from tool_process import Command, Runner, SubprocessRunner  # noqa: E402

__all__ = ["Gate", "SubprocessRunner", "nix_bin"]


def nix_bin(runner: Runner, env: Mapping[str, str]) -> Optional[str]:
    """nix's bin directory as bin/nix_path.py finds it, or None (its reason already on stderr)."""
    found = runner.capture(Command(["python3", "bin/nix_path.py"], env))
    if found.code != 0:
        sys.stderr.write(found.stderr)
        sys.stderr.flush()
        return None
    return found.stdout.rstrip("\n")


def lines(text: str) -> List[str]:
    """`echo "$out"`'s lines: an empty output is one empty line."""
    return text.split("\n")


def tail(text: str, count: int) -> List[str]:
    return lines(text)[-count:]


def indented(text_lines: Sequence[str]) -> List[str]:
    return ["  " + line for line in text_lines]


def ok_count(text: str, anchored: bool = True) -> int:
    """`grep -c '\\.\\.\\. ok$'` (or, not anchored, `grep -c '\\.\\.\\. ok'`) over the output."""
    return sum(1 for line in lines(text) if (line.endswith("... ok") if anchored else "... ok" in line))


class Gate:
    """Runs a step's commands and prints its lines; `failed` once any step has failed."""

    def __init__(self, runner: Runner, env: Mapping[str, str], cwd: str, out: TextIO = sys.stdout) -> None:
        self.runner, self.env, self.cwd, self.out = runner, dict(env), cwd, out
        self.failed = False

    def step(self, name: str) -> None:
        self.say(f"\n\033[1m==> {name}\033[0m")

    def say(self, *text_lines: str) -> None:
        for line in text_lines:
            self.out.write(line + "\n")
        self.out.flush()

    def fail(self, *text_lines: str) -> None:
        self.say(*text_lines)
        self.failed = True

    def run(self, argv: Sequence[str], cwd: str = "") -> int:
        """The command with this tool's stdout and stderr."""
        return self.runner.run(Command(argv, self.env, cwd=self.where(cwd)))

    def silently(self, argv: Sequence[str], cwd: str = "") -> int:
        """`cmd >/dev/null 2>&1`."""
        return self.runner.run(Command(argv, self.env, log=os.devnull, append=True, cwd=self.where(cwd)))

    def output(self, argv: Sequence[str], cwd: str = "") -> Tuple[int, str]:
        """`out="$(cmd 2>&1)"`, its trailing newlines kept (the caller joins outputs, then drops them)."""
        with tempfile.TemporaryDirectory(prefix="infra-check-") as scratch:
            log = os.path.join(scratch, "out")
            code = self.runner.run(Command(argv, self.env, log=log, cwd=self.where(cwd)))
            with open(log, "rb") as f:
                return code, f.read().decode(errors="replace")

    def stdout_of(self, argv: Sequence[str]) -> Tuple[int, str]:
        """`out="$(cmd 2>/dev/null)"`: stdout, trailing newlines dropped."""
        captured = self.runner.capture(Command(argv, self.env, cwd=self.cwd))
        return captured.code, captured.stdout.rstrip("\n")

    def where(self, cwd: str) -> str:
        return os.path.join(self.cwd, cwd) if cwd else self.cwd
