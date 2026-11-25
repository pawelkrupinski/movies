"""THE COMMAND LINE CONVENTIONS the Python tools share (scripts/lib; tool-conventions.sh's `die` and usage exits): a
usage error exits 64 (EX_USAGE) with its line on stderr, and a tool's `main` is one call of `run_main`. A tool ported
from the shell keeps the shell's usage exit, 2 (tool-conventions.sh's usage_error): `usage_failed`. A tool that
cleans up on the way out (a shell tool's EXIT trap) raises `Interrupted` on INT and TERM (`raise_on_signals`) and ends
with 128 + the signal, as a shell does.

  read_options(o, argv, usage, word)  `o` with each word read into it by `word(o, argv, i)` (the index of the last
                                 word it took); None when -h/--help asked for the usage, printed
  value_at(argv, i)              the value of the flag at argv[i], a UsageError when it has none
  minutes(flag, value)           a whole number of minutes, a UsageError otherwise
  Refused(line, with_usage)      a command line refused as a shell tool refused it (`echo line >&2; usage >&2; exit 2`):
                                 `refused(r, usage)` prints it and gives the exit, 2
  say_as(tool)                   the tool's `say(line)`: `tool: line` on stderr, flushed"""
from __future__ import annotations

import signal
import sys
from types import FrameType
from typing import Callable, List, Optional, TypeVar

EX_USAGE = 64
T = TypeVar("T")


def say_as(tool: str) -> Callable[[str], None]:
    """`say(line)` for `tool`: `<tool>: <line>` on stderr, flushed at once (a progress line, never the tool's output)."""
    def say(line: str) -> None:
        print(f"{tool}: {line}", file=sys.stderr, flush=True)
    return say


class UsageError(Exception):
    """A command line the tool does not take: its message is the usage line (or why), and the exit is 64."""


def run_main(tool: Callable[[], int]) -> int:
    try:
        return tool()
    except UsageError as e:
        print(e, file=sys.stderr)
        return EX_USAGE


def usage_failed(tool: str, error: UsageError) -> int:
    print(f"{tool}: {error}\n{tool}: see {tool} --help", file=sys.stderr)
    return 2


def read_options(o: T, argv: List[str], usage: str, word: Callable[[T, List[str], int], int]) -> Optional[T]:
    i = 0
    while i < len(argv):
        if argv[i] in ("-h", "--help"):
            print(usage, end="")
            return None
        i = word(o, argv, i) + 1
    return o


def value_at(argv: List[str], i: int) -> str:
    if i + 1 >= len(argv):
        raise UsageError(f"{argv[i]} needs a value")
    return argv[i + 1]


def minutes(flag: str, value: str) -> int:
    if not value.isdigit():
        raise UsageError(f"{flag} takes minutes, not {value}")
    return int(value)


class Refused(Exception):
    """A command line refused: its line (none: "") on stderr, then the usage when `with_usage`; the exit 2."""

    def __init__(self, line: str, with_usage: bool = False) -> None:
        super().__init__(line)
        self.line, self.with_usage = line, with_usage


def refused(refusal: Refused, usage: str) -> int:
    sys.stderr.write((refusal.line + "\n" if refusal.line else "") + (usage if refusal.with_usage else ""))
    return 2


class Interrupted(BaseException):
    """INT or TERM: the clean-up run on the way out, as a shell tool's EXIT trap did; the exit 128 + `signum`."""

    def __init__(self, signum: int) -> None:
        super().__init__(signum)
        self.signum = signum


def interrupted(signum: int, _frame: Optional[FrameType]) -> None:
    raise Interrupted(signum)


def raise_on_signals() -> None:
    """INT and TERM raise Interrupted from here on."""
    for sig in (signal.SIGINT, signal.SIGTERM):
        signal.signal(sig, interrupted)
