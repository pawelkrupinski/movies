"""THE COMMAND LINE CONVENTIONS the Python tools share (scripts/lib; tool-conventions.sh's `die` and usage exits): a
usage error exits 64 (EX_USAGE) with its line on stderr, and a tool's `main` is one call of `run_main`."""
from __future__ import annotations

import sys
from typing import Callable

EX_USAGE = 64


class UsageError(Exception):
    """A command line the tool does not take: its message is the usage line (or why), and the exit is 64."""


def run_main(tool: Callable[[], int]) -> int:
    try:
        return tool()
    except UsageError as e:
        print(e, file=sys.stderr)
        return EX_USAGE
