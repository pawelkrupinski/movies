"""THE MACHINE, for the scripts/ci tools that run in a sparse checkout (scripts/ci alone, no scripts/lib, so no
`machine`, `tool_git` or `tool_process`): the one place they reach the environment, the clock and a process, so
every other tool receives them from its `main()` and this module is the single adapter row for scripts/ci
(test/resources/design-rules.tsv). The same shape as scripts/lib/machine.py, kept apart because that directory is
not there."""
from __future__ import annotations

import datetime
import os
import subprocess
import time
from typing import Any, MutableMapping, Sequence


def environment() -> MutableMapping[str, str]:
    return os.environ


def wall() -> float:
    return time.time()


def today() -> datetime.date:
    return datetime.date.today()


def run(argv: Sequence[str], **options: Any) -> "subprocess.CompletedProcess[Any]":
    """subprocess.run, with its keywords (capture_output, text, check, cwd, timeout, input...)."""
    return subprocess.run(list(argv), **options)
