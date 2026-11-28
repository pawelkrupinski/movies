"""The clock: the one place the dispatcher reads the time or waits on it.

A part of nix/files/scheduled_dispatch.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import datetime
import time


def epoch() -> float:
    """Seconds since the epoch."""
    return time.time()


def now_utc() -> datetime.datetime:
    """The current moment, UTC."""
    return datetime.datetime.now(datetime.timezone.utc)


def sleep(seconds: float) -> None:
    """Wait `seconds`; the decision tests hand the dispatcher a clock of their own instead."""
    time.sleep(seconds)
