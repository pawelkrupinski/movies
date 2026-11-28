"""The clock: the one place the applier reads the time or waits on it.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import time


def now() -> float:
    """Seconds since the epoch."""
    return time.time()


def sleep(seconds: float) -> None:
    """Wait `seconds`; the recorded-output tests make this return at once."""
    time.sleep(seconds)
