"""The alert backtest's clock (infra/bin/backtest-alerts): the one module that reads the time or waits, handed to whoever
needs it, so a retry loop or a readiness wait is never a direct use of the clock."""

from __future__ import annotations

import time


class Clock(object):
    """The wall clock."""

    def now(self) -> float:
        return time.time()

    def pause(self, seconds: float) -> None:
        time.sleep(seconds)


SYSTEM = Clock()
