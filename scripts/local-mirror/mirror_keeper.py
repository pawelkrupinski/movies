"""THE PROD TUNNEL'S ONE OWNER (mirror.py --keep-tunnel), a child process of the daemon: the only thing that opens,
restarts or closes the forward -- the supervisors only ask whether it answers. A probe that misses while the ssh is
still alive is not a dead tunnel (restarting it drops every database's stream at once): KINOWO_MIRROR_TUNNEL_MISSES
misses in a row replace it, a dead ssh none. While prod cannot be reached at all the retry interval doubles from
KINOWO_MIRROR_TUNNEL_POLL up to KINOWO_MIRROR_TUNNEL_MAX_BACKOFF. It runs until TERMed or its parent is gone, and
closes the forward it holds either way."""
from __future__ import annotations

import signal
from types import FrameType
from typing import Dict, NoReturn, Optional

from mirror_settings import Pauses, Settings, say, seconds


class Ended(Exception):
    pass


def end(_signum: int, _frame: Optional[FrameType]) -> NoReturn:
    raise Ended()


class Keeper:
    def __init__(self, settings: Settings) -> None:
        self.settings = settings
        self.pause = Pauses()
        self.tunnel = settings.tunnel(self.pause)
        self.poll = float(settings.knob("TUNNEL_POLL", "30"))
        self.misses_allowed = int(settings.knob("TUNNEL_MISSES", "3"))
        self.max_backoff = float(settings.knob("TUNNEL_MAX_BACKOFF", "300"))
        self.misses = 0
        self.failures = 0
        self.wait = self.poll

    def run(self) -> int:
        signal.signal(signal.SIGTERM, end)
        signal.signal(signal.SIGINT, end)
        try:
            while not self.settings.parent_gone():
                self.step()
                self.pause(seconds(self.wait))
            say("tunnel keeper: parent is gone — closing the tunnel and exiting")
        except Ended:
            pass
        finally:
            self.pause.stop()
            self.tunnel.close()
        return 0

    def step(self) -> None:
        if self.tunnel.answers():
            if self.failures:
                say(f"tunnel keeper: prod answers again after {self.failures} failed attempt(s)")
            self.misses, self.failures, self.wait = 0, 0, self.poll
        elif self.tunnel.alive() and self.misses + 1 < self.misses_allowed:
            self.misses += 1
            say(f"tunnel keeper: probe missed ({self.misses} of {self.misses_allowed}) — keeping the open tunnel")
        else:
            self.misses = 0
            self.reopen()

    def reopen(self) -> None:
        if self.tunnel.ensure():
            if self.failures:
                say(f"tunnel keeper: tunnel back after {self.failures} failed attempt(s)")
            self.failures, self.wait = 0, self.poll
        else:
            self.failures += 1
            self.wait = min(self.wait * 2, self.max_backoff)
            say(f"tunnel keeper: attempt {self.failures} failed — next try in {seconds(self.wait)}s")


def keep_tunnel(env: Dict[str, str]) -> int:
    return Keeper(Settings.for_child(env)).run()
