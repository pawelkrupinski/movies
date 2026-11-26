"""scripts/sbt-warm's KEEPER (`--keep`; its header: ONE JVM, ONE SLOT and IDLE EXIT): a small process beside a checkout's
kept server, holding the server's JVM slot on descriptor 7 and $key.keep.flock's lock for its whole life. It takes a
slot once one is free when it started without one, stops the server after KINOWO_SBT_WARM_IDLE_MINUTES without a call
(only holding $key.busy exclusively: never under a running command), logs a server that ended without a stop as
vanished, and ends once the checkout has had no server for KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS with no call running."""
from __future__ import annotations

import datetime
import math
import os
import signal
import sys
from dataclasses import dataclass
from types import FrameType
from typing import Callable, List, Mapping, Optional

from jvm_gate import FileLock, JvmGate
from sbt_warm_servers import Checkout, Servers, older_than, read


@dataclass
class KeeperSettings:
    poll: float
    grace: float
    idle_minutes: float
    reaper_logs: List[str]

    @classmethod
    def read(cls, env: Mapping[str, str], home: str) -> "KeeperSettings":
        reaper = env.get("KINOWO_JVM_REAPER_LOG")
        logs = [reaper] if reaper else [os.path.join(home, ".cache/agentctl/reap.log"),
                                        os.path.join(home, "Library/Logs/jvm-reaper.log")]
        return cls(float(env.get("KINOWO_SBT_WARM_KEEPER_POLL") or 10),
                   float(env.get("KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS") or 60),
                   float(env.get("KINOWO_SBT_WARM_IDLE_MINUTES") or 10), logs)


class Keeper:
    def __init__(self, checkout: Checkout, servers: Servers, gate: JvmGate, settings: KeeperSettings,
                 say: Callable[[str], None], sleep: Callable[[float], None],
                 now: Callable[[], datetime.datetime]) -> None:
        self.checkout, self.servers, self.gate, self.settings = checkout, servers, gate, settings
        self.say, self.sleep, self.now = say, sleep, now
        self.busy = FileLock(checkout.file(".busy"))
        self.pid: Optional[int] = None
        self.absent = 0.0

    def terminated(self, _signum: int, _frame: Optional[FrameType]) -> None:
        self.say(f"keeper pid {os.getpid()} of {self.checkout.root}: terminated (SIGTERM)")
        sys.exit(0)

    def run(self) -> int:
        signal.signal(signal.SIGTERM, self.terminated)
        held = self.gate.riding()
        slot = os.path.basename(self.gate.env.get("KINOWO_JVM_SLOT_HELD", ""))
        self.say(f"keeper of {self.checkout.root}: pid {os.getpid()}, "
                 + (f"holding JVM slot {slot}" if held else "no JVM slot yet"))
        while not self.done():
            self.sleep(self.settings.poll)
        return 0

    def done(self) -> bool:
        """One poll: whether the keeper's work is over."""
        root = self.checkout.root
        if not os.path.isdir(root):
            return self.checkout_removed()
        if self.gate.take_if_free():
            self.say(f"took JVM slot {os.path.basename(self.gate.slot)}")
        server = self.servers.pid(root)
        if self.pid is not None and server != self.pid and not self.servers.procs.alive(self.pid):
            self.servers.vanished(self.pid, root, self.settings.reaper_logs)
        if server is None:
            return self.absent_poll()
        self.pid, self.absent = server, 0
        if read(self.checkout.file(".pid")) != str(server):
            with open(self.checkout.file(".pid"), "w") as f:
                f.write(f"{server}\n")
        return self.idle() and self.stop_idle(server)

    def checkout_removed(self) -> bool:
        if self.pid is not None and self.servers.of_removed_checkout(self.pid):
            self.servers.stop(self.pid, self.checkout.root, "its checkout was removed (keeper)")
        return True

    def absent_poll(self) -> bool:
        self.absent += math.ceil(self.settings.poll)
        if self.absent >= self.settings.grace and self.busy.take(os.getpid()):
            self.say(f"no server of {self.checkout.root} for {int(self.absent)}s: done")
            return True
        return False

    def aged(self) -> bool:
        return older_than(self.checkout.file(".used"), self.settings.idle_minutes, self.now)

    def idle(self) -> bool:
        """Aged, then aged STILL once no call holds $key.busy (a call ending between the two refreshed the mark)."""
        if not (self.aged() and self.busy.take(os.getpid())):
            return False
        if self.aged():
            return True
        self.busy.give()
        return False

    def stop_idle(self, server: int) -> bool:
        root, minutes = self.checkout.root, f"{self.settings.idle_minutes:g}"
        self.servers.stop(server, root, f"idle for {minutes} minutes (keeper)")
        self.servers.await_none(root, 30)
        for pid in self.servers.pids(root):
            self.servers.stop(pid, root, "idle, alive 30 s after SIGTERM (keeper)", kill=True)
        return True
