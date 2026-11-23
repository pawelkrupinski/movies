"""ONE DATABASE'S SUPERVISOR (mirror.py --supervise <db> <force>), a child process of the daemon: every cycle it
re-ensures the local mirror Mongo, waits for the tunnel (the keeper's to fix, never its own), re-seeds when the mirror
is empty or has drifted too far (the staleness gate) or a re-seed is owed, then tails prod's change stream -- auditing
again every KINOWO_MIRROR_AUDIT_POLLS digest polls while it streams, and restarting the tailer when the published
collection-list digest moves. Nothing here is fatal: a down tunnel, a stopped Mongo, a failed seed or a wiped mirror
recovers on the next cycle. It ends only when TERMed (its tailer with it) or when its parent is gone, and says how it
ended either way. The incidents behind each step: mirror.py's header and mirror.sh's git history."""
from __future__ import annotations

import os
import signal
import subprocess
from types import FrameType
from typing import NoReturn, Optional

import mirror_settings
from mirror_settings import Pauses, Settings, here, say, warn
from prod_tunnel import shell_status

RESEED, TAIL = True, False


class Stop(Exception):
    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def stop_on(signum: int, _frame: Optional[FrameType]) -> NoReturn:
    raise Stop(128 + signum)


def fence_holder(settings: Settings, database: str) -> str:
    """The what-if fence in force on `database` (fence-rule.js), or nothing."""
    return settings.nodb("--eval", f"var FENCE_DIR='{settings.fence_dir}'; var SRC_DB='{database}'",
                         "--file", here("fence-rule.js"), "--file", here("fence-check.js")).strip()


def published_digest(settings: Settings) -> str:
    try:
        with open(settings.targets_file) as f:
            return f.read().strip()
    except OSError:
        return ""


def bounded(argv: "list[str]", limit: float) -> int:
    """argv's exit status, or 124 once it outlives `limit` seconds (it is TERMed, then waited for)."""
    child = subprocess.Popen(argv, stdin=subprocess.DEVNULL)
    try:
        return shell_status(child.wait(timeout=limit))
    except subprocess.TimeoutExpired:
        child.terminate()
        child.wait()
        return 124
    finally:
        if child.poll() is None:
            child.kill()
            child.wait()


class Supervisor:
    def __init__(self, settings: Settings, database: str, force: bool) -> None:
        self.settings = settings
        self.db = database
        self.force = force
        self.pause = Pauses()
        self.tunnel = settings.tunnel(self.pause)
        self.tailer: Optional["subprocess.Popen[bytes]"] = None
        self.fails = 0
        self.tunnel_waits = 0

    def run(self) -> int:
        signal.signal(signal.SIGTERM, stop_on)
        signal.signal(signal.SIGINT, stop_on)
        code = 1
        try:
            while True:
                self.cycle()
        except Stop as stopped:
            code = stopped.code
        except Exception as failed:  # a supervisor never ends on purpose: say which, how and on what
            warn(f"{self.db}: supervisor failing on `{failed!r}` (exit 1)")
        finally:
            self.stop_tailer()
            self.pause.stop()
            warn(f"{self.db}: supervisor ended (exit {code})")
        return code

    def stop_if_orphaned(self) -> None:
        if self.settings.parent_gone():
            say(f"{self.db}: parent mirror.sh is gone — stopping this database's tailer")
            raise Stop(0)

    def stop_tailer(self) -> None:
        if self.tailer is not None and self.tailer.poll() is None:
            self.tailer.terminate()

    def ready(self) -> bool:
        if not self.settings.local_mongo():
            say(f"{self.db}: local Mongo unavailable — retrying in 5s")
            self.pause("5")
            return False
        if not self.tunnel.answers():
            if self.tunnel_waits == 0:
                say(f"{self.db}: prod tunnel not answering — waiting for the tunnel keeper")
            self.tunnel_waits += 1
            self.pause("5")
            return False
        if self.tunnel_waits:
            say(f"{self.db}: prod tunnel answers again (waited ~{self.tunnel_waits * 5}s)")
        self.tunnel_waits = 0
        return True

    def cycle(self) -> None:
        self.stop_if_orphaned()
        if not self.ready():
            return
        if self.force or self.needs_reseed():
            if not self.reseed_or_wait():
                return  # a forced re-seed still owes one next cycle
            self.force = False
        launched_with = published_digest(self.settings)
        self.tailer = subprocess.Popen(self.settings.mongosh(
            "--quiet", "--eval", f"{self.settings.db_vars(self.db)}; var FENCE_DIR='{self.settings.fence_dir}'",
            "--file", here("mirror-targets.js"), "--file", here("stream-start.js"), "--file", here("fence-rule.js"),
            "--file", here("tail.js")), stdin=subprocess.DEVNULL)
        restarted = self.watch(self.tailer, launched_with)
        code = shell_status(self.tailer.wait())
        self.tailer = None
        if restarted:
            self.fails = 0  # a tailer WE stopped is no failure
        elif code == 2:
            say(f"{self.db}: resume token expired — re-seeding…")
            if self.reseed_or_wait():
                self.fails = 0
        else:
            self.back_off(code)

    def watch(self, tailer: "subprocess.Popen[bytes]", launched_with: str) -> bool:
        """While the tailer streams: True once it was stopped for a new collection list or for drift."""
        polls, audit_every = 0, int(self.settings.knob("AUDIT_POLLS", "10"))
        while tailer.poll() is None:
            self.pause(self.settings.knob("TARGETS_POLL", "60"))
            self.stop_if_orphaned()
            now = published_digest(self.settings)
            if now and launched_with and now != launched_with:
                say(f"{self.db}: mirrored-collection list changed — restarting tailer to pick it up")
                tailer.terminate()
                return True
            polls += 1
            if polls % audit_every == 0 and self.needs_reseed():
                say(f"{self.db}: drifted while tailing — restarting to re-seed")
                tailer.terminate()
                return True
        return False

    def back_off(self, code: int) -> None:
        self.fails += 1
        backoff = 2 if self.fails < 5 else 15 if self.fails < 20 else 60
        if self.fails <= 3 or self.fails % 20 == 0:
            say(f"{self.db}: stream ended (exit {code}, {self.fails} in a row) — recovering in {backoff}s…")
        self.pause(str(backoff))

    def needs_reseed(self) -> bool:
        holder = fence_holder(self.settings, self.db)
        if holder:
            warn(f"{self.db}: held by the what-if fence {holder} — no re-seed audit meanwhile")
            return TAIL
        code = bounded(self.settings.mongosh("--quiet", "--eval", self.settings.db_vars(self.db),
                                             "--file", here("mirror-targets.js"), "--file", here("staleness-rule.js"),
                                             "--file", here("staleness.js")),
                       float(self.settings.knob("STALENESS_TIMEOUT", "120")))
        if code == 3:
            return RESEED
        if code != 0:
            warn(f"{self.db}: staleness check could not run (exit {code}) — tailing anyway")
        return TAIL

    def reseed_or_wait(self) -> bool:
        """A seed under a governor ticket (jvm-gate.sh gov_run: 1 core, 1 GB); a failure is a cycle to retry."""
        code = subprocess.run(["bash", "-c", '. "$1" || exit 2; shift; gov_run "$@"', "gov_run",
                               os.path.join(os.path.dirname(mirror_settings.HERE), "jvm-gate.sh"),
                               f"mirror seed {self.db}", "mirror-seed", "1", "1",
                               *self.settings.mongosh("--quiet", "--eval", self.settings.db_vars(self.db),
                                                      "--file", here("mirror-targets.js"), "--file", here("seed.js"))],
                              stdin=subprocess.DEVNULL, env=self.settings.env).returncode
        if code == 0:
            return True
        say(f"{self.db}: seed failed (exit {shell_status(code)}) — retrying in 30s")
        self.pause("30")
        return False


def supervise(env: "dict[str, str]", database: str, force: bool) -> int:
    return Supervisor(Settings.for_child(env), database, force).run()
