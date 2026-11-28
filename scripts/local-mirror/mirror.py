#!/usr/bin/env python3
"""Keep a local mirror of prod's /debug data in sync, continuously, so the dev `/debug` pages read from a fast LAN Mongo
instead of over the prod ssh tunnel -- where the corpus scan takes 30-60 s and every per-row read pays a ~110 ms
round-trip. Per mirrored database: ensure the local mirror Mongo is up (start-local-mongo.sh), ensure prod is reachable
through the tunnel, seed the mirrored collections when the mirror is empty, has drifted too far to catch up by resuming
or is missing one (or `--reseed` to force), tail prod's change stream into it, restarting on disconnect, and notice
mirror-targets.js changing under it (the tailers restart on the new list) and the daemon's own code changing (it exits,
for launchd or service.sh to restart it on the new code).

WHICH databases and collections are mirrored lives in mirror-targets.js. Each database gets its OWN supervisor process
(mirror_supervisor.py): a change stream is per database here, and mongosh is single-threaded. ONE tunnel keeper process
(mirror_keeper.py) owns the prod forward; the parent (this file) starts them, publishes the collection-list digest,
trims its log, sweeps up what an earlier run left behind (mirror_upkeep.py), and exits the moment a supervisor is gone,
so launchd restarts a clean process rather than a live pid with nothing tailing. Every child polls whether the parent
is gone and exits by itself (a parent SIGKILLed takes no tailer or ssh with it). Every line, ours and every child's, is
dated on its way out (log-path.sh's stamp_lines).

Reads, from the environment:
  MONGODB_URI                prod = source. A SECRET: when it is not set, the daemon re-runs itself under
                             `secrets run movies MONGODB_URI` (one Touch ID); the tunnel form is derived from it.
  MONGODB_MOVIES_MIRROR_URI  local = target (default mongodb://127.0.0.1:$LOCAL_MIRROR_PORT/kinowo_prod_mirror?...)
  KINOWO_MIRROR_DBS          optional, space/comma-separated prod databases (default: every kinowo* the tunnel lists)
  KINOWO_MIRROR_*            the knobs mirror_settings.py lists
Long-running: leave it in its own terminal (Ctrl-C stops it), or run it as a login agent (service.sh). The web app
reads the mirror only when ITS MONGODB_MOVIES_MIRROR_URI is set.

  scripts/local-mirror/mirror.sh [--reseed]
  (internal: mirror.py --supervise <db> <0|1>, mirror.py --keep-tunnel -- the daemon's own children)"""
from __future__ import annotations

import os
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
from typing import Dict, List

import mirror_keeper
import mirror_supervisor
import mirror_upkeep
from mirror_settings import Pauses, Settings, here, say, warn
from mongo import port_probe


def under_the_vault(argv: List[str]) -> int:
    """MONGODB_URI unset: start over under `secrets run` (exec), or 1 when there is no vault CLI to ask."""
    if not shutil.which("secrets"):
        print("[vault] MONGODB_URI not set, and no `secrets` CLI on PATH to read it from the vault", file=sys.stderr)
        return 1
    os.execvp("secrets", ["secrets", "run", "movies", "MONGODB_URI", "--", sys.executable, os.path.abspath(__file__),
                          *argv])


def stamp_output(env: Dict[str, str]) -> None:
    """Our stdout and stderr, and every child's, through log-path.sh's stamp_lines (a process of its own, so it
    outlives a parent killed outright and dates the children's last lines too)."""
    read, write = os.pipe()
    subprocess.Popen(["bash", "-c", '. "$1"; stamp_lines', "stamp", here("log-path.sh")], stdin=read, env=env)
    os.close(read)
    sys.stdout.flush()
    sys.stderr.flush()
    os.dup2(write, 1)
    os.dup2(write, 2)
    os.close(write)


class Parent:
    def __init__(self, settings: Settings, force: bool) -> None:
        self.settings = settings
        self.force = force
        self.pause = Pauses()
        self.tunnel = settings.tunnel(self.pause)
        self.children: List["subprocess.Popen[bytes]"] = []

    def spawn(self, *role: str) -> "subprocess.Popen[bytes]":
        child = subprocess.Popen([sys.executable, os.path.abspath(__file__), *role], stdin=subprocess.DEVNULL,
                                 env=self.settings.child_env())
        self.children.append(child)
        return child

    def stop(self) -> None:
        """TERM every child (the keeper closes the forward it holds on it), drop the digest file."""
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
        signal.signal(signal.SIGINT, signal.SIG_IGN)
        for child in self.children:
            if child.poll() is None:
                child.terminate()
        self.pause.stop()
        if self.settings.targets_file:
            try:
                os.remove(self.settings.targets_file)
            except OSError:
                pass

    def run(self) -> int:
        self.spawn("--keep-tunnel")
        waits = 0
        while not (self.settings.local_mongo() and self.tunnel.answers()):
            if waits % 12 == 0:
                say(f"waiting for local Mongo + tunnel (~{waits * 5}s so far)…")
            waits += 1
            self.pause("5")
        databases = self.databases()
        if not databases:
            warn("no kinowo* databases found via the tunnel — is MONGODB_URI right?")
            return 1
        say(f"mirroring: {' '.join(databases)}")
        return self.mirror(databases)

    def databases(self) -> List[str]:
        configured = self.settings.env.get("KINOWO_MIRROR_DBS", "").replace(",", " ").split()
        if configured:
            return configured
        listing = subprocess.run(self.settings.mongosh(
            "--quiet", "--eval", "db.adminCommand({listDatabases:1,nameOnly:true}).databases.map(d=>d.name)"
            '.filter(n=>/^kinowo/.test(n)).join(" ")'), stdin=subprocess.DEVNULL, capture_output=True, text=True)
        lines = listing.stdout.splitlines()
        return lines[-1].split() if lines else []

    def mirror(self, databases: List[str]) -> int:
        handle, path = tempfile.mkstemp(prefix="kinowo-mirror-targets.", dir=os.environ.get("TMPDIR") or "/tmp")
        os.close(handle)
        self.settings = Settings(self.settings.env, self.settings.source, self.settings.target, self.settings.parent,
                                 path, self.settings.probe)
        self.publish(mirror_upkeep.targets_fingerprint(self.settings))
        code = mirror_upkeep.code_fingerprint()
        poll = self.settings.knob("TARGETS_POLL", "60")
        say(f"watching mirror-targets.js for collection changes, and the sync scripts for edits (every {poll}s)")
        supervisors = [self.spawn("--supervise", db, "1" if self.force else "0") for db in databases]
        say(f"{len(supervisors)} tailer(s) running (Ctrl-C to stop)…")
        log = mirror_upkeep.log_path(self.settings.env)
        mirror_upkeep.rotate_log(log)
        mirror_upkeep.sweep_orphans(self.settings.env)
        threading.Thread(target=self.upkeep, args=(log,), daemon=True).start()
        threading.Thread(target=self.watch_code, args=(code, poll), daemon=True).start()
        return self.await_supervisor_exit(supervisors)

    def publish(self, digest: str) -> None:
        if digest:
            with open(self.settings.targets_file, "w") as f:
                f.write(digest + "\n")

    def upkeep(self, log: str) -> None:
        while True:
            self.pause("300")
            mirror_upkeep.rotate_log(log)
            mirror_upkeep.sweep_orphans(self.settings.env)

    def watch_code(self, code: str, poll: str) -> None:
        """Re-reads the collection list for the tailers; new CODE needs the supervisors themselves replaced."""
        while True:
            self.pause(poll)
            self.publish(mirror_upkeep.targets_fingerprint(self.settings))
            now = mirror_upkeep.code_fingerprint()
            if code and now and now != code:
                say("sync scripts changed on disk — exiting so the supervisor restarts on them")
                os.kill(os.getpid(), signal.SIGTERM)
                return

    def await_supervisor_exit(self, supervisors: List["subprocess.Popen[bytes]"]) -> int:
        while True:
            for child in supervisors:
                if child.poll() is not None:
                    say(f"database supervisor {child.pid} is gone — exiting so launchd restarts the sync")
                    return 143
            self.pause(self.settings.knob("LIVENESS_POLL", "5"))


def daemon(argv: List[str], env: Dict[str, str]) -> int:
    if not env.get("MONGODB_URI"):
        return under_the_vault(argv)
    stamp_output(env)
    parent = Parent(Settings.for_parent(env, env["MONGODB_URI"], port_probe.SocketProbe()), argv[:1] == ["--reseed"])
    signal.signal(signal.SIGTERM, parent.pause.on_signal)  # it stops at its next pause, mirror_settings.Stopped
    signal.signal(signal.SIGINT, parent.pause.on_signal)
    try:
        return parent.run()
    finally:
        parent.stop()


def main(argv: List[str]) -> int:
    env = dict(os.environ)
    if argv[:1] == ["--supervise"] and len(argv) == 3:
        return mirror_supervisor.supervise(env, argv[1], argv[2] == "1")
    if argv == ["--keep-tunnel"]:
        return mirror_keeper.keep_tunnel(env)
    return daemon(argv, env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
