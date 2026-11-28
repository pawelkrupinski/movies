"""WHAT EVERY PROCESS OF THE MIRROR DAEMON SHARES (mirror.py): its endpoints and knobs, read once from the environment
by the parent and handed to its children in it; the paths of the scripts it runs; how it says things, pauses, asks
whether its parent is gone, runs mongosh at the source and runs a no-server check under node.

Knobs, from the environment (mirror.py's header says what each is for):
  KINOWO_MIRROR_TUNNEL_POLL (30), _TUNNEL_MISSES (3), _TUNNEL_MAX_BACKOFF (300)   the tunnel keeper
  KINOWO_MIRROR_TARGETS_POLL (60), _AUDIT_POLLS (10), _LIVENESS_POLL (5)          the digest poll, audit, liveness
  KINOWO_MIRROR_STALENESS_TIMEOUT (120), KINOWO_MIRROR_FENCE_DIR                  the staleness gate, the fences
A pause is a `sleep` child, as the shell's `sleep N & wait $!` was: a TERM ends the wait at once, and the self-test's
own `sleep` first on the PATH shortens every one."""
from __future__ import annotations

import os
import subprocess
import sys
from dataclasses import dataclass
from types import FrameType
from typing import Dict, List, Mapping, Optional, Set

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import mongo_local  # noqa: E402
import prod_tunnel  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
TAG = "mirror"


def here(name: str) -> str:
    """A file the daemon runs, beside it (what the code-change watch digests: mirror_upkeep.runtime_files)."""
    return os.path.join(HERE, name)


def say(text: str) -> None:
    print(f"[{TAG}] {text}", flush=True)


def warn(text: str) -> None:
    print(f"[{TAG}] {text}", file=sys.stderr, flush=True)


def seconds(value: float) -> str:
    return f"{value:g}"


class Stopped(SystemExit):
    """A TERM (or INT) taken, raised at the process's next pause or wait (Pauses.check), never by the signal handler: an
    exception raised from a handler lands wherever the process is, and inside subprocess's timed child-wait (Python
    3.12) it left the Popen's lock held, so the wait the cleanup then made never returned (ci-runner, 2026-11-27: the
    keeper hung on it, its ssh holding the tunnel and the daemon's output open). A SystemExit, so a process it ends exits
    with its status; a thread it would end catches it (mirror.Parent.quietly), as threading prints any subclass's
    traceback."""

    def __init__(self, status: int) -> None:
        super().__init__(status)
        self.status = status


class Pauses:
    """Every pause is a `sleep` child, and every child a process waits on is waited through `wait`: a TERM (`on_signal`)
    ends those still running at once and the process stops at its next pause (Stopped), with the code given."""

    def __init__(self) -> None:
        self.live: Set["subprocess.Popen[bytes]"] = set()
        self.stopped: Optional[int] = None

    def check(self) -> None:
        if self.stopped is not None:
            raise Stopped(self.stopped)

    def wait(self, child: "subprocess.Popen[bytes]", timeout: Optional[float] = None) -> int:
        """`child`'s exit status; a stop ends it (TERM) meanwhile. TimeoutExpired as Popen.wait raises it."""
        self.live.add(child)
        try:
            if self.stopped is not None:
                child.terminate()
            return child.wait(timeout)
        finally:
            self.live.discard(child)

    def __call__(self, how_long: str) -> None:
        self.check()
        child = subprocess.Popen(["sleep", how_long], stdin=subprocess.DEVNULL)
        try:
            self.wait(child)
        finally:
            if child.poll() is None:
                child.kill()
                child.wait()
        self.check()

    def stop(self, code: int = 0) -> None:
        """Safe inside a signal handler: it takes no lock and raises nothing (the set is copied in one step)."""
        if self.stopped is None:
            self.stopped = code
        for child in list(self.live):
            child.terminate()

    def on_signal(self, signum: int, _frame: Optional[FrameType]) -> None:
        self.stop(128 + signum)


def tuned(source: str) -> str:
    """The source with zlib wire compression (~6x less over the tunnel) and a 10 s selection timeout (a brief tunnel
    slowdown does not fail the watch), each appended only when the URI does not already set it."""
    join = "&" if "?" in source else "?"
    if "compressors=" not in source:
        source, join = f"{source}{join}compressors=zlib", "&"
    if "serverSelectionTimeoutMS=" not in source:
        source = f"{source}{join}serverSelectionTimeoutMS=10000"
    return source


@dataclass(frozen=True)
class Settings:
    env: Dict[str, str]
    source: str  # SRC: prod through the tunnel, the tunnel probe's URI too
    target: str  # DST: the local mirror
    parent: Optional[int]
    targets_file: str

    @staticmethod
    def for_parent(env: Mapping[str, str], source_uri: str) -> "Settings":
        port = prod_tunnel.free_port(env, os.getpid()) or "27017"
        return Settings(dict(env), prod_tunnel.tunnel_uri(source_uri, port),
                        env.get("MONGODB_MOVIES_MIRROR_URI") or mongo_local.local_mongo_uri(env, "kinowo_prod_mirror"),
                        os.getpid(), "")

    @staticmethod
    def for_child(env: Mapping[str, str]) -> "Settings":
        return Settings(dict(env), env["KINOWO_MIRROR_SOURCE"], env["KINOWO_MIRROR_TARGET"],
                        int(env["KINOWO_MIRROR_PARENT"]), env.get("KINOWO_MIRROR_TARGETS_FILE", ""))

    def child_env(self) -> Dict[str, str]:
        return {**self.env, "KINOWO_MIRROR_SOURCE": self.source, "KINOWO_MIRROR_TARGET": self.target,
                "KINOWO_MIRROR_PARENT": str(self.parent), "KINOWO_MIRROR_TARGETS_FILE": self.targets_file}

    def knob(self, name: str, default: str) -> str:
        return self.env.get(f"KINOWO_MIRROR_{name}") or default

    @property
    def tuned_source(self) -> str:
        return tuned(self.source)

    @property
    def fence_dir(self) -> str:
        return self.knob("FENCE_DIR", os.path.join(self.env.get("HOME", ""), ".cache/kinowo/mirror-fence"))

    def parent_gone(self) -> bool:
        if self.parent is None or self.parent == os.getpid():
            return False
        try:
            os.kill(self.parent, 0)
        except OSError:
            return True
        return False

    def mongosh(self, *args: str) -> List[str]:
        """mongosh at the tuned source, its URI kept off mongosh's command line (mongosh-at.sh)."""
        return [here("mongosh-at.sh"), self.tuned_source, *args]

    def db_vars(self, database: str) -> str:
        return f"var DST='{self.target}'; var SRC_DB='{database}'"

    def tunnel(self, pause: Pauses) -> prod_tunnel.ProdTunnel:
        return prod_tunnel.ProdTunnel(TAG, self.source, self.env, [here("mongosh-at.sh")], pause)

    def nodb(self, *args: str) -> str:
        """A check that needs no server, under plain node (nodb.js), never mongosh: each mongosh start checks in with
        macOS's window server as a GUI app. Its stdout, or nothing when it could not run ("don't know")."""
        try:
            done = subprocess.run(["node", here("nodb.js"), *args], stdin=subprocess.DEVNULL, capture_output=True,
                                  text=True, env=self.env)
        except OSError:
            return ""
        return done.stdout

    def local_mongo(self) -> bool:
        """The local mirror Mongo answers, its idempotent starter re-run only when it does not (local-mongo.sh)."""
        port = mongo_local.local_mongo_port(self.env)
        if mongo_local.answers(port):
            return True
        say(f"local Mongo not reachable on :{port} — (re)starting")
        return subprocess.run([here("start-local-mongo.sh")], stdin=subprocess.DEVNULL, env=self.env).returncode == 0
