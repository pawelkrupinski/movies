"""The fixture scripts/local-mirror/mirror_test.py, mirror_supervision_test.py and mirror_tunnel_test.py share: the fake
mongosh, ssh, sleep, ps, secrets and agentctl first on the daemon's PATH, the case's listening sockets and the helpers that
run and read the daemon (MirrorCase). Split from one 750-line file so the three halves run side by side."""
from __future__ import annotations

import json
import os
import re
import shlex
import shutil
import signal
import socket
import subprocess
import sys
import threading
from typing import Callable, Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/local-mirror/mirror.py", "scripts/local-mirror/mirror.sh")
HERE = os.path.join(cli_harness.ROOT, "scripts", "local-mirror")
NODE = shutil.which("node")
SOURCE = "mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin"
TARGET = "root@178.105.221.61"
STAMPED = re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}[+-]\d{4} ")

MONGOSH = r'''
import re, signal
work = env["FAKE_WORK"]
flags = list(zip(argv, argv[1:]))
files = [os.path.basename(v) for k, v in flags if k == "--file"]
evals = " ".join(v for k, v in flags if k == "--eval")
found = re.search(r"SRC_DB='([^']*)'", evals)
db = found.group(1) if found else ""
scripts = ("seed.js", "staleness.js", "tail.js", "fence-check.js", "targets-fingerprint.js")
kind = next((f[:-3] for f in files if f in scripts), "")
if not kind:
    kind = "ping" if "ping:1" in evals else "list" if "listDatabases" in evals else "other"
try:
    os.fstat(7)
    ticket = True
except OSError:
    ticket = False
with open(os.path.join(work, "mongosh.log"), "a") as log:
    log.write(json.dumps({"kind": kind, "db": db, "uri": env.get("KINOWO_MONGOSH_URI", ""), "pid": os.getpid(),
                          "evals": evals, "ticket": ticket and bool(env.get("KINOWO_JVM_SLOT_HELD"))}) + "\n")
def answer(default):
    for name in (kind + "-" + db, kind):
        path = os.path.join(work, "answers", name)
        if os.path.exists(path):
            with open(path) as f:
                lines = f.read().splitlines()
            if lines:
                with open(path, "w") as f:
                    f.write("".join(line + "\n" for line in lines[1:]))
                return lines[0]
    return default
def hang():
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(143))
    while True:
        signal.pause()
if kind == "ping":
    exit(1 if os.path.exists(os.path.join(work, "ping-fails")) else 0)
if kind == "list":
    out(env.get("FAKE_LISTING", ""))
    exit(0)
said = answer("stream" if kind == "tail" else "0")
if said in ("stream", "hang"):
    hang()
exit(int(said))
'''
# ssh: the forward's local end listens (or fails as ExitOnForwardFailure would), and takes each probe's connection until
# TERMed (a backlog of probes never accepted would fill, and the port stop answering)
SSH = r'''
import signal, socket
work = env["FAKE_WORK"]
with open(os.path.join(work, "ssh.pids"), "a") as f:
    f.write(f"{os.getpid()}\n")
if env.get("FAKE_SSH_EXIT"):
    exit(int(env["FAKE_SSH_EXIT"]))
port = int(argv[argv.index("-L") + 1].split(":")[0])
server = socket.socket()
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
try:
    server.bind(("127.0.0.1", port))
    server.listen(64)
except OSError:
    exit(255)
signal.signal(signal.SIGTERM, lambda *_: sys.exit(143))
while True:
    server.accept()[0].close()
'''
SLEEP = r'''
import select, time
work = env["FAKE_WORK"]
parent = os.getppid()
text = argv[0] if argv else "0"
with open(os.path.join(work, "sleeps.log"), "a") as f:
    f.write(text + "\n")
held = False
if env.get("FAKE_SLEEP_HOLD"):
    values, after = env["FAKE_SLEEP_HOLD"].split(":")
    with open(os.path.join(work, "sleeps.log")) as f:
        held = sum(1 for line in f if line.strip() in values.split()) >= int(after)
seconds = float(text)
span = 86400.0 if held or seconds >= 100 else seconds if seconds < 1 else 0.2
end = time.monotonic() + span
while time.monotonic() < end and os.getppid() == parent:
    select.select([], [], [], 0.02)
'''
PS = r'''
table = os.path.join(env["FAKE_WORK"], "ps-table")
if os.path.exists(table):
    with open(table) as f:
        out(f.read(), end="")
'''
# agentctl: a governor charging cores (it answers the `gov probe` jvm-gate.sh's governor_bin reads); `gov admit` waits while
# $GOV_HOLD names a file not there yet (a starved laptop), then takes the ticket's lock on the descriptor it was given
GOVERNOR = r'''
import fcntl, select, time
if argv[:2] == ["gov", "probe"]:
    print("agentctl governor: admit cores route width")
    exit(0)
if env.get("GOV_HOLD"):
    with open(env["GOV_HOLD"] + ".waiting", "w") as f:
        f.write("waiting\n")
    end = time.monotonic() + 120
    while not os.path.exists(env["GOV_HOLD"]) and time.monotonic() < end:
        select.select([], [], [], 0.05)
fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)
'''

# A TERM landing just after a child-wait took subprocess's private lock, before the code that releases it: Python 3.12's
# timed wait meets it when the signal arrives between its acquire(False) and its try (ci-runner's python3; 3.15 waits on a
# kqueue or pidfd first, and no longer does). On the PYTHONPATH as sitecustomize: in the mirror.py process whose role
# $FAKE_TERM_IN_WAIT names (keeper, supervisor, daemon), the first wait on a probe (mongosh-at.sh) after the file exists
# TERMs the process itself, once.
TERM_IN_WAIT = r"""
import os, signal, subprocess, sys, threading
ARMED = os.environ.get("FAKE_TERM_IN_WAIT", "")


def role():
    if not sys.argv or not sys.argv[0].endswith("mirror.py"):
        return ""
    return {"--keep-tunnel": "keeper", "--supervise": "supervisor"}.get((sys.argv[1:] or [""])[0], "daemon")


class Window:
    def __init__(self, child):
        self.lock, self.child = threading.Lock(), child

    def acquire(self, blocking=True, timeout=-1):
        got = self.lock.acquire(blocking, timeout)
        if got and "mongosh-at.sh" in str(self.child.args) and os.path.exists(ARMED):
            with open(ARMED) as f:
                wanted = f.read().strip()
            if wanted == role():
                os.remove(ARMED)
                os.kill(os.getpid(), signal.SIGTERM)
        return got

    def release(self):
        self.lock.release()

    def locked(self):
        return self.lock.locked()

    __enter__ = acquire

    def __exit__(self, *_):
        self.release()


if ARMED:
    started = subprocess.Popen.__init__

    def windowed(self, *args, **kwargs):
        started(self, *args, **kwargs)
        self._waitpid_lock = Window(self)
    subprocess.Popen.__init__ = windowed
"""


def listening() -> socket.socket:
    """A port that answers: every connection taken and closed (an unaccepted backlog fills, and the port goes quiet)."""
    server = socket.socket()
    server.bind(("127.0.0.1", 0))
    server.listen(64)

    def take() -> None:
        while True:
            try:
                server.accept()[0].close()
            except OSError:
                return
    threading.Thread(target=take, daemon=True).start()
    return server


def reserved_port() -> socket.socket:
    """A bound, never listening socket that keeps its port ours; the fake ssh shares it (SO_REUSEPORT) to listen."""
    reservation = socket.socket()
    reservation.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
    reservation.bind(("127.0.0.1", 0))
    return reservation


def alive(pid: int) -> bool:
    """Whether `pid` runs: answers kill -0 and is no zombie (ended, its status not yet collected)."""
    if not cli_harness.alive(pid):
        return False
    state = subprocess.run(["ps", "-o", "stat=", "-p", str(pid)], capture_output=True, text=True).stdout.strip()
    return bool(state) and not state.startswith("Z")


def gone(pid: int) -> bool:
    return not alive(pid)


def described(pid: int) -> str:
    return subprocess.run(["ps", "-o", "pid=,ppid=,stat=,args=", "-p", str(pid)], capture_output=True, text=True).stdout


class MirrorCase(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        for name in ("home", "tmp", "fences", "answers"):
            os.makedirs(self.path(name))
        self.local_mongo = listening()
        reservation = reserved_port()
        self.held: List[socket.socket] = [self.local_mongo, reservation]
        self.tunnel_port = int(reservation.getsockname()[1])
        self.daemon: Optional[subprocess.Popen[str]] = None
        self.spawned: List[subprocess.Popen[str]] = []
        self.fakes.program("mongosh", MONGOSH)
        self.fakes.program("ssh", SSH)
        self.fakes.program("sleep", SLEEP)
        self.fakes.program("ps", PS)
        self.fakes.script("secrets", "exit 0")
        if NODE:
            self.fakes.script("node", f'exec {shlex.quote(NODE)} "$@"')

    def tearDown(self) -> None:
        if self.daemon is not None:
            try:
                os.killpg(self.daemon.pid, signal.SIGKILL)
            except OSError:
                pass
            self.daemon.wait()
            self.daemon_out.close()
        for pid in self.recorded_pids():
            try:
                os.kill(pid, signal.SIGKILL)
            except OSError:
                pass
        for child in self.spawned:
            child.kill()
            child.wait()
        for server in self.held:
            try:
                server.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            server.close()
        super().tearDown()

    def recorded_pids(self) -> List[int]:
        pids = [int(str(c["pid"])) for c in self.mongosh_calls()]
        if os.path.exists(self.path("ssh.pids")):
            pids += [int(line) for line in self.read("ssh.pids").split()]
        return pids

    def daemon_env(self, extra: Optional[Dict[str, Optional[str]]] = None) -> Dict[str, Optional[str]]:
        env: Dict[str, Optional[str]] = {
            "FAKE_WORK": self.work, "HOME": self.path("home"), "TMPDIR": self.path("tmp"), "MONGODB_URI": SOURCE,
            "LOCAL_MIRROR_PORT": str(self.local_mongo.getsockname()[1]), "PROD_TUNNEL_PORT": str(self.tunnel_port),
            "KINOWO_MIRROR_DBS": "kinowo", "KINOWO_MIRROR_FENCE_DIR": self.path("fences"),
            "KINOWO_MIRROR_TARGETS_POLL": "0.2", "KINOWO_MIRROR_TUNNEL_POLL": "1", "KINOWO_MIRROR_LIVENESS_POLL": "0.2",
            "KINOWO_MIRROR_AUDIT_POLLS": "1000", "MONGODB_MOVIES_MIRROR_URI": None, "KINOWO_MONGO_SSH": None,
            "KINOWO_JVM_SLOT_HELD": None}
        env.update(extra or {})
        return env

    def start(self, *args: str, tool: Optional[str] = None, **extra: Optional[str]) -> subprocess.Popen[str]:
        """The daemon in the background, in a process group of its own (ended whole at the case's end)."""
        self.daemon_out = open(self.path("out.log"), "w")
        self.daemon = subprocess.Popen([tool or self.TOOL, *args], env=self.env(self.daemon_env(extra)), cwd=self.work,
                                       text=True, start_new_session=True, stdin=subprocess.DEVNULL,
                                       stdout=self.daemon_out, stderr=subprocess.STDOUT)
        return self.daemon

    def output(self) -> str:
        return self.read("out.log") if os.path.exists(self.path("out.log")) else ""

    def lines(self, text: str) -> List[str]:
        return [line for line in self.output().splitlines() if text in line]

    def until(self, what: str, condition: Callable[[], bool]) -> None:
        self.assertTrue(cli_harness.wait_until(condition), f"{what}; the daemon said:\n{self.output()}")

    def until_said(self, text: str, count: int = 1) -> None:
        self.until(f"never said {text!r} {count}x", lambda: len(self.lines(text)) >= count)

    def answer(self, kind: str, *lines: str, db: str = "") -> None:
        self.write(f"answers/{kind}-{db}" if db else f"answers/{kind}", "".join(line + "\n" for line in lines))

    def mongosh_calls(self, kind: Optional[str] = None, db: Optional[str] = None) -> List[Dict[str, object]]:
        if not os.path.exists(self.path("mongosh.log")):
            return []
        calls = [json.loads(line) for line in self.read("mongosh.log").splitlines() if line.strip()]
        return [c for c in calls if (kind is None or c["kind"] == kind) and (db is None or c["db"] == db)]

    def until_tailing(self, count: int = 1, db: Optional[str] = None) -> List[int]:
        self.until(f"no {count} tailer(s) started", lambda: len(self.mongosh_calls("tail", db)) >= count)
        return [int(str(c["pid"])) for c in self.mongosh_calls("tail", db)]

    def ssh_pids(self) -> List[int]:
        return [int(line) for line in self.read("ssh.pids").split()] if os.path.exists(self.path("ssh.pids")) else []

    def sleeps(self) -> List[str]:
        return self.read("sleeps.log").split() if os.path.exists(self.path("sleeps.log")) else []

    def fence(self, db: str, holder: str, expiry: str, kind: str, root: str = "fences") -> None:
        self.write(f"{root}/{db}/{holder}", f"{expiry} {kind}\n")

    def ended(self) -> int:
        assert self.daemon is not None
        self.until("the daemon never ended", lambda: self.daemon is not None and self.daemon.poll() is not None)
        return int(self.daemon.returncode)
