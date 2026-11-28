#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror* scripts/local-mirror/*.js scripts/local-mirror/prod-tunnel.sh scripts/local-mirror/local-mongo.sh scripts/local-mirror/log-path.sh scripts/local-mirror/mongosh-at.sh scripts/lib/prod_tunnel.py scripts/lib/mongo_local.py scripts/jvm-gate.sh scripts/vault-env.sh
"""scripts/local-mirror/mirror.sh (the local prod mirror's daemon: one prod tunnel keeper, one supervised tailer a
database), black-box, in three files that run side by side: this one (the vault re-exec, how the source URI is tuned, which
databases it mirrors, the staleness gate, the collection-list digest and the code-change watch), mirror_supervision_test.py
(a tailer ending, failing, drifting or hanging) and mirror_tunnel_test.py (the tunnel keeper, a TERM in a wait). The fixture
all three share -- MirrorCase and the fakes -- is below. Run by path with fake mongosh, ssh, sleep, ps, secrets and agentctl
first on its PATH, a listening socket of the case's own for the local mirror Mongo, and the prod tunnel's port pinned.

The fake `sleep` shortens every pause the daemon takes (under 1 s as asked, up to 100 s to 0.2 s, longer ones until
its caller is gone), and logs each; the fake mongosh answers each script from `answers/<kind>[-<db>]` (a line a call),
logs each call, and streams (blocks until TERMed) as a tailer by default. Ported from mirror-test.sh and
mirror-resilience-test.sh; the same cases hold the shell daemon and its Python rewrite (cli_harness.tool_path).
Run: python3 scripts/local-mirror/mirror_test.py"""
from __future__ import annotations

import glob
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
import unittest
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


def claimed():
    # Whether this process takes the armed TERM: the file names its role and this process's removal of it succeeded.
    # Every mirror.py process's probe waits read the file at once, and this runs holding the wait's lock: an OSError
    # raised here (the file removed by the process it named since this one looked) left the lock held, the Popen's own
    # cleanup wait blocked on it forever, and the hung keeper's ssh outlived the TERMed daemon (~1 stress run in 1,100).
    try:
        with open(ARMED) as f:
            if f.read().strip() != role():
                return False
        os.remove(ARMED)
    except OSError:
        return False
    return True


class Window:
    def __init__(self, child):
        self.lock, self.child = threading.Lock(), child

    def acquire(self, blocking=True, timeout=-1):
        got = self.lock.acquire(blocking, timeout)
        if got and "mongosh-at.sh" in str(self.child.args) and claimed():
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
        # whole lines only: a fake mongosh may be mid-append, its line not yet ended
        complete = self.read("mongosh.log").split("\n")[:-1]
        calls = [json.loads(line) for line in complete if line.strip()]
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


class FixtureTest(MirrorCase):
    def test_a_call_line_still_being_appended_is_not_read_yet(self) -> None:
        self.write("mongosh.log", '{"kind": "tail", "db": "kinowo", "uri": "", "pid": 1}\n{"kind": "tail", "db": "kin')
        self.assertEqual([1], [c["pid"] for c in self.mongosh_calls("tail")])


class StartupTest(MirrorCase):
    def test_a_missing_mongodb_uri_reexecs_the_daemon_under_the_vault_and_nothing_else(self) -> None:
        done = self.run_tool("--reseed", env=self.daemon_env({"MONGODB_URI": None}))
        self.assertEqual(0, done.code, done.output)
        [call] = self.fakes.calls("secrets")
        self.assertEqual(["run", "movies", "MONGODB_URI", "--"], call[:4], call)
        self.assertEqual("--reseed", call[-1])
        self.assertEqual(os.path.dirname(TOOL), os.path.dirname(call[-2]))
        self.assertIn(os.path.basename(call[-2]), ("mirror.sh", "mirror.py"))
        self.assertEqual(([], []), (self.mongosh_calls(), self.ssh_pids()))

    @unittest.skipIf(shutil.which("secrets", path="/usr/bin:/bin"), "a secrets CLI in /usr/bin or /bin")
    def test_no_mongodb_uri_and_no_vault_cli_is_refused(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "secrets"))
        env = self.env(self.daemon_env({"MONGODB_URI": None}))
        env["PATH"] = self.fakes.bin + ":/usr/bin:/bin"
        done = cli_harness.run_bounded([TOOL], env=env, cwd=self.work, stdin=None)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("MONGODB_URI not set, and no `secrets` CLI on PATH", done.output)

    def test_no_database_through_the_tunnel_ends_the_run_and_leaves_nothing_behind(self) -> None:
        done = self.run_tool(env=self.daemon_env({"KINOWO_MIRROR_DBS": "", "FAKE_LISTING": ""}))
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no kinowo* databases found via the tunnel — is MONGODB_URI right?", done.output)
        self.assertEqual([], os.listdir(self.path("tmp")))
        for pid in self.ssh_pids():
            self.until(f"the keeper's ssh outlived the run: {described(pid)}", lambda: gone(pid))
        port = self.tunnel_port
        [listing] = self.mongosh_calls("list")
        self.assertEqual(f"mongodb://u:p@127.0.0.1:{port}/kinowo?authSource=admin&directConnection=true"
                         "&compressors=zlib&serverSelectionTimeoutMS=10000", listing["uri"])
        self.assertEqual(f"mongodb://u:p@127.0.0.1:{port}/kinowo?authSource=admin&directConnection=true"
                         "&serverSelectionTimeoutMS=3000", self.mongosh_calls("ping")[0]["uri"])
        [ssh] = self.fakes.calls("ssh")
        self.assertEqual(["-N", "-L", f"{port}:127.0.0.1:27017", "-o", "ExitOnForwardFailure=yes", "-o", "BatchMode=yes",
                          "-o", "ConnectTimeout=10", "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=3",
                          "-o", "ControlMaster=no", "-o", "ControlPath=none", TARGET], ssh)

    def test_a_tuned_uri_is_not_tuned_twice_and_the_ssh_target_is_the_environments(self) -> None:
        done = self.run_tool(env=self.daemon_env({
            "KINOWO_MIRROR_DBS": "", "FAKE_LISTING": "", "KINOWO_MONGO_SSH": "root@override",
            "MONGODB_URI": "mongodb://h/kinowo?compressors=snappy&serverSelectionTimeoutMS=500"}))
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(f"mongodb://127.0.0.1:{self.tunnel_port}/kinowo?compressors=snappy&serverSelectionTimeoutMS=500"
                         "&directConnection=true", self.mongosh_calls("list")[0]["uri"])
        self.assertEqual("root@override", self.fakes.calls("ssh")[0][-1])

    def test_a_port_another_process_holds_with_a_dead_backend_is_reported_never_bound_over(self) -> None:
        holder = listening()
        self.held.append(holder)
        self.tunnel_port = int(holder.getsockname()[1])
        self.write("ping-fails", "")
        self.start()
        self.until_said(f":{self.tunnel_port} is held by another process but prod does NOT answer through it.")
        self.until_said("waiting for local Mongo + tunnel")
        os.kill(self.daemon.pid if self.daemon else 0, signal.SIGTERM)
        self.assertEqual(143, self.ended())
        self.assertEqual([], self.ssh_pids())

    def test_an_ssh_that_exits_at_once_is_reported_with_its_code_and_retried_later(self) -> None:
        self.start(FAKE_SSH_EXIT="255")
        self.until_said(f"ssh to {TARGET} exited (code 255) before prod answered — 255 is a connection or login failure")
        self.until_said("tunnel keeper: attempt 1 failed — next try in 2s")

class DatabasesTest(MirrorCase):
    def test_configured_databases_are_mirrored_comma_separated_without_a_listing(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo,kinowo_uk")
        self.until_said("2 tailer(s) running")
        self.until_tailing(2)
        self.assertEqual(1, len(self.lines("[mirror] mirroring: kinowo kinowo_uk")), self.output())
        self.assertEqual([], self.mongosh_calls("list"))
        self.assertEqual({"kinowo", "kinowo_uk"}, {str(c["db"]) for c in self.mongosh_calls("tail")})

    def test_unconfigured_the_tunnels_kinowo_databases_are_discovered_from_the_listings_last_line(self) -> None:
        self.start(KINOWO_MIRROR_DBS="", FAKE_LISTING="Current Mongosh Log ID: x\nkinowo kinowo_uk kinowo_de")
        self.until_said("[mirror] mirroring: kinowo kinowo_uk kinowo_de")
        self.until_said("3 tailer(s) running")

class UpkeepTest(MirrorCase):
    def spawn(self) -> int:
        child = subprocess.Popen(["/bin/sleep", "3600"], text=True)
        self.spawned.append(child)
        return child.pid

    def test_the_sweep_stops_orphaned_mirror_trees_and_prod_forwards_and_nothing_live_or_foreign(self) -> None:
        p = {name: self.spawn() for name in ("service", "live", "live_sub", "live_tailer", "live_ssh", "orphan",
                                              "orphan_tailer", "orphan_child", "tunnel", "foreign", "unranged")}
        tun = ("-o ExitOnForwardFailure=yes -o BatchMode=yes -o ConnectTimeout=10 -o ServerAliveInterval=15 "
               "-o ServerAliveCountMax=3 root@178.105.221.61")
        mirror = "/bin/bash /Users/pawel/projects/movies/scripts/local-mirror/mirror.sh"
        self.write("ps-table", f"""\
  {p['service']}     1 /bin/bash /Users/pawel/projects/movies/scripts/local-mirror/service.sh agent
 {p['live']}  {p['service']} {mirror}
 {p['live_sub']} {p['live']} {mirror}
 {p['live_tailer']} {p['live_sub']} mongosh
 {p['live_ssh']} {p['live']} ssh -N -L 27454:127.0.0.1:27017 {tun}
 {p['orphan']}     1 {mirror}
 {p['orphan_tailer']} {p['orphan']} mongosh
 {p['orphan_child']} {p['orphan_tailer']} sleep 30
 {p['tunnel']}     1 ssh -N -L 27607:127.0.0.1:27017 {tun}
 {p['foreign']}     1 ssh -N -L 8080:127.0.0.1:80 somebody@example.org
 {p['unranged']}     1 ssh -N -L 27017:127.0.0.1:27017 {tun}
""")
        self.start()
        self.until_said("sweep: stopping pid", 4)
        swept = {"orphan", "orphan_tailer", "orphan_child", "tunnel"}
        for name in swept:
            self.until(f"{name} was not stopped", lambda: self.spawned[list(p).index(name)].poll() is not None)
        self.assertEqual(set(), {name for name in p if name not in swept and self.spawned[list(p).index(name)].poll()
                                 is not None})
        self.assertEqual(4, len(self.lines("sweep: stopping pid")), self.output())
        self.assertEqual(1, len(self.lines(f"sweep: stopping pid {p['tunnel']} (orphaned prod tunnel): ssh -N -L 27607")))
        self.assertEqual(1, len(self.lines(f"sweep: stopping pid {p['orphan']} (orphaned mirror.sh): {mirror}")))

    def test_a_log_past_its_cap_is_trimmed_in_place_bsd_stat_or_not(self) -> None:
        self.fakes.script("stat", 'case "$1" in -f*) echo "stat: cannot read file system information" >&2; exit 1 ;; '
                                  'esac\nexec /usr/bin/stat "$@"')
        log = self.write("home/Library/Logs/kinowo-local-mirror.log",
                         "".join(f"line {n:07d}\n" for n in range(1, 700001)))
        inode = os.stat(log).st_ino
        self.start()
        self.until_said("log passed 8MB — trimmed to its last 2000 lines")
        with open(log) as f:
            kept = f.read().splitlines()
        self.assertEqual((2000, "line 0698001", "line 0700000"), (len(kept), kept[0], kept[-1]))
        self.assertEqual(inode, os.stat(log).st_ino)

    def test_a_seed_waits_for_its_governor_ticket_then_runs_holding_it(self) -> None:
        governor = self.fakes.program("agentctl", GOVERNOR)
        self.start("--reseed", KINOWO_MIRROR_DBS="kinowo_pl", KINOWO_GOVERNOR="1", KINOWO_GOVERNOR_BIN=governor,
                   KINOWO_JVM_GATE_OS="Darwin", AGENTCTL_GOV_DIR=self.path("gov"), GOV_HOLD=self.path("admit"))
        self.until("the seed never asked the governor", lambda: os.path.exists(self.path("admit.waiting")))
        self.assertEqual([], self.mongosh_calls("seed"))
        self.write("admit", "")
        self.until_tailing()
        [seed] = self.mongosh_calls("seed")
        self.assertEqual((True, "kinowo_pl"), (seed["ticket"], seed["db"]))
        admits = [" ".join(c) for c in self.fakes.calls("agentctl") if c[:2] == ["gov", "admit"]]
        self.assertEqual(1, len([a for a in admits if re.search(
            r"--mem 1 --label mirror seed kinowo_pl .*--job mirror-seed --cores 1$", a)]), admits)

class CodeWatchTest(MirrorCase):
    """Against a copy of the daemon's tree, so a case can edit the files it runs."""

    def setUp(self) -> None:
        super().setUp()
        tree = self.path("tree")
        for relative in ("scripts/local-mirror", "scripts/lib"):
            shutil.copytree(os.path.join(cli_harness.ROOT, relative), os.path.join(tree, relative),
                            ignore=shutil.ignore_patterns("__pycache__"))
        for library in glob.glob(os.path.join(cli_harness.ROOT, "scripts", "*.sh")):
            shutil.copy2(library, os.path.join(tree, "scripts"))
        listing = "common/src/main/resources/debug-mirror-collections.json"
        os.makedirs(os.path.dirname(os.path.join(tree, listing)))
        shutil.copy2(os.path.join(cli_harness.ROOT, listing), os.path.join(tree, listing))
        self.tree_tool = os.path.join(tree, os.path.relpath(TOOL, cli_harness.ROOT))
        self.tree_here = os.path.join(tree, "scripts", "local-mirror")

    def digests(self) -> int:
        return len([c for c in self.fakes.calls("node") if c[-1].endswith("targets-fingerprint.js")])

    def append(self, name: str) -> None:
        with open(os.path.join(self.tree_here, name), "a") as f:
            f.write("// an edit\n")

    def test_an_edit_to_a_spec_restarts_nothing_and_one_to_code_it_runs_restarts_the_daemon(self) -> None:
        self.start(tool=self.tree_tool)
        self.until_tailing()
        self.until_said("watching mirror-targets.js for collection changes")
        self.append("staleness-rule-spec.js")
        polled = self.digests()
        self.until("no two polls after the spec edit", lambda: self.digests() >= polled + 2)
        assert self.daemon is not None
        self.assertIsNone(self.daemon.poll(), self.output())
        self.append("tail.js")
        self.until_said("sync scripts changed on disk — exiting so the supervisor restarts on them")
        self.assertEqual(143, self.ended())

    def test_an_unreadable_collection_list_publishes_nothing_never_a_digest(self) -> None:
        with open(os.path.join(self.path("tree"), "common/src/main/resources/debug-mirror-collections.json"), "w") as f:
            f.write('{"collections": [')
        self.start(tool=self.tree_tool)
        self.until_tailing()
        [published] = glob.glob(self.path("tmp", "kinowo-mirror-targets.*"))
        with open(published) as f:
            self.assertEqual("", f.read().strip())


if __name__ == "__main__":
    unittest.main()
