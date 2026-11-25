#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror* scripts/local-mirror/*.js scripts/local-mirror/prod-tunnel.sh scripts/local-mirror/local-mongo.sh scripts/local-mirror/log-path.sh scripts/local-mirror/mongosh-at.sh scripts/lib/prod_tunnel.py scripts/lib/mongo_local.py scripts/jvm-gate.sh scripts/vault-env.sh
"""scripts/local-mirror/mirror.sh (the local prod mirror's daemon: one prod tunnel keeper, one supervised tailer a
database), black-box: run by path with fake mongosh, ssh, sleep, ps, secrets and agentctl first on its PATH, a
listening socket of the case's own for the local mirror Mongo, and the prod tunnel's port pinned. It covers the vault
re-exec, how the source URI is tuned, which databases it mirrors, what the staleness gate's verdicts mean, a tailer
ending (its resume token expired, over and over), a failing seed, drift and a hung check while it tails, the what-if
fences, the collection-list digest and the no-server checks under node, the tunnel keeper as the tunnel's one owner,
the parent's death, a TERM and a dead supervisor, the orphan sweep, the log trim, every line dated, the code-change
watch and a seed's governor ticket. Ported from mirror-test.sh and mirror-resilience-test.sh; the same cases hold the
shell daemon and its Python rewrite (cli_harness.tool_path).

The fake `sleep` shortens every pause the daemon takes (under 1 s as asked, up to 100 s to 0.2 s, longer ones until
its caller is gone), and logs each; the fake mongosh answers each script from `answers/<kind>[-<db>]` (a line a call),
logs each call, and streams (blocks until TERMed) as a tailer by default."""
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
import time
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
# agentctl: a governor charging cores (`def cmd_gov def gov_route`, what jvm-gate.sh greps for); `gov admit` waits while
# $GOV_HOLD names a file not there yet (a starved laptop), then takes the ticket's lock on the descriptor it was given
GOVERNOR = r'''
# def cmd_gov def gov_route
import fcntl, select, time
if env.get("GOV_HOLD"):
    with open(env["GOV_HOLD"] + ".waiting", "w") as f:
        f.write("waiting\n")
    end = time.monotonic() + 120
    while not os.path.exists(env["GOV_HOLD"]) and time.monotonic() < end:
        select.select([], [], [], 0.05)
fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)
'''


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
        pids = [int(c["pid"]) for c in self.mongosh_calls()]
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


class SupervisionTest(MirrorCase):
    def test_the_staleness_gate_reseeds_on_3_alone_whatever_fences_the_machine_holds(self) -> None:
        self.fence("kinowo_a", "what-if-machine", str(int(time.time()) + 600), "run",
                   root="home/.cache/kinowo/mirror-fence")
        self.answer("staleness", "3", db="kinowo_a")
        self.answer("staleness", "0", db="kinowo_b")
        self.answer("staleness", "1", db="kinowo_c")
        self.start(KINOWO_MIRROR_DBS="kinowo_a kinowo_b kinowo_c")
        for db in ("kinowo_a", "kinowo_b", "kinowo_c"):
            self.until_tailing(db=db)
        self.assertEqual(["kinowo_a"], [c["db"] for c in self.mongosh_calls("seed")])
        port = self.local_mongo.getsockname()[1]
        self.assertIn(f"var DST='mongodb://127.0.0.1:{port}/kinowo_prod_mirror?directConnection=true'",
                      str(self.mongosh_calls("seed")[0]["evals"]))
        self.assertEqual(1, len(self.lines("kinowo_c: staleness check could not run (exit 1) — tailing anyway")))

    def test_a_tailer_whose_resume_token_expired_reseeds_at_once_without_a_backoff(self) -> None:
        self.answer("tail", "2")
        self.start()
        self.until_tailing(2)
        kinds = [c["kind"] for c in self.mongosh_calls() if c["kind"] in ("seed", "tail")]
        self.assertEqual(["tail", "seed", "tail"], kinds)
        self.assertEqual(1, len(self.lines("kinowo: resume token expired — re-seeding…")))
        self.assertNotIn("2", self.sleeps())

    def test_a_stream_ending_over_and_over_backs_off_and_says_so_for_the_first_three(self) -> None:
        self.answer("tail", *["1"] * 30)
        self.start(FAKE_SLEEP_HOLD="2 15:6")
        self.until("no six backoffs", lambda: len([s for s in self.sleeps() if s in ("2", "15")]) >= 6)
        self.assertEqual(["2", "2", "2", "2", "15", "15"], [s for s in self.sleeps() if s in ("2", "15")])
        self.assertEqual(3, len(self.lines("kinowo: stream ended (exit 1, ")), self.output())
        self.assertEqual(1, len(self.lines("kinowo: stream ended (exit 1, 1 in a row) — recovering in 2s…")))

    def test_a_failed_seed_retries_on_the_next_cycle_instead_of_ending_the_supervisor(self) -> None:
        self.answer("seed", "1", "0")
        self.start("--reseed")
        self.until_tailing()
        self.assertEqual(2, len(self.mongosh_calls("seed")))
        self.assertEqual(1, len(self.lines("kinowo: seed failed (exit 1) — retrying in 30s")))
        self.assertIn("30", self.sleeps())

    def test_a_mirror_that_drifts_while_tailing_is_reseeded_and_its_tailer_stopped(self) -> None:
        self.answer("staleness", "0", "3", "3")
        self.start(KINOWO_MIRROR_AUDIT_POLLS="1")
        first = self.until_tailing(2)[0]
        self.assertEqual(1, len(self.lines("kinowo: drifted while tailing — restarting to re-seed")))
        kinds = [c["kind"] for c in self.mongosh_calls() if c["kind"] in ("seed", "tail")]
        self.assertEqual(["tail", "seed", "tail"], kinds)
        self.until("the drifted tailer was left streaming", lambda: gone(first))

    def test_a_staleness_check_that_never_returns_is_cut_off_and_reads_as_tail_anyway(self) -> None:
        self.answer("staleness", "hang")
        self.start(KINOWO_MIRROR_STALENESS_TIMEOUT="1")
        self.until_tailing()
        self.assertEqual(1, len(self.lines("kinowo: staleness check could not run (exit 124) — tailing anyway")))
        self.until("the hung check was left running", lambda: gone(int(str(self.mongosh_calls("staleness")[0]["pid"]))))
        self.assertEqual([], self.mongosh_calls("seed"))

    @unittest.skipUnless(NODE, "no node")
    def test_a_what_if_fence_in_force_holds_the_reseed_audit_of_its_database_alone(self) -> None:
        now = int(time.time())
        self.fence("kinowo_pl", "what-if-1", str(now + 600), "run")
        self.fence("kinowo_de", "what-if-2", str(now - 5), "run")
        self.fence("kinowo_uk", "what-if-3", str(now + 600), "linger")
        self.fence("kinowo_uk", "what-if-4", str(now + 600), "drain")
        self.fence("kinowo_fr", "what-if-5", "not-a-number", "run")
        self.fence("kinowo_es", "what-if-6", str(now + 600), "linger")
        dbs = ["kinowo_pl", "kinowo_de", "kinowo_uk", "kinowo_fr", "kinowo_es", "kinowo_it"]
        self.start(KINOWO_MIRROR_DBS=" ".join(dbs))
        for db in dbs:
            self.until_tailing(db=db)
        self.assertEqual(1, len(self.lines("kinowo_pl: held by the what-if fence what-if-1 — no re-seed audit")))
        self.assertEqual(1, len(self.lines("kinowo_es: held by the what-if fence what-if-6 — no re-seed audit")))
        self.assertEqual(2, len(self.lines("held by the what-if fence")), self.output())
        self.assertEqual({"kinowo_de", "kinowo_uk", "kinowo_fr", "kinowo_it"},
                         {str(c["db"]) for c in self.mongosh_calls("staleness")})
        self.assertEqual([], self.mongosh_calls("fence-check") + self.mongosh_calls("targets-fingerprint"))
        self.assertTrue(any(c[-1].endswith("fence-check.js") for c in self.fakes.calls("node")))

    @unittest.skipUnless(NODE, "no node")
    def test_steady_state_publishes_the_digest_audits_through_mongosh_and_dates_every_line(self) -> None:
        self.start(KINOWO_MIRROR_AUDIT_POLLS="1")
        self.until("no three audits", lambda: len(self.mongosh_calls("staleness")) >= 3)
        [published] = glob.glob(self.path("tmp", "kinowo-mirror-targets.*"))
        with open(published) as f:
            digest = f.read().strip()
        with open(os.path.join(cli_harness.ROOT, "common/src/main/resources/debug-mirror-collections.json")) as f:
            names = {c["name"] for c in json.load(f)["collections"]}
        self.assertRegex(digest, rf"^{len(names)}-[0-9a-f]{{8}}$")
        expected = subprocess.run([str(NODE), os.path.join(HERE, "nodb.js"), "--file", os.path.join(HERE, "mirror-targets.js"),
                                   "--file", os.path.join(HERE, "targets-fingerprint.js")],
                                  capture_output=True, text=True, check=True).stdout.strip()
        self.assertEqual(expected, digest)
        self.assertEqual({"ping", "staleness", "tail"}, {str(c["kind"]) for c in self.mongosh_calls()})
        undated = [line for line in self.output().splitlines() if not STAMPED.match(line)]
        self.assertEqual([], undated)


class TunnelTest(MirrorCase):
    def test_supervisors_wait_for_the_keeper_never_open_the_tunnel_and_log_the_outage_once(self) -> None:
        dbs = ["kinowo_a", "kinowo_b", "kinowo_c"]
        self.start(KINOWO_MIRROR_DBS=" ".join(dbs), KINOWO_MIRROR_TUNNEL_MISSES="1000")
        tailers = self.until_tailing(3)
        self.write("ping-fails", "")
        for pid in tailers:
            os.kill(pid, signal.SIGTERM)
        for db in dbs:
            self.until_said(f"{db}: prod tunnel not answering — waiting for the tunnel keeper")
        self.until("the supervisors did not keep waiting", lambda: self.sleeps().count("5") >= 6)
        os.remove(self.path("ping-fails"))
        for db in dbs:
            self.until_said(f"{db}: prod tunnel answers again")
        for db in dbs:
            self.assertEqual(1, len(self.lines(f"{db}: prod tunnel not answering")), self.output())
            self.assertEqual(1, len(self.lines(f"{db}: prod tunnel answers again")), self.output())
        self.assertEqual(1, len(self.ssh_pids()))

    def test_the_keeper_keeps_a_live_tunnel_through_missed_probes_then_replaces_it(self) -> None:
        self.start(KINOWO_MIRROR_TUNNEL_MISSES="3")
        self.until_tailing()
        first = self.ssh_pids()[0]
        self.write("ping-fails", "")
        self.until_said("tunnel to prod Mongo is down — restarting it")
        os.remove(self.path("ping-fails"))
        said = self.output()
        missed = [said.find("probe missed (1 of 3) — keeping the open tunnel"),
                  said.find("probe missed (2 of 3) — keeping the open tunnel"),
                  said.find("tunnel to prod Mongo is down — restarting it")]
        self.assertTrue(-1 < missed[0] < missed[1] < missed[2], said)
        self.until("the replaced ssh kept running", lambda: gone(first))
        self.until("no second tunnel", lambda: len(self.ssh_pids()) == 2)

    def test_a_parent_killed_outright_leaves_no_tailer_and_no_tunnel(self) -> None:
        self.start()
        [tailer] = self.until_tailing()
        [ssh] = self.ssh_pids()
        assert self.daemon is not None
        os.kill(self.daemon.pid, signal.SIGKILL)
        self.daemon.wait()  # reaped, as its own parent (launchd, service.sh) does: a zombie still answers kill -0
        self.until("the tailer outlived its parent", lambda: gone(tailer))
        self.until("the tunnel outlived its parent", lambda: gone(ssh))
        self.until_said("kinowo: parent mirror.sh is gone — stopping this database's tailer")
        self.until_said("tunnel keeper: parent is gone — closing the tunnel and exiting")

    def test_a_term_ends_the_daemon_and_stops_its_tailers_and_tunnel(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo kinowo_uk")
        tailers = self.until_tailing(2)
        assert self.daemon is not None
        os.kill(self.daemon.pid, signal.SIGTERM)
        self.assertEqual(143, self.ended())
        for pid in tailers + self.ssh_pids():
            self.until(f"pid {pid} outlived the TERMed daemon", lambda: gone(pid))

    def test_a_supervisor_that_dies_takes_the_whole_daemon_down(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo kinowo_uk")
        [tailer] = self.until_tailing(db="kinowo_uk")
        supervisor = int(subprocess.run(["ps", "-o", "ppid=", "-p", str(tailer)], capture_output=True, text=True,
                                        check=True).stdout.strip())
        os.kill(supervisor, signal.SIGKILL)
        self.until_said(f"database supervisor {supervisor} is gone — exiting so launchd restarts the sync")
        self.assertEqual(143, self.ended())


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


@unittest.skipUnless(NODE, "no node")
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
