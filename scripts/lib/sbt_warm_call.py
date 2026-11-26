"""One call of scripts/sbt-warm: its commands run in this checkout's kept server (its header says every rule this
follows -- THE IT LAYER, THE GITOPS MANIFESTS, ONE STARTER, IN USE, ONE COMMAND AT A TIME, the KINOWO_* settings, ONE
JVM ONE SLOT, ONE CALL ITS CORES, NO PROGRESS). Its collaborators come from scripts/sbt-warm's main()."""
from __future__ import annotations

import fcntl
import glob
import hashlib
import locale
import os
import re
import signal
import subprocess
import sys
import threading
import time
from dataclasses import dataclass
from types import FrameType
from typing import Callable, List, MutableMapping, Optional

import stall_watch
from cpu_meter import CpuMeter
from jvm_gate import FileLock, JvmGate, lock_busy
from sbt_warm_cache import Builds
from sbt_warm_client import STALLED, Client, Ended, Watch, WatchSettings
from sbt_warm_servers import Checkout, Servers, read, remove

KEEP_FD = 5
IT_RUN = re.compile(r"(^|[;\s/])itAll([;\s]|$)|IntegrationTest/test")
TEST_RUN = re.compile(r"(^|[;\s/])test[A-Za-z]*([;\s]|$)")
TOOL_KNOBS = ("KINOWO_SBT_WARM_", "KINOWO_SBT_CACHE", "KINOWO_MEMORY_GATE", "KINOWO_JVM_", "KINOWO_STALL_",
              "KINOWO_PROGRESS_FILE=", "KINOWO_GOV")


def sbt_job(commands: List[str]) -> str:
    """The job a call is charged cores for: testUnit, test (a whole module's), testOnly, compile, else job."""
    words = " " + " ".join(commands).replace(";", " ") + " "
    if " testUnit" in words:
        return "testUnit"
    if re.search(r"[\s/]test\s", words):
        return "test"
    if "testOnly" in words or "testQuick" in words:
        return "testOnly"
    compiles = all(w.endswith(("compile", "fullClasspath")) or w in ("reload", "staleTastyDrop", "export", "clean")
                   for w in words.split())
    return "compile" if compiles else "job"


def digest(data: bytes, width: int = 16) -> str:
    return hashlib.sha1(data).hexdigest()[:width]


def settings_digest(env: MutableMapping[str, str]) -> str:
    """The KINOWO_* settings a server starts with (less the tool-side knobs), as `env | grep | sort | shasum` says."""
    lines = [line for line in "".join(f"{k}={v}\n" for k, v in env.items()).splitlines()
             if line.startswith("KINOWO_") and not line.startswith(TOOL_KNOBS)]
    done = subprocess.run(["sort"], input="".join(line + "\n" for line in lines), capture_output=True, text=True)
    return digest(done.stdout.encode())


def build_digest(root: str) -> str:
    """The build definition's digest: build.sbt, project/*.sbt, project/*.scala and project/build.properties."""
    locale.setlocale(locale.LC_COLLATE, "")
    paths = [os.path.join(root, "build.sbt")]
    for pattern in ("project/*.sbt", "project/*.scala"):
        paths += sorted(glob.glob(os.path.join(glob.escape(root), pattern)), key=locale.strxfrm)
    data = b""
    for path in paths + [os.path.join(root, "project/build.properties")]:
        try:
            with open(path, "rb") as f:
                data += f.read()
        except OSError:
            pass
    return digest(data)


def flock(fd: int, mode: int, wait: bool = False) -> bool:
    try:
        fcntl.flock(fd, mode if wait else mode | fcntl.LOCK_NB)
    except OSError:
        return False
    return True


@dataclass
class Tools:
    """A call's collaborators (scripts/sbt-warm's main() makes them)."""
    checkout: Checkout
    env: MutableMapping[str, str]
    servers: Servers
    builds: Builds
    gate: JvmGate
    log: Callable[[str], None]
    me: str
    sleep: Callable[[float], None]


class Call:
    def __init__(self, tools: Tools, commands: List[str], serve: bool) -> None:
        self.t, self.commands, self.serve = tools, commands, serve
        self.root = tools.checkout.root
        self.command = "; ".join(commands)
        self.job = "serve" if serve else sbt_job(commands)
        self.busy = os.open(tools.checkout.file(".busy"), os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o644)
        self.start_lock = FileLock(tools.checkout.file(".flock"))
        self.ended = threading.Event()
        self.client: Optional[Client] = None
        self.fresh = False

    def file(self, suffix: str) -> str:
        return self.t.checkout.file(suffix)

    def refused(self) -> Optional[int]:
        """An it/ run with no MONGODB_URI: refused at once, naming the command that works."""
        if self.t.env.get("MONGODB_URI") or not any(IT_RUN.search(c) for c in self.commands):
            return None
        self.t.log("refusing an it/ run with no MONGODB_URI: every suite would abort. Run it against a MongoDB of its own:")
        self.t.log(f"  scripts/it-local.sh '{self.command}'")
        return 2

    def fetch_gitops(self) -> None:
        fetch = os.path.join(self.root, "infra/bin/fetch-gitops")
        if self.t.env.get("KINOWO_SBT_WARM_GITOPS", "1") == "0" or os.path.isdir(os.path.join(self.root, "infra/kubernetes")):
            return
        if not os.access(fetch, os.X_OK) or not any(TEST_RUN.search(c) for c in self.commands):
            return
        self.t.log(f"no GitOps manifests in {self.root}: fetching them for the deploy specs (infra/bin/fetch-gitops)")
        env = {**self.t.env, "KINOWO_GITOPS_DEST": os.path.join(self.root, "infra/kubernetes")}
        try:
            code = subprocess.run([fetch], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                                  timeout=float(self.t.env.get("KINOWO_SBT_WARM_GITOPS_SECONDS") or 90)).returncode
        except (OSError, subprocess.TimeoutExpired):
            code = 1
        if code != 0:
            self.t.log("the GitOps fetch failed: the deploy specs will fail on the missing manifests (run infra/bin/fetch-gitops)")

    def share_busy(self) -> None:
        """IN USE: $key.busy held shared for the call's life, waiting out an exclusive holder or an idle stop."""
        if flock(self.busy, fcntl.LOCK_SH):
            return
        holder = read(self.file(".exclusive")).split(None, 1)
        if holder and holder[0].isdigit() and self.t.servers.procs.alive(int(holder[0])):
            who = holder[1] if len(holder) > 1 else ""
            self.t.log(f"another sbt holds the build of {self.root} exclusively ({who}, pid {holder[0]}): waiting for it to end")
        else:
            self.t.log(f"the keeper of {self.root} is stopping its idle server: waiting for it")
        flock(self.busy, fcntl.LOCK_SH, wait=True)

    def lock_start(self) -> None:
        """ONE STARTER: the start lock, waited for up to KINOWO_SBT_WARM_LOCK_SECONDS."""
        waited, limit = 0, int(self.t.env.get("KINOWO_SBT_WARM_LOCK_SECONDS") or 1800)
        while not self.start_lock.take(os.getpid()):
            if waited == 0:
                self.t.log(f"another call is starting the server of {self.root}: waiting for it")
            if waited >= limit:
                self.t.log(f"gave up waiting for the server start lock of {self.root}")
                sys.exit(1)
            self.t.sleep(1)
            waited += 1

    def heartbeat(self) -> None:
        every = float(self.t.env.get("KINOWO_SBT_WARM_HEARTBEAT_SECONDS") or 60)
        while True:
            touch(self.file(".used"))
            if self.ended.wait(every):
                return

    def blocked(self) -> Optional[int]:
        """ONE COMMAND AT A TIME: a live serve in this server fails the call (75); an orphaned one restarts it."""
        serving = self.file(".serving")
        pid = self.t.servers.pid(self.root)
        if os.path.isfile(serving) and pid is not None:
            fields = read(serving).split(None, 2) + ["", "", ""]
            spid, owner, cmd = fields[0], fields[1] or "-", fields[2]
            procs = self.t.servers.procs
            if spid.isdigit() and "sbt-warm" in procs.args(int(spid)) and (owner == "-" or (owner.isdigit() and procs.alive(int(owner)))):
                return self.say_blocked(pid, spid, owner, cmd)
            self.t.servers.stop_all(self.root, f"still serving `{cmd}` for sbt-warm pid {spid} / owner {owner}, which ended: an "
                                    "orphaned command every later one would queue behind",
                                    "orphaned serving command, alive 10 s after SIGTERM")
            remove(serving)
            remove(os.path.join(self.root, "project/target/active.json"))
        if self.t.servers.pid(self.root) is None:
            remove(serving)
        return None

    def say_blocked(self, pid: int, spid: str, owner: str, cmd: str) -> int:
        self.t.log(f"server pid {pid} of {self.root} is serving `{cmd}` (sbt-warm pid {spid}, owner {owner}): this command "
                   "would wait for it to end")
        sys.stderr.write(f"sbt-warm BLOCKED: the server of {self.root} is serving `{cmd}` (sbt-warm pid {spid}); stop that "
                         "first (its owner's clean-up, or scripts/sbt-warm --stop)\n")
        if self.t.env.get("KINOWO_STALL_FILE"):
            with open(self.t.env["KINOWO_STALL_FILE"], "a") as f:
                f.write(f"sbt-warm blocked by serving {cmd}\n")
        return STALLED

    def settle_server(self) -> None:
        """Which server the call runs in: this checkout's own, restarted when its KINOWO_* settings differ."""
        pid = self.t.servers.pid(self.root)
        if pid is not None:
            self.t.log(f"checkout {self.root}, server pid {pid}")
        else:
            self.fresh = True
            remove(os.path.join(self.root, "project/target/active.json"))
            self.t.log(f"checkout {self.root}, server none: starting one")
        settings = settings_digest(self.t.env)
        if not self.fresh and os.path.isfile(self.file(".env")) and read(self.file(".env")) != settings:
            self.restart(pid)
        if self.fresh:
            write(self.file(".env"), settings)

    def restart(self, pid: Optional[int]) -> None:
        self.t.log(f"this call's KINOWO_* settings differ from the ones server pid {pid} started with: restarting it with them")
        if not flock(self.busy, fcntl.LOCK_EX):
            self.t.log(f"another call is running a command in server pid {pid}: waiting for it to end before restarting it")
            flock(self.busy, fcntl.LOCK_EX, wait=True)
        self.t.servers.stop_servers_of(self.root, "this call's KINOWO_* settings differ from the ones it started with")
        self.t.servers.await_none(self.root, 10)
        remove(os.path.join(self.root, "project/target/active.json"))
        flock(self.busy, fcntl.LOCK_SH, wait=True)
        self.fresh = True

    def reload_first(self) -> None:
        """The build changed under a kept server (a rebase onto a new module): `reload` before the commands."""
        now = build_digest(self.root)
        if not self.fresh and os.path.isfile(self.file(".build")) and read(self.file(".build")) != now:
            self.t.log("the build changed since the server loaded it: reload first")
            self.command = "reload; " + self.command
        write(self.file(".build"), now)

    def keep_slot(self) -> None:
        """ONE JVM, ONE SLOT: a live keeper holds the server's slot; else this call takes one and hands it to a keeper."""
        keep = self.file(".keep.flock")
        if lock_busy(keep):
            self.t.log(f"server kept by keeper pid {read(keep)}, which holds its JVM slot: this command takes none")
            return
        gate, env = self.t.gate, self.t.env
        job = env.get("KINOWO_GOV_JOB")
        env["KINOWO_GOV_JOB"] = "keeper"
        try:
            gate.slot_take("sbt-warm")
        finally:
            if job is None:
                env.pop("KINOWO_GOV_JOB", None)
            else:
                env["KINOWO_GOV_JOB"] = job
        lock = FileLock(keep, KEEP_FD)
        if not lock.take(os.getpid()):
            gate.release()
            return
        pid = gate.keeper_start(self.root, self.file(".log"), [self.t.me, "--keep"], (KEEP_FD,))
        write(keep, str(pid))
        lock.close()
        idle = env.get("KINOWO_SBT_WARM_IDLE_MINUTES") or "10"
        self.t.log(f"keeper pid {pid} holds the server's JVM slot; it stops the server after {idle} idle minutes, or with "
                   "the checkout")

    def mark_serving(self) -> None:
        owner = self.t.env.get("KINOWO_SBT_WARM_SERVE_OWNER", "")
        write(self.file(".serving"), f"{os.getpid()} {owner or '-'} {self.command}")
        if owner.isdigit():
            threading.Thread(target=self.watch_owner, args=(int(owner),), daemon=True).start()

    def watch_owner(self, owner: int) -> None:
        """Its owner gone (killed: no clean-up of its own ran), the serve's command would hold the server forever."""
        poll = float(self.t.env.get("KINOWO_SBT_WARM_OWNER_POLL") or 2)
        while not self.ended.is_set():
            if not self.t.servers.procs.alive(owner):
                self.t.servers.stop_servers_of(self.root, f"serving `{self.command}` for pid {owner}, which ended")
                return
            self.ended.wait(poll)

    def terminated(self, signum: int, _frame: Optional[FrameType]) -> None:
        if self.client is not None and self.client.proc is not None:
            for pid in stall_watch.tree_pids(self.client.proc.pid):
                try:
                    os.kill(pid, signal.SIGTERM)
                except OSError:
                    pass
        os._exit(143)

    def client_for(self) -> Client:
        guard = os.path.join(os.path.dirname(self.t.me), "ci/sbt-guard/sbt")
        sbt = guard if os.access(guard, os.X_OK) else "sbt"
        return Client(self.t.checkout, self.t.servers, self.command, sbt, self.t.env, self.t.log, self.t.servers.now)

    def run_client(self) -> Ended:
        self.client = self.client_for()
        self.client.start(self.t.gate.cpu_options())
        if self.serve:
            return Ended(self.client.wait())
        progress = self.t.env.get("KINOWO_PROGRESS_FILE") or None
        return Watch(self.client, WatchSettings.read(self.t.env), progress).run()

    def run_watched(self) -> int:
        ended = self.run_client()
        if self.serve:
            remove(self.file(".serving"))
            return ended.code
        if ended.code != STALLED:
            return ended.code
        self.t.log(f"running `{self.command}` once more, in a fresh server")
        again = self.run_client()
        if again.code == STALLED:
            sys.stderr.write(f"sbt-warm STALLED: {self.command} ({again.why}, twice, each in a fresh server; thread dumps: "
                             f"{ended.dump or '-'} {again.dump or '-'})\n")
            if self.t.env.get("KINOWO_STALL_FILE"):
                with open(self.t.env["KINOWO_STALL_FILE"], "a") as f:
                    f.write(f"sbt-warm {self.command}\n")
        return again.code

    def prepare(self) -> Optional[int]:
        """Everything before the command runs; a status when the call ends here."""
        self.share_busy()
        if self.t.servers.pid(self.root) is None:
            self.lock_start()
        self.t.servers.reap(None)
        self.t.servers.dedupe(self.root)
        self.t.builds.seed()
        write(self.file(".root"), self.root)
        threading.Thread(target=self.heartbeat, daemon=True).start()
        code = self.blocked()
        if code is not None:
            return code
        self.settle_server()
        self.reload_first()
        self.keep_slot()
        return None

    def run(self) -> int:
        try:
            code = self.prepare()
            return code if code is not None else self.execute()
        finally:
            self.ended.set()
            self.start_lock.give()

    def passed(self, code: int, used: int, wall: int) -> None:
        """A command's call ended: a passing one records its cores and publishes its build (with no other call running)."""
        if code != 0:
            return
        if self.t.servers.pid(self.root) is not None:
            self.t.gate.usage(self.job, wall, used, "tree")
        if flock(self.busy, fcntl.LOCK_EX):
            self.t.builds.publish()

    def execute(self) -> int:
        gate, servers = self.t.gate, self.t.servers
        gate.cpu_take(f"sbt-warm {self.job}", self.job)
        started = time.monotonic()
        meter = CpuMeter(lambda: [p for p in [servers.pid(self.root)] if p is not None],
                         float(self.t.env.get("KINOWO_CPU_METER_POLL") or 2), servers.procs.alive)
        if self.serve:
            self.mark_serving()
        else:
            meter.start()
        signal.signal(signal.SIGTERM, self.terminated)
        signal.signal(signal.SIGINT, self.terminated)
        code = self.run_watched()
        self.client = None
        if not self.serve:
            self.passed(code, meter.stop(), int(time.monotonic() - started))
        self.ended.set()
        gate.release()
        touch(self.file(".used"))
        pid = servers.pid(self.root)
        if self.fresh and pid is not None:
            write(self.file(".pid"), str(pid))
            self.t.log(f"kept sbt server pid {pid} for {self.root}")
        return code


def write(path: str, text: str) -> None:
    with open(path, "w") as f:
        f.write(text + "\n")


def touch(path: str) -> None:
    with open(path, "a"):
        pass
    os.utime(path)
