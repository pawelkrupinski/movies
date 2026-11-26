"""scripts/sbt-warm's sbt client and its watch (its header: ONE BUILD PATH, NO PROGRESS and SERVER LIVENESS): the client
runs through scripts/ci/sbt-guard/sbt with every lock descriptor closed, and the call watches it -- a client whose server
and own processes make no progress for KINOWO_SBT_WARM_STALL_SECONDS, or whose server is gone, is ended (status 75),
the server's thread dump written and the server stopped."""
from __future__ import annotations

import datetime
import os
import re
import signal
import subprocess
import time
from dataclasses import dataclass
from typing import Callable, List, Mapping, Optional

import stall_watch
from sbt_warm_servers import Checkout, Servers, command_out, remove

STALLED = 75


@dataclass
class WatchSettings:
    poll: float
    need: int
    live: float
    start: float
    stall: float

    @classmethod
    def read(cls, env: Mapping[str, str]) -> "WatchSettings":
        return cls(float(env.get("KINOWO_STALL_POLL_SECONDS") or 30), int(env.get("KINOWO_STALL_CPU_CENTIS") or 100),
                   float(env.get("KINOWO_SBT_WARM_LIVENESS_SECONDS") or 15),
                   float(env.get("KINOWO_SBT_WARM_START_SECONDS") or 240),
                   float(env.get("KINOWO_SBT_WARM_STALL_SECONDS") or 600))


@dataclass
class Ended:
    """How a watched client ended: its status, and for a stall (75) why and its thread dump."""
    code: int
    why: str = ""
    dump: str = ""


class Client:
    def __init__(self, checkout: Checkout, servers: Servers, command: str, sbt: str, env: Mapping[str, str],
                 say: Callable[[str], None], now: Callable[[], datetime.datetime]) -> None:
        self.checkout, self.servers, self.command, self.sbt, self.env = checkout, servers, command, sbt, env
        self.say, self.now = say, now
        self.proc: Optional["subprocess.Popen[bytes]"] = None

    @property
    def root(self) -> str:
        return self.checkout.root

    def start(self, sbt_opts: str) -> None:
        env = dict(self.env)
        env["SBT_OPTS"] = f"{env.get('SBT_OPTS', '')} {sbt_opts}"
        self.proc = subprocess.Popen([self.sbt, "--client", self.command], cwd=self.root, env=env)

    def running(self) -> bool:
        """Whether the client still runs: an ended one not yet waited for (a zombie) does not."""
        return self.proc is not None and self.proc.poll() is None

    def tree(self) -> List[int]:
        """The client's and the server's process trees."""
        pids = stall_watch.tree_pids(self.proc.pid) if self.proc is not None else []
        server = self.servers.pid(self.root)
        return pids + (stall_watch.tree_pids(server) if server is not None else [])

    def tree_state(self) -> str:
        """The client's and server's processes as one line (pid, state, CPU time, command)."""
        out = command_out(["ps", "-o", "pid=,stat=,time=,args=", "-p", stall_watch.pid_list(self.tree())])
        return ";".join(re.sub(" +", " ", line[:160]) for line in out.splitlines())

    def end(self) -> None:
        """The client's tree ended: TERM, then KILL 5 s later; one surviving even KILL is abandoned, never waited for."""
        if self.proc is None:
            return
        pids = stall_watch.tree_pids(self.proc.pid)
        signal_all(pids, signal.SIGTERM)
        for _ in range(10):
            if not tree_running(pids):
                break
            time.sleep(0.5)
        if tree_running(pids):
            signal_all(pids, signal.SIGKILL)
        for _ in range(4):
            if not self.running():
                break
            time.sleep(0.5)
        if self.running():
            self.say(f"client pid {self.proc.pid} survives KILL: abandoned")

    def wait(self) -> int:
        """The client's status, as a shell gives it (128 + a signal's number)."""
        if self.proc is None:
            return 0
        code = self.proc.wait()
        return 128 - code if code < 0 else code

    def gone_ended(self) -> Ended:
        self.say(f"the server of {self.root} is gone, and the client of `{self.command}` waits on it: ending the client")
        self.say(f"...its processes: {self.tree_state()}")
        self.end()
        remove(os.path.join(self.root, "project/target/active.json"))
        return Ended(STALLED, "its server vanished")

    def stalled(self, quiet: float, stall: float) -> Ended:
        """No progress for `quiet` s: the server's thread dump, the client ended, the server stopped."""
        server = self.servers.pid(self.root)
        dump = self.checkout.file(f".stall-{self.now().strftime('%Y%m%d-%H%M%S')}.txt")
        if server is not None:
            self.thread_dump(server, dump, quiet)
        else:
            self.say(f"no progress for {quiet:g}s on `{self.command}`, and no server of {self.root} runs: the client "
                     "waits on nothing")
        self.say(f"...its processes: {self.tree_state()}")
        self.end()
        self.servers.stop_all(self.root, f"no progress for {quiet:g}s on `{self.command}` (thread dump: "
                                         f"{dump if server is not None else ''})", "stalled, alive 10 s after SIGTERM")
        remove(os.path.join(self.root, "project/target/active.json"))
        return Ended(STALLED, f"no progress for {stall:g}s", dump)

    def thread_dump(self, server: int, dump: str, quiet: float) -> None:
        jcmd = os.path.join(self.env["JAVA_HOME"], "bin/jcmd") if self.env.get("JAVA_HOME") else "jcmd"
        with open(dump, "w") as f:
            f.write(f"# server pid {server} of {self.root}, `{self.command}`: no progress for {quiet:g}s\n")
            f.flush()
            if run_into([jcmd, str(server), "Thread.print"], f) != 0:
                run_into(["jstack", str(server)], f)
        self.say(f"no progress for {quiet:g}s on `{self.command}` (server pid {server} and client at 0% CPU); thread "
                 f"dump: {dump}")
        with open(dump, errors="replace") as f:
            text = f.read()
        main = re.search(r'^"main".*(?:\n.*){0,14}', text, re.M)
        if main and "sbt.Execute.runKeep" in main.group(0):
            frame = re.search(r"^[ \t]+at ((?:tools|services|controllers)\..*)$", text, re.M)
            self.say("...its main thread is running a command: this one was queued behind it "
                     f"({'at ' + frame.group(1) if frame else ''})")


def run_into(argv: List[str], sink: object) -> int:
    try:
        return subprocess.run(argv, stdout=sink, stderr=subprocess.STDOUT).returncode  # type: ignore[arg-type]
    except OSError:
        return 127


def signal_all(pids: List[int], signum: int) -> None:
    for pid in pids:
        try:
            os.kill(pid, signum)
        except OSError:
            pass


def tree_running(pids: List[int]) -> bool:
    """Whether one of `pids` runs (an ended one not yet waited for, a zombie, does not)."""
    if not pids:
        return False
    out = command_out(["ps", "-o", "stat=", "-p", stall_watch.pid_list(pids)])
    return any(line.strip() and not line.startswith("Z") for line in out.splitlines())


class Watch:
    """The poll loop over a running client (sbt-warm's watch_client): its status, or a stall's Ended."""

    def __init__(self, client: Client, settings: WatchSettings, progress: Optional[str],
                 clock: Callable[[], float] = time.monotonic, sleep: Callable[[float], None] = time.sleep) -> None:
        self.client, self.settings, self.progress, self.clock, self.sleep = client, settings, progress, clock, sleep
        self.flag = stall_watch.children_flag()
        self.server_seen, self.gone_checks = False, 0
        self.before: List[int] = []
        self.last, self.quiet = 0, 0.0
        self.started = self.due = self.live = 0.0

    def server_gone(self, waited: float) -> bool:
        if self.client.servers.pid(self.client.root) is not None:
            self.server_seen, self.gone_checks = True, 0
            return False
        self.gone_checks += 1
        return self.gone_checks >= 2 if self.server_seen else waited >= self.settings.start

    def progressed(self, last: int, now: int, before: List[int], pids: List[int]) -> bool:
        return (now - last >= self.settings.need or stall_watch.any_runnable(pids)
                or stall_watch.tree_shrank(before, pids) or stall_watch.any_gone(stall_watch.kept_pids(before, pids)))

    def run(self) -> Ended:
        self.started = self.clock()
        self.due, self.live = self.started + self.settings.poll, self.started + self.settings.live
        self.before = self.client.tree()
        self.last, self.quiet = stall_watch.cpu_centis(self.before, self.flag), 0.0
        while self.client.running():
            self.sleep(0.5)
            ended = self.tick()
            if ended is not None:
                return ended
        return Ended(self.client.wait())

    def tick(self) -> Optional[Ended]:
        """Half a second's look: the server's liveness when due, a progress poll when due; a stall's Ended."""
        s = self.settings
        if self.clock() >= self.live:
            self.live = self.clock() + s.live
            if self.server_gone(self.clock() - self.started):
                return self.client.gone_ended()
        if self.clock() < self.due:
            return None
        self.due = self.clock() + s.poll
        if self.poll() and self.quiet >= s.stall:
            return self.client.stalled(self.quiet, s.stall)
        return None

    def poll(self) -> bool:
        """One progress poll; false when the client ended meanwhile (a poll that finds it ended judges nothing)."""
        pids = self.client.tree()
        now = stall_watch.cpu_centis(pids, self.flag)
        if not self.client.running():
            return False
        if self.progressed(self.last, now, self.before, pids):
            self.quiet = 0.0
            self.touch_progress()
        else:
            self.quiet += self.settings.poll
        self.last, self.before = now, pids
        return True

    def touch_progress(self) -> None:
        if self.progress:
            with open(self.progress, "a"):
                pass
            os.utime(self.progress)
