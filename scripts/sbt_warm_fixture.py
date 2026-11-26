"""The fixture scripts/sbt-warm's black-box tests share (sbt_warm_test.py, sbt_warm_keeper_test.py,
sbt_warm_stall_test.py, sbt_warm_governor_test.py), ported from sbt-warm-test.sh: a scratch origin and repository at two
main commits, builds of an older main commit (a worktree), of the newest (an identity ref tree) and of a branch ahead of
main, and a stub `sbt --client` that runs the commands in the server project/target/active.json names (starting one in
the current checkout when there is none) and logs which sources that server sees. The tool runs by path, so the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import fcntl
import hashlib
import os
import subprocess
import sys
from typing import Dict, List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402
import machine  # noqa: E402

TOOL = cli_harness.tool_path("scripts/sbt-warm")
ROOT = cli_harness.ROOT
SERVER_ARGS = "java -jar /fake/sbt-launch.jar --detach-stdio"

# the stub is sbt --client (@S@: the case's scratch directory)
STUB = r"""#!/usr/bin/env bash
printf "%s|%s\n" "$(pwd -P)" "$*" >> "@S@/sbt-calls"
# ...and which of the lock descriptors (scripts/jvm-gate.sh) it inherited: the server it starts must hold none
printf "%s|%s|%s\n" "${SBT_OPTS:-}" "${KINOWO_JVM_SLOT_HELD:-}" "$(for fd in 5 6 7 8 9; do { : >&$fd; } 2>/dev/null && printf 'fd%s ' $fd; done)" >> "@S@/sbt-env"
active=project/target/active.json
[ -f "$active" ] || { mkdir -p project/target; echo "{\"uri\":\"local://$(pwd -P)/sock\"}" > "$active"; }
server=$(sed -n 's|.*"local://\(.*\)/sock".*|\1|p' "$active")
printf "%s|%s\n" "$server" "$(cat "$server/worker/src/s" 2>/dev/null)" >> "@S@/sbt-sources"
[ -n "${STUB_FAIL:-}" ] && exit 1
# a stale TASTy read, once: the command fails with the signature (project/StaleTastyRetry.scala)
if [ -n "${STUB_STALE_ONCE:-}" ] && [ ! -e "$STUB_STALE_ONCE" ] && [ "$*" != "--client staleTastyDrop" ]; then
  touch "$STUB_STALE_ONCE"; echo "[error] Ignoring analyses of x due to error in reading TASTy"; exit 1
fi
# a slow command: held until the case writes to the slow-go fifo, then whether the server named was still running
if [ -n "${STUB_SLOW_SERVER:-}" ]; then
  touch "@S@/slow-started"; read -r _ < "@S@/slow-go"; kill -0 "$STUB_SLOW_SERVER" 2>/dev/null && echo running > "@S@/slow" || echo stopped > "@S@/slow"
fi
# a long command: its call's last-used mark aged past the idle timeout mid-run, then whether it is fresh again
if [ -n "${STUB_AGE_USED:-}" ]; then
  touch -t 202001010000 "$STUB_AGE_USED"; sleep 3
  [ -n "$(find "$STUB_AGE_USED" -mmin +1)" ] && echo idle > "@S@/busy" || echo busy > "@S@/busy"
fi
"""


def checkout_key(root: str) -> str:
    """The tool's key of a checkout: the first 12 hex digits of its path's SHA-1."""
    return hashlib.sha1(root.encode()).hexdigest()[:12]


def lock_state(path: str) -> str:
    """"held" while some process holds `path`'s kernel lock, else "free" (shell-spec.sh's spec_lock_state)."""
    if not os.path.exists(path):
        return "free"
    with open(path, "a") as f:
        try:
            fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            return "held"
    return "free"


def kill_group(pid: int) -> None:
    """`pid`'s process group, else `pid` alone, killed."""
    if not machine.send(pid, 9, group=True):
        machine.send(pid, 9)


def state_word(pid: int) -> str:
    return "running" if cli_harness.alive(pid) else "stopped"


class SbtWarmCase(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.state = self.path("state")
        self.identity = self.path("identity")
        self.cache = self.path("cache")
        self.extra: Dict[str, Optional[str]] = {
            "KINOWO_SBT_WARM_STATE": self.state, "KINOWO_IDENTITY_CACHE": self.identity, "KINOWO_SBT_CACHE": self.cache,
            "KINOWO_SBT_CACHE_PUBLISH": "0", "KINOWO_SBT_WARM_SEED": None, "KINOWO_STALL_FILE": None,
            "KINOWO_PROGRESS_FILE": None, "KINOWO_SBT_WARM_KEEPER_POLL": "0.2",
            "KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS": "2", "KINOWO_GOVERNOR": "0", "MONGODB_URI": None}
        self.stub(STUB)
        self.servers: List["subprocess.Popen[bytes]"] = []
        self.addCleanup(self.end_servers)
        self.addCleanup(self.end_keepers)
        self.make_repositories()

    def stub(self, body: str) -> None:
        """The sbt on the tool's PATH: `body` with @S@ the scratch directory."""
        path = os.path.join(self.fakes.bin, "sbt")
        with open(path, "w") as f:
            f.write(body.replace("@S@", self.work))
        os.chmod(path, 0o755)

    def shim(self, directory: str, name: str, body: str) -> str:
        """An executable `name` in <scratch>/`directory`, a PATH entry for one call: its directory."""
        path = self.write(os.path.join(directory, name), body.replace("@S@", self.work))
        os.chmod(path, 0o755)
        return self.path(directory)

    def make_repositories(self) -> None:
        self.origin = self.path("origin.git")
        os.makedirs(self.origin)
        self.git(self.origin, "init", "-q", "--bare", "-b", "main")
        self.main = self.repo("repo", "-b", "main")
        self.write("repo/a", "a\n")
        self.write("repo/worker/src/s", "s\n")
        self.commit_tree(self.main, "one")
        self.git(self.main, "remote", "add", "origin", self.origin)
        self.write("repo/a", "a\nb\n")
        self.commit_tree(self.main, "two")
        self.git(self.main, "push", "-q", "origin", "HEAD:main")
        self.git(self.main, "fetch", "-q", "origin")
        self.head = self.git(self.main, "rev-parse", "HEAD")
        self.git(self.main, "worktree", "add", "-q", "--detach", self.path("old"), "HEAD~1")
        self.build(self.path("old"), "old")
        self.newest = os.path.join(self.identity, "refs", self.head)
        self.build(self.newest, "newest")
        self.ahead = self.worktree("ahead", "HEAD")
        self.write("ahead/a", "a\nb\nc\n")
        self.git(self.ahead, "commit", "-qam", "three")
        self.build(self.ahead, "branch")

    def build(self, directory: str, mark: str) -> None:
        """A build of `directory`: classes and Zinc state saying `mark`, and its server's socket, as sbt leaves it."""
        os.makedirs(os.path.join(directory, "worker/target/scala-3.x/test-classes"), exist_ok=True)
        os.makedirs(os.path.join(directory, "project/target"), exist_ok=True)
        self.put(os.path.join(directory, "worker/target/scala-3.x/zinc/from"), mark + "\n")
        self.put(os.path.join(directory, "project/target/active.json"), '{"uri":"local://%s/sock"}\n' % directory)

    @staticmethod
    def put(path: str, text: str) -> None:
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)

    @staticmethod
    def text(path: str) -> str:
        try:
            with open(path) as f:
                return f.read().strip()
        except OSError:
            return ""

    def worktree(self, name: str, ref: str = "origin/main") -> str:
        self.git(self.main, "worktree", "add", "-q", "-b", name, self.path(name), ref)
        return self.path(name)

    def environment(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, Optional[str]]:
        return {**self.extra, **(extra or {})}

    def warm(self, cwd: str, *args: str, env: Optional[Mapping[str, Optional[str]]] = None,
             bound: float = cli_harness.HANG_BOUND) -> cli_harness.Done:
        """The tool with `args`, from `cwd`."""
        return self.run_tool(*args, env=self.environment(env), cwd=cwd, bound=bound)

    def start(self, cwd: str, *args: str, env: Optional[Mapping[str, Optional[str]]] = None,
              stderr: Optional[str] = None) -> "subprocess.Popen[bytes]":
        """The tool in the background (stdout dropped, stderr to the file `stderr` or dropped)."""
        sink = os.open(stderr, os.O_WRONLY | os.O_CREAT | os.O_TRUNC) if stderr else subprocess.DEVNULL
        how = machine.Ran(cwd, self.env(self.environment(env)), subprocess.DEVNULL, sink)
        proc = machine.spawn([self.TOOL, *args], how)
        if stderr:
            os.close(sink)
        self.servers.append(proc)
        return proc

    def fake_server(self, cwd: str) -> int:
        """A process standing in for an sbt server of `cwd` (its command line a server's), once it carries it. A JVM
        survives SIGQUIT (the thread dump's signal), so the stand-in ignores it."""
        proc = machine.spawn(["bash", "-c", "trap '' QUIT; exec -a \"%s\" sleep 300" % SERVER_ARGS],
                             machine.Ran(cwd, None, subprocess.DEVNULL, subprocess.DEVNULL))
        self.servers.append(proc)
        self.assertTrue(cli_harness.wait_until(lambda: "--detach-stdio" in self.args_of(proc.pid)),
                        f"fake server {proc.pid} never carried the server's command line")
        return proc.pid

    def end_server(self, pid: int) -> None:
        """A fake server ended, and reaped."""
        for proc in self.servers:
            if proc.pid == pid:
                proc.kill()
                proc.wait()

    def background(self, argv: List[str]) -> "subprocess.Popen[bytes]":
        proc = machine.spawn(argv, machine.Ran(None, None, subprocess.DEVNULL, subprocess.DEVNULL))
        self.servers.append(proc)
        return proc

    def gone(self, pid: int) -> bool:
        """Whether `pid` has ended (reaped here when it is a child of this case)."""
        for proc in self.servers:
            if proc.pid == pid:
                return proc.poll() is not None
        return not cli_harness.alive(pid)

    def wait_gone(self, pid: int, what: str) -> None:
        self.assertTrue(cli_harness.wait_until(lambda: self.gone(pid)), f"{what}: pid {pid} still runs")

    def end_servers(self) -> None:
        for proc in self.servers:
            if proc.poll() is None:
                proc.kill()
            proc.wait()

    def end_keepers(self) -> None:
        """Every keeper a case's calls left (each its checkout's lock file names), ended with its group."""
        if not os.path.isdir(self.state):
            return
        for name in os.listdir(self.state):
            path = os.path.join(self.state, name)
            if name.endswith(".keep.flock") and lock_state(path) == "held" and self.text(path).isdigit():
                kill_group(int(self.text(path)))

    @staticmethod
    def args_of(pid: int) -> str:
        return machine.ps("-o", "args=", "-p", str(pid))

    def state_file(self, checkout: str, suffix: str) -> str:
        return os.path.join(self.state, checkout_key(checkout) + suffix)

    def calls(self) -> List[str]:
        """The stub's calls, "<checkout>|<arguments>", in order."""
        return self.text(self.path("sbt-calls")).splitlines()

    def last_call(self) -> str:
        """The arguments of the stub's last call."""
        calls = self.calls()
        return calls[-1].split("|", 1)[1] if calls else ""

    def stops(self) -> str:
        return self.text(os.path.join(self.state, "stops.log"))
