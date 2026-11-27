"""HOW A LAPTOP REACHES PROD MONGO, for the Python tools (scripts/lib): scripts/local-mirror/prod-tunnel.sh's
prod_tunnel_uri, prod_tunnel_free_port, prod_tunnel_port, prod_tunnel_target, prod_tunnel_answers, ensure_prod_tunnel
and close_prod_tunnel, with the same lines. The shell library stays for its shell callers (with-prod-mongo.sh,
prod-ops.sh, identity-capture.sh, identity-calibrate.sh, reset-corpus.sh), which hold the forward's pid; its header says why an ssh forward, why
127.0.0.1 on the far side, why a port of each caller's own, why an authenticated ping and not a TCP probe, and why each
ssh option. Repoint both together.

  tunnel_uri(uri, port)    the laptop's view of a prod URI: 127.0.0.1:<port>, directConnection=true, the rest kept
  free_port(env, pid)      PROD_TUNNEL_PORT, else the first port nothing accepts on from a pid-derived start
  ProdTunnel(tag, probe_uri, env, mongosh_at, pause)   .answers(), .ensure(), .close(), .alive(): ONE caller's forward

The shell library calls the two pure steps through this file's command line, so each is written once:
  prod_tunnel.py uri --pid <caller's pid> [--pin $PROD_TUNNEL_PORT] <prod uri>   the laptop's URI (exit 1, nothing printed: no free port)
  prod_tunnel.py free-port --pid <caller's pid>        the port the caller's tunnel binds (exit 1: none free)
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
from typing import Callable, List, Mapping, Optional

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import mongo_local  # noqa: E402

DEFAULT_TARGET = "root@178.105.221.61"  # mongo-1's public address (infra/nix/hosts/mongo-1/default.nix)
PORT_LOW, PORT_HIGH, PORT_TRIES = 27100, 27900, 200
SSH_OPTIONS = ["-o", "ExitOnForwardFailure=yes", "-o", "BatchMode=yes", "-o", "ConnectTimeout=10",
               "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=3",
               "-o", "ControlMaster=no", "-o", "ControlPath=none"]
PING_TIMEOUT = 15  # seconds: serverSelectionTimeoutMS bounds the connect, not a mongod that stalls after it
URI_PORT = re.compile(r"^mongodb://([^@/]*@)?127\.0\.0\.1:([0-9]+)")


def tunnel_uri(uri: str, port: str) -> str:
    rest = uri.split("://", 1)[1] if "://" in uri else uri
    creds = ""
    if "@" in rest:
        user, rest = rest.split("@", 1)
        creds = user + "@"
    cut = re.search(r"[/?]", rest)
    tail = rest[cut.start():] if cut else ""
    if not tail.startswith("/"):
        tail = "/" + tail
    query = ""
    if "?" in tail:
        tail, query = tail.split("?", 1)
        query = "&".join(p for p in query.split("&") if not p.startswith("directConnection="))
    return f"mongodb://{creds}127.0.0.1:{port}{tail}?{query + '&' if query else ''}directConnection=true"


def free_port(env: Mapping[str, str], pid: int) -> Optional[str]:
    if env.get("PROD_TUNNEL_PORT"):
        return env["PROD_TUNNEL_PORT"]
    port = PORT_LOW + pid % (PORT_HIGH - PORT_LOW)
    for _ in range(PORT_TRIES):
        if not mongo_local.answers(str(port)):
            return str(port)
        port = port + 1 if port + 1 < PORT_HIGH else PORT_LOW
    return None


def target(env: Mapping[str, str]) -> str:
    return env.get("KINOWO_MONGO_SSH") or DEFAULT_TARGET


def shell_status(code: int) -> int:
    """A child's exit status as a shell reports it: a signal N is 128+N."""
    return code if code >= 0 else 128 - code


class ProdTunnel:
    """One caller's forward. `mongosh_at` is the argv prefix that runs mongosh at a URI (mongosh-at.sh); `pause`
    waits the seconds given."""

    def __init__(self, tag: str, probe_uri: str, env: Mapping[str, str], mongosh_at: List[str],
                 pause: Callable[[str], None]) -> None:
        self.tag = tag
        self.probe_uri = probe_uri
        self.env = env
        self.mongosh_at = mongosh_at
        self.pause = pause
        self.ssh: Optional[subprocess.Popen[bytes]] = None
        self.port = ""

    def say(self, text: str) -> None:
        print(f"[{self.tag}] {text}", flush=True)

    def warn(self, text: str) -> None:
        print(f"[{self.tag}] {text}", file=sys.stderr, flush=True)

    def current_port(self) -> Optional[str]:
        if self.env.get("PROD_TUNNEL_PORT"):
            return self.env["PROD_TUNNEL_PORT"]
        found = URI_PORT.match(self.probe_uri)
        return found.group(2) if found else free_port(self.env, os.getpid())

    def answers(self) -> bool:
        if not mongo_local.answers(self.current_port() or "27017"):
            return False
        if not self.probe_uri:
            return True  # nothing to authenticate with; TCP is all we have
        probe = re.sub(r"[?&]serverSelectionTimeoutMS=[0-9]+", "", self.probe_uri)
        probe += ("&" if "?" in probe else "?") + "serverSelectionTimeoutMS=3000"
        try:
            done = subprocess.run([*self.mongosh_at, probe, "--quiet", "--eval", "db.adminCommand({ping:1})"],
                                  stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                                  timeout=PING_TIMEOUT)
        except subprocess.TimeoutExpired:
            return False  # a half-open tunnel: ssh up, mongod stalled
        return done.returncode == 0

    def alive(self) -> bool:
        return self.ssh is not None and self.ssh.poll() is None

    def ensure(self) -> bool:
        """Up if prod answers, ours if we open it: True when prod answers through 127.0.0.1:<port>. Never raises."""
        port = self.current_port()
        if port is None:
            self.warn(f"no free local port in {PORT_LOW}-{PORT_HIGH - 1} for the prod tunnel")
            return False
        self.port = port
        if self.answers():
            return True
        if self.ssh is not None:
            self.say("tunnel to prod Mongo is down — restarting it")
            self.close()
        elif mongo_local.answers(port):
            self.warn(f":{port} is held by another process but prod does NOT answer through it.")
            self.warn(f"Find the holder with 'lsof -nP -iTCP:{port} -sTCP:LISTEN' and stop that one pid yourself,")
            self.warn("or unset PROD_TUNNEL_PORT to take a free port.")
            return False
        return self.open(port)

    def open(self, port: str) -> bool:
        where = target(self.env)
        self.say(f"opening ssh tunnel to prod Mongo ({where}) on :{port}")
        self.ssh = subprocess.Popen(["ssh", "-N", "-L", f"{port}:127.0.0.1:27017", *SSH_OPTIONS, where],
                                    stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for _ in range(30):
            if self.answers():
                return True
            code = self.ssh.poll()
            if code is not None:
                self.ssh = None
                self.warn(f"ssh to {where} exited (code {shell_status(code)}) before prod answered — 255 is a "
                          "connection or login failure")
                return False
            self.pause("1")
        self.warn(f"prod Mongo did not answer via {where} within 30s")
        return False

    def close(self) -> None:
        """Ends ONLY a forward we started; safe to call more than once."""
        ssh, self.ssh = self.ssh, None
        if ssh is None:
            return
        ssh.terminate()
        try:
            ssh.wait(timeout=5)
        except subprocess.TimeoutExpired:
            ssh.kill()
            ssh.wait()


def main(argv: List[str]) -> int:
    """`uri --pid N [--pin PORT] <prod uri>` or `free-port --pid N [--pin PORT]`: what the shell library asks of this
    file (see the header); --pin is its $PROD_TUNNEL_PORT, when set."""
    rest = argv[1:]
    pinned = {}
    if "--pin" in rest and rest.index("--pin") + 1 < len(rest):
        at = rest.index("--pin")
        pinned = {"PROD_TUNNEL_PORT": rest[at + 1]}
        rest = rest[:at] + rest[at + 2:]
    if len(rest) < 2 or argv[0] not in ("uri", "free-port") or rest[0] != "--pid" or not rest[1].isdigit():
        print("usage: prod_tunnel.py uri --pid N [--pin PORT] <prod uri> | free-port --pid N [--pin PORT]", file=sys.stderr)
        return 64
    port = free_port(pinned, int(rest[1]))
    if port is None:
        print(f"no free local port in {PORT_LOW}-{PORT_HIGH - 1} for the prod tunnel", file=sys.stderr)
        return 1
    if argv[0] == "free-port":
        print(port)
    elif len(rest) == 3:
        print(tunnel_uri(rest[2], port))
    else:
        print("usage: prod_tunnel.py uri --pid N [--pin PORT] <prod uri>", file=sys.stderr)
        return 64
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
