#!/usr/bin/env python3
"""Open the CI end of the route to prod Mongo and DO NOT RETURN until bytes actually flow through it -- then hold it
open until close-mongo-tunnel.sh tears it down, and export the URI the recorder should dial.

  wait-for-mongo-tunnel.sh [local-port]     local-port: default 27018 (27017 is a leg's own throwaway Mongo)

Environment: MONGO_CI_SSH_KEY (required: the PRIVATE half of the key pinned to the forced command on mongo-1),
MONGO_CI_SSH_HOST_KEY (required: mongo-1's host key, one known_hosts line), MONGO_CI_SSH_TARGET (default
mongociread@178.105.221.61, hosts/mongo-1's `fleet.publicAddress`, pinned by terraform). On success it appends
KINOWO_CONVERGENCE_SCRAPES_URI to $GITHUB_ENV, so no step has to spell out a production connection string.

WHY NOT `ssh -L`, WHICH IS WHAT THE LAPTOP USES. A forward is the client naming a host and port for the server to allow;
the key that does it can be pointed at anything the server tolerates. This runner is GitHub's, not ours, so it gets
less than that: its key is pinned to a FORCED COMMAND on mongo-1 (infra/nix/modules/roles/mongo-ci-read.nix) that
relays one connection to an address baked into the host's own closure. The runner cannot ask for a different
destination, cannot get a shell, and cannot forward anything. The shape that falls out of it: one ssh session PER TCP
CONNECTION, so the local listener is a `socat ... fork` that spawns a fresh relay per connect rather than a single
long-lived tunnel process. (Prod Mongo moved off Fly's `flyctl proxy 27017 --app kinowo-mongo` onto mongo-1; that app
is STOPPED, the trap scripts/local-mirror/prod-tunnel.sh documents for the laptop side of the move.)

THE READINESS PROBE SPEAKS MONGO. `nc -z` succeeds the instant something binds the local port, well before there is a
usable upstream -- three convergence legs once spent eight minutes failing against a tunnel that had reported itself
healthy at second three (MongoTimeoutException ... MongoSocketReadTimeoutException). mongo-ping.py sends the smallest
legal handshake and requires a reply: a reply has crossed local socat -> ssh -> the forced command -> mongod.

IT IS SUPERVISED, NOT STARTED-AND-TRUSTED. The old proxy died mid-job and left everything after it reading `Connection
refused`; a listener can die the same way, and "it was alive when the step finished" says nothing about the sbt run
several steps later (SUPERVISOR, below).

The workflows run it by the name scripts/ci/wait-for-mongo-tunnel.sh; tested by wait_for_mongo_tunnel_test.py."""
from __future__ import annotations

import os
import shlex
import shutil
import subprocess
import sys
import time
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mongo_tunnel import DEFAULT_PORT, TunnelPaths, ci_read_uri  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
PROBE = os.path.join(HERE, "mongo-ping.py")
TARGET = "mongociread@178.105.221.61"
# BatchMode: nothing on a runner can answer a passphrase prompt, so hang-forever must become fail-now. IdentitiesOnly:
# the runner's agent may hold other keys and offering them first is how a key-count limit turns into "Too many
# authentication failures".
SSH_OPTIONS = ["-o", "IdentitiesOnly=yes", "-o", "BatchMode=yes", "-o", "StrictHostKeyChecking=yes"]
SSH_TIMING = ["-o", "ConnectTimeout=10", "-o", "ServerAliveInterval=15", "-o", "ServerAliveCountMax=3",
              "-o", "LogLevel=ERROR"]

# Restarts the listener when it EXITS *and* when it merely stops answering. The second kind is the one that cost a run
# before the move: the process stayed alive, held its port, and served nothing, so an exit-only supervisor sat happily
# beside a dead tunnel while every query timed out. The liveness check is the same handshake the readiness probe uses,
# so "supervised" and "ready" mean exactly the same thing. A first failure is tolerated -- the first connection pays
# for an ssh handshake -- but two consecutive silent checks mean it is wedged rather than starting.
SUPERVISOR = """#!/usr/bin/env bash
while true; do
  socat "TCP4-LISTEN:{port},bind=127.0.0.1,reuseaddr,fork" "EXEC:{relay}" >>"{log}" 2>&1 &
  listener=$!
  sleep 5
  misses=0
  while kill -0 "$listener" 2>/dev/null; do
    if python3 "{probe}" "{port}" 2>/dev/null; then
      misses=0
    else
      misses=$((misses + 1))
      if [ "$misses" -ge 2 ]; then
        echo "[tunnel] listener alive but not answering — killing it" >>"{log}"
        kill "$listener" 2>/dev/null || true
        break
      fi
    fi
    sleep 10
  done
  wait "$listener" 2>/dev/null || true
  echo "[tunnel] listener gone — restarting" >>"{log}"
  sleep 2
done
"""


def say(text: str) -> None:
    print(f"[tunnel] {text}", flush=True)


def refuse(text: str) -> int:
    print(f"[tunnel] {text}", file=sys.stderr, flush=True)
    return 1


def missing_secret(env: Mapping[str, str]) -> Optional[str]:
    """Why the secrets cannot open the tunnel, or None. THE HOST KEY IS REQUIRED, AND `accept-new` IS NOT AN ACCEPTABLE
    FALLBACK: a runner is a fresh machine every night with an empty known_hosts, so trust-on-first-use is
    trust-on-every-use -- no verification at all -- and what a wrong host would be handed is the production read
    credential asked for next."""
    if not env.get("MONGO_CI_SSH_KEY"):
        return "MONGO_CI_SSH_KEY is not set — see mongo-ci-read.nix"
    if not env.get("MONGO_CI_SSH_HOST_KEY"):
        return ("MONGO_CI_SSH_HOST_KEY is not set. Refusing to connect unverified:\n[tunnel]   ssh-keyscan -t ed25519 "
                "178.105.221.61   → the MONGO_CI_SSH_HOST_KEY secret")
    return None


def private_file(path: str, text: str) -> None:
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        f.write(text + "\n")
    os.chmod(path, 0o600)


def ensure_socat(env: Mapping[str, str]) -> None:
    """socat turns "one ssh session per connection" into a local port. The runner image lacks it, so convergence-setup
    starts installing it in the background at the top of the job (scripts/ci/in-background.sh) and this waits for
    whatever is left of that -- but it is still installed here rather than assumed: a chore that never started (`wait`
    exits 2 at once) or failed should cost the install's seconds, not a red nightly."""
    if shutil.which("socat", path=env.get("PATH")):
        return
    wait = env.get("SOCAT_WAIT_SECONDS", "180")
    subprocess.run([os.path.join(HERE, "in-background.sh"), "wait", "socat-install", wait], env=dict(env))
    if shutil.which("socat", path=env.get("PATH")):
        return
    say("socat missing — installing")
    subprocess.run([os.path.join(HERE, "drop-microsoft-apt-sources.sh")], env=dict(env), check=True)
    subprocess.run([os.path.join(HERE, "install-apt-package.sh"), "socat"], env=dict(env), check=True)


class Tunnel:
    def __init__(self, port: str, env: Mapping[str, str]) -> None:
        self.port = port
        self.env = env
        self.paths = TunnelPaths.of(env)
        self.target = env.get("MONGO_CI_SSH_TARGET") or TARGET
        self.log = env.get("TUNNEL_LOG") or "/tmp/mongo-tunnel.log"
        self.tries = int(env.get("TUNNEL_ATTEMPTS", "3")) * int(env.get("TUNNEL_PROBE_TRIES", "30"))

    def ssh(self) -> List[str]:
        return ["-T", "-i", self.paths.key, *SSH_OPTIONS, "-o", f"UserKnownHostsFile={self.paths.known_hosts}",
                *SSH_TIMING]

    def credential(self) -> Optional[str]:
        """THE CREDENTIAL COMES FROM THE HOST, not from a repository secret (the endpoint's own comment says why).
        Fetched once, here, so a failure to authorise shows up in the step about the connection, not as an sbt stack
        trace. Both halves masked before the URI exists: GitHub only redacts what it has been told about, and a driver
        logging its connection string on a retry would otherwise print a production password into a public log."""
        say(f"asking {self.target} for the read-only credential")
        done = subprocess.run(["ssh", *self.ssh(), self.target, "credential"], env=dict(self.env),
                              stdout=subprocess.PIPE, text=True)
        if done.returncode != 0:
            sys.exit(done.returncode)
        userinfo = done.stdout.rstrip("\n")
        if userinfo:
            print(f"::add-mask::{userinfo.split(':', 1)[-1]}\n::add-mask::{userinfo}", flush=True)
        return userinfo or None

    def write_relay(self) -> None:
        """ONE RELAY PER CONNECTION. `connect` is the verb; the endpoint decides what it means, and this side
        deliberately cannot say more than that."""
        options = "".join(f"  {shlex.quote(option)} \\\n" for option in self.ssh())
        with open(self.paths.relay, "w") as f:
            f.write(f"#!/usr/bin/env bash\nexec ssh \\\n{options}  {shlex.quote(self.target)} connect\n")
        os.chmod(self.paths.relay, 0o700)

    def detach_supervisor(self) -> None:
        """`setsid` + `nohup`, never a child of this process: the supervisor has to outlive this tool and the step's
        shell, because the reads it exists for happen several steps later."""
        with open(self.paths.supervisor, "w") as f:
            f.write(SUPERVISOR.format(port=self.port, relay=self.paths.relay, log=self.log, probe=PROBE))
        os.chmod(self.paths.supervisor, 0o700)
        subprocess.Popen(["setsid", "nohup", self.paths.supervisor], env=dict(self.env), stdin=subprocess.DEVNULL,
                         stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True)
        say(f"supervisor detached, log {self.log}")

    def ready(self) -> bool:
        for attempt in range(self.tries):
            if subprocess.run(["python3", PROBE, self.port], env=dict(self.env),
                              stderr=subprocess.DEVNULL).returncode == 0:
                return True
            if attempt + 1 < self.tries:
                time.sleep(2)
        return False

    def export(self, userinfo: str) -> None:
        # directConnection and maxPoolSize: mongo_tunnel.ci_read_uri (tools.TunnelTunedUri adds the timeouts)
        say(f"ready on :{self.port} — Mongo answered a handshake through {self.target}")
        github_env = self.env.get("GITHUB_ENV")
        if not github_env:
            print("[tunnel] no $GITHUB_ENV — export KINOWO_CONVERGENCE_SCRAPES_URI yourself", file=sys.stderr)
            return
        with open(github_env, "a") as f:
            f.write(f"KINOWO_CONVERGENCE_SCRAPES_URI={ci_read_uri(userinfo, self.port)}\n")

    def never_answered(self) -> int:
        refuse(f"prod Mongo never answered a handshake on :{self.port} (via {self.target})")
        try:
            with open(self.log, errors="replace") as f:
                sys.stderr.write("".join(f.readlines()[-20:]))
        except OSError:
            pass
        return 1

    def open(self) -> int:
        private_file(self.paths.key, self.env["MONGO_CI_SSH_KEY"])
        private_file(self.paths.known_hosts, self.env["MONGO_CI_SSH_HOST_KEY"])
        userinfo = self.credential()
        if not userinfo:
            return refuse("the endpoint returned no credential")
        ensure_socat(self.env)
        self.write_relay()
        self.detach_supervisor()
        if not self.ready():
            return self.never_answered()
        self.export(userinfo)
        return 0


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if not os.path.isfile(PROBE):
        return refuse(f"missing probe script at {PROBE}")
    why = missing_secret(env)
    if why:
        return refuse(why)
    return Tunnel(argv[0] if argv else DEFAULT_PORT, env).open()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
