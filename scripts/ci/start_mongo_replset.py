#!/usr/bin/env python3
"""Start a throwaway MongoDB in Docker as a SINGLE-NODE REPLICA SET on 127.0.0.1:27017, and return once it is PRIMARY.

  start-mongo-replset.sh [extra mongod args...]   e.g. --wiredTigerCacheSizeGB 2

A replica set rather than a standalone because change streams (MovieRepoIntegrationSpec, the read-model projector) and
multi-document transactions (the staging fold) are refused by a standalone mongod. Not a `services:` block because
GitHub cannot override a service container's command, and `--replSet` is a command-line flag.

BOUNDED. Every wait gives up after MONGO_START_TIMEOUT_SECONDS (default 180) with the container's log tail, instead of
looping until the job's own ceiling -- which, in the nightly stress run, is three hours of a runner doing nothing.

MONGO_MAJORITY_JOURNAL=false acknowledges a majority write once it is applied, not once it is synced to the journal (the
replica set's `writeConcernMajorityJournalDefault`). Every write the app makes is `w: majority`, so on a database that
dies with the runner it is a disk sync per write bought for nothing: ~7 ms a write against ~0.2 ms, measured on a
single-node set. A convergence leg makes hundreds of thousands of them (the US take-up alone ~160,000).

MONGO_TMPFS=<size> (e.g. 6g) puts the data directory on a RAM-backed tmpfs of at most that size. A convergence leg's
replays write hundreds of thousands of documents into databases that die with the runner, and on disk they kept 1.3 of
the runner's 4 cores in iowait (run 37513892540); a leg holds 1.1-1.5 GB there. Only where the box has the room: a
10g-heap leg does not.

On the runner's own network (`--network host`), not a published port: a client reaching a published port on 127.0.0.1
is relayed by docker-proxy, a userland process copying every byte of every round trip. A convergence leg makes millions
of them, and through its detail phases the runner was ~0.7 of a core busier than the JVM and mongod together (run
37581348550). The image's mongod binds every interface, so 127.0.0.1:27017 reaches it directly.

The image is .github/pinned-images.tsv's `mongo`, pulled BY DIGEST by scripts/ci/pull-pinned-image.sh: GitHub's own
registry first (this repo's GHCR mirror of it), then mirror.gcr.io, then Docker Hub. In CI the job pulls it through
./.github/actions/pull-pinned-image, which runs this tool after the pull with PINNED_IMAGE set to what it pulled
(CiPinnedImageSpec); without PINNED_IMAGE (locally, a test) this tool runs the pull itself.

NATIVE WHERE THE JOB IMAGE BAKES IT: a job image may carry this very release's mongod and mongosh, copied out of the
mongo image, at MONGO_NATIVE_HOME (/opt/mongodb/<version>). There it runs on the job's own localhost with the same
flags -- no image pulled into the job's empty dockerd and no layers unpacked: 8 s of a convergence row's setup on
average, 28 s at p90 (2026-10-23). Its data in /data/db (a tmpfs there for MONGO_TMPFS), its log at MONGO_NATIVE_LOG.
It leaves a marker naming its home, which scripts/ci/mongo-exec.sh reads to run a command beside it instead of in the
container. MONGO_NO_NATIVE=1 runs the container regardless.

Stdlib only. The workflows run it by the name scripts/ci/start-mongo-replset.sh; tested by start_mongo_replset_test.py
beside it (with mongo-exec.sh)."""
from __future__ import annotations

import os
import re
import resource
import subprocess
import sys
import time
from typing import Callable, List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
PULL = os.path.join(HERE, "pull-pinned-image.sh")
MONGO_EXEC = os.path.join(HERE, "mongo-exec.sh")
INITIATE = ('rs.initiate({{_id:"rs0",writeConcernMajorityJournalDefault:{journal},'
            'members:[{{_id:0,host:"127.0.0.1:27017"}}]}})')
QUIET = subprocess.DEVNULL


def say(line: str) -> None:
    print(line, flush=True)


Finished = subprocess.CompletedProcess[str]


def output(argv: Sequence[str], env: Mapping[str, str], stderr: Optional[int] = None) -> Finished:
    """`argv`'s stdout (and its stderr where `stderr` is STDOUT); a program that cannot start fails with 127."""
    try:
        return subprocess.run(list(argv), env=dict(env), stdout=subprocess.PIPE, stderr=stderr, text=True)
    except OSError as e:
        return subprocess.CompletedProcess(list(argv), 127, "", str(e))


def pinned_image(env: Mapping[str, str], *args: str) -> Optional[str]:
    """pull-pinned-image.sh's answer on stdout (a pin's field, or the ref it pulled); None when it failed."""
    done = output([PULL, *args], env)
    return done.stdout.rstrip("\n") if done.returncode == 0 else None


class Starter:
    def __init__(self, env: Mapping[str, str], clock: Callable[[], float], sleep: Callable[[float], None]) -> None:
        self.clock = clock
        self.sleep = sleep
        self.started = clock()
        self.timeout = int(env.get("MONGO_START_TIMEOUT_SECONDS") or "180")
        self.deadline = self.started + self.timeout
        self.journal = env.get("MONGO_MAJORITY_JOURNAL") or "true"
        # The pin's tag is the release (MongoServerVersionParitySpec), so the baked home is /opt/mongodb/<that tag>.
        self.native_home = env.get("MONGO_NATIVE_HOME") or "/opt/mongodb/" + (
            pinned_image(env, "--pin", "mongo", "tag") or "")
        self.marker = env.get("MONGO_NATIVE_MARKER") or "/tmp/kinowo-mongo-native"
        self.env = {**env, "MONGO_NATIVE_MARKER": self.marker}
        self.log = env.get("MONGO_NATIVE_LOG") or "/tmp/mongod.log"
        self.native = not env.get("MONGO_NO_NATIVE") and all(
            os.access(os.path.join(self.native_home, "bin", name), os.X_OK) for name in ("mongod", "mongosh"))

    def seconds(self, since: float) -> int:
        return int(self.clock() - since)

    def mongosh(self, script: str, stdout: Optional[int] = None, stderr: Optional[int] = None) -> Finished:
        """mongosh --eval <script> where the mongod runs (mongo-exec.sh)."""
        argv = [MONGO_EXEC, "mongosh", "--quiet", "--eval", script]
        return subprocess.run(argv, env=self.env, stdout=stdout, stderr=stderr, text=True)

    def is_up(self) -> bool:
        return self.mongosh("db.runCommand({ping:1})", stdout=QUIET, stderr=QUIET).returncode == 0

    def is_primary(self) -> bool:
        done = self.mongosh("rs.status().myState", stdout=subprocess.PIPE, stderr=QUIET)
        return "1" in (done.stdout or "").split("\n")

    def log_tail(self) -> str:
        """mongod's last 50 log lines, from its log file or its container, errors with them."""
        argv = ["tail", "-n", "50", self.log] if self.native else ["docker", "logs", "--tail", "50", "mongo"]
        return output(argv, self.env, stderr=subprocess.STDOUT).stdout

    def show_log_tail(self) -> None:
        sys.stdout.write(self.log_tail())
        sys.stdout.flush()

    def fatal_line(self) -> str:
        """mongod's first fatal ("s":"F") log line's message, else its last line: the reason a start failed."""
        lines = self.log_tail().rstrip("\n").split("\n")
        fatal = next((line for line in lines if '"s":"F"' in line), None)
        message = re.search(r'"msg":"[^"]*"', fatal) if fatal is not None else None
        reason = message.group(0) if message else lines[-1]
        return (reason or "no log")[:300]

    def container_exited(self, what: str) -> bool:
        """A container that has exited never becomes reachable: say so at once, with mongod's own fatal line, instead
        of polling it for the whole bound. Every leg of convergence #523 (run 41199223125, 2026-10-31) waited out its
        180 s for a mongod that had died in its first second (SERVER-121912). A `docker inspect` that cannot answer
        says nothing either way. The native mongod needs no such check: `--fork` returns only once the child is up."""
        if self.native:
            return False
        done = output(["docker", "inspect", "-f", "{{.State.Running}} {{.State.ExitCode}}", "mongo"], self.env,
                      stderr=QUIET)
        state = done.stdout.rstrip("\n")
        if done.returncode != 0 or state.split(" ", 1)[0] != "false":
            return False
        status = state.split(" ", 1)[1] if " " in state else state
        say(f"::error::MongoDB's container exited (status {status}) {self.seconds(self.started)}s after it started, "
            f"before it became {what}: {self.fatal_line()}")
        return True

    def wait_for(self, what: str, ready: Callable[[], bool]) -> bool:
        """Retry once a second until `ready`, the deadline passes or the container exits, saying how long it took."""
        waited = self.clock()
        while not ready():
            if self.container_exited(what):
                self.show_log_tail()
                return False
            if self.clock() >= self.deadline:
                say(f"::error::MongoDB did not become {what} within {self.timeout}s: {self.fatal_line()}")
                self.show_log_tail()
                return False
            self.sleep(1)
        say(f"MongoDB {what} after {self.seconds(waited)}s ({self.seconds(self.started)}s since the start)")
        return True

    def replica_set(self) -> int:
        """Once mongod runs: wait for it, initiate the set, wait for it to be PRIMARY."""
        if not self.wait_for("reachable", self.is_up):
            return 1
        if self.mongosh(INITIATE.format(journal=self.journal)).returncode != 0:
            return 1
        if not self.wait_for("PRIMARY", self.is_primary):
            return 1
        say("MongoDB replica set rs0 is PRIMARY on 127.0.0.1:27017")
        return 0

    def start(self, args: Sequence[str]) -> int:
        try:
            os.remove(self.marker)
        except FileNotFoundError:
            pass
        return self.start_native(args) if self.native else self.start_container(args)

    def start_container(self, args: Sequence[str]) -> int:
        tmpfs = self.env.get("MONGO_TMPFS")
        storage: List[str] = ["--tmpfs", f"/data/db:rw,size={tmpfs}"] if tmpfs else []
        image = self.env.get("PINNED_IMAGE") or pinned_image(self.env, "mongo")
        if not image:
            return 1
        sys.stdout.flush()
        argv = ["docker", "run", "-d", "--name", "mongo", "--network", "host", *storage, image, "--replSet", "rs0",
                *args]
        if subprocess.run(argv, env=self.env).returncode != 0:
            return 1
        return self.replica_set()

    def start_native(self, args: Sequence[str]) -> int:
        dbpath = self.env.get("MONGO_NATIVE_DBPATH") or "/data/db"
        if not self.data_directory(dbpath):
            return 1
        mongod = os.path.join(self.native_home, "bin", "mongod")
        version = output([mongod, "--version"], self.env).stdout.split("\n")[0]
        say(f"MongoDB: the job image's native {version}")
        argv = [mongod, "--replSet", "rs0", "--bind_ip", "127.0.0.1", "--port", "27017", "--dbpath", dbpath,
                "--fork", "--logpath", self.log, *args]
        if subprocess.run(argv, env=self.env, stdout=QUIET).returncode != 0:
            sys.stdout.write(output(["tail", "-n", "50", self.log], self.env, stderr=QUIET).stdout)
            return 1
        with open(self.marker, "w") as f:
            f.write(self.native_home + "\n")
        return self.replica_set()

    def data_directory(self, dbpath: str) -> bool:
        """The native mongod's data directory, ours to write, on a tmpfs when asked: whether it is ready."""
        made = self.made(dbpath)
        if not (made and os.access(dbpath, os.W_OK)):
            owner = f"{os.getuid()}:{os.getgid()}"
            if subprocess.run(["sudo", "chown", owner, dbpath], env=self.env).returncode != 0:
                return False
        size = self.env.get("MONGO_TMPFS")
        if size:
            # A tmpfs that will not mount leaves the data on disk: slower, never wrong.
            options = f"size={size},uid={os.getuid()},gid={os.getgid()},mode=0700"
            mount = ["sudo", "mount", "-t", "tmpfs", "-o", options, "tmpfs", dbpath]
            if subprocess.run(mount, env=self.env).returncode != 0:
                say(f"::warning::could not mount a {size} tmpfs at {dbpath} -- MongoDB's data stays on disk")
        # one .wt file per collection: a hosted runner's open-file limit, as the container's dockerd gives it
        try:
            hard = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
            resource.setrlimit(resource.RLIMIT_NOFILE, (hard, hard))
        except (ValueError, OSError):
            pass
        return True

    def made(self, dbpath: str) -> bool:
        try:
            os.makedirs(dbpath, exist_ok=True)
            return True
        except OSError:
            return subprocess.run(["sudo", "mkdir", "-p", dbpath], env=self.env).returncode == 0


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    return Starter(env, time.monotonic, time.sleep).start(argv)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
