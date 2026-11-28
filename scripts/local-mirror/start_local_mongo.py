#!/usr/bin/env python3
"""Ensure the native (Homebrew) single-node replica-set Mongo that backs local dev is up on :28017.

Ensure the native (Homebrew) single-node replica-set Mongo that backs local dev is up on :28017
(LOCAL_MIRROR_PORT names another). Single-node replica set (not standalone) because both the /debug live SSE view AND
the local web dev server read change streams, which a standalone mongod rejects (error 40573) -- the same reason prod
runs `--replSet rs0`. No auth: it's loopback-only, dev-only, already-public data.

One instance, two databases:
  - kinowo_prod_mirror  synced from prod by mirror.sh (the /debug corpus mirror, read-only)
  - kinowo_local        the local web dev server's playground (its own change streams)

This replaces the former `mongo:7.0` Docker container. It's managed by `brew services` when that works, so it restarts
at login on its own; mirror.sh re-invokes this whenever :28017 goes unreachable.

The formula is DETECTED, not hardcoded (LOCAL_MONGO_FORMULA names one): a hardcoded `mongodb-community@7.0` failed
outright (nothing started) on a machine whose `brew services` could not find/load that service. And when `brew
services` cannot start it for any reason, this falls back to a plain `mongod --config … --fork` -- same config, just not
restarted at login. start_local_mongo_test.py holds both paths.

Exit 0 once the set's member is PRIMARY; 1 when mongod did not answer within 60 s or reach PRIMARY within 30 s."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import time
from typing import List, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
from processes import machine  # noqa: E402
from mongo import mongo_local  # noqa: E402

FORMULA = re.compile(r"mongodb-community(@[0-9.]+)?")
INITIATE = '''
  try { rs.status(); print("[local-mongo] replica set already initiated"); }
  catch (e) {
    rs.initiate({_id:"rs0", members:[{_id:0, host:"127.0.0.1:%s"}]});
    print("[local-mongo] replica set initiated");
  }'''


def said(text: str, error: bool = False) -> None:
    print(f"[local-mongo] {text}", file=sys.stderr if error else sys.stdout, flush=True)


PING_TRY_SECONDS = 5.0  # one mongosh ping's own bound: against a down port it would wait its 30 s selection timeout


def output(argv: Sequence[str], quiet: bool = True, timeout: Optional[float] = None) -> Optional[str]:
    """The command's stdout (its stderr dropped unless not quiet); None when it failed, timed out or is missing."""
    try:
        done = subprocess.run(list(argv), stdout=subprocess.PIPE, stderr=subprocess.DEVNULL if quiet else None,
                              text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired):
        return None
    return done.stdout if done.returncode == 0 else None


def last_line(text: Optional[str]) -> str:
    lines = (text or "").splitlines()
    return lines[-1] if lines else ""


def version_key(formula: str) -> List[int]:
    """`sort -V`'s order of the formulas: the unversioned first, then by version."""
    return [int(n) for n in re.findall(r"[0-9]+", formula)]


def config(prefix: str, port: str) -> str:
    """Our config is the source of truth: loopback, our port, single-node replica set. `brew services` launches
    `mongod --config <it>`, so writing it here (default is a standalone on 27017) is what makes change streams work and
    keeps us off the prod tunnel's 27017. Its path."""
    conf, dbpath, logpath = f"{prefix}/etc/mongod.conf", f"{prefix}/var/mongodb", f"{prefix}/var/log/mongodb/mongo.log"
    os.makedirs(dbpath, exist_ok=True)
    os.makedirs(os.path.dirname(logpath), exist_ok=True)
    with open(conf, "w") as f:
        f.write(f"systemLog:\n  destination: file\n  path: {logpath}\n  logAppend: true\nstorage:\n  dbPath: {dbpath}\n"
                f"net:\n  bindIp: 127.0.0.1\n  port: {port}\nreplication:\n  replSetName: rs0\n")
    return conf


def installed_formula() -> str:
    """The installed MongoDB formula: the newest `mongodb-community[@X.Y]`, or the one LOCAL_MONGO_FORMULA names;
    empty when brew has none (mongod from elsewhere)."""
    if machine.environment().get("LOCAL_MONGO_FORMULA"):
        return machine.environment()["LOCAL_MONGO_FORMULA"]
    listed = (output(["brew", "list", "--formula", "-1"]) or "").splitlines()
    formulas = sorted((f for f in listed if FORMULA.fullmatch(f)), key=version_key)
    return formulas[-1] if formulas else ""


class LocalMongo:
    def __init__(self, prefix: str, port: str) -> None:
        self.prefix = prefix
        self.port = port
        self.uri = mongo_local.local_mongo_uri({"LOCAL_MIRROR_PORT": port})

    def evaluates(self, script: str, timeout: Optional[float] = None) -> str:
        return last_line(output(["mongosh", self.uri, "--quiet", "--eval", script], timeout=timeout))

    def answers_ping(self, seconds: float) -> bool:
        """Whether mongod answers a ping within `seconds`: every try is bounded too, so a hanging mongosh cannot stretch
        the deadline."""
        deadline = machine.monotonic() + seconds
        while (remaining := deadline - machine.monotonic()) > 0:
            if self.evaluates("db.runCommand({ping:1}).ok", min(PING_TRY_SECONDS, remaining)) == "1":
                return True
            time.sleep(min(1.0, max(0.0, deadline - machine.monotonic())))
        return False

    def mongod_bin(self, formula: str) -> str:
        """The formula's own mongod when brew has one, so the fallback runs the same version `brew services` would;
        else whatever `mongod` is on PATH."""
        own = f"{self.prefix}/opt/{formula}/bin/mongod"
        return own if formula and os.access(own, os.X_OK) else "mongod"

    def started(self, formula: str, conf: str) -> str:
        """(Re)start to apply the config, then wait for connections; how it was started. Only called when the port is
        down (or at first setup), so a restart is safe here."""
        if formula:
            said(f"(re)starting {formula} via brew services on :{self.port}")
            if output(["brew", "services", "restart", formula]) is not None and self.answers_ping(30):
                return f"brew services ({formula})"
            said(f"brew services could not start {formula} — falling back to mongod --fork", error=True)
        else:
            said("no mongodb-community formula installed — starting mongod directly", error=True)
        subprocess.run([self.mongod_bin(formula), "--config", conf, "--fork"], stdout=subprocess.DEVNULL)
        if not self.answers_ping(60):
            said(f"mongod did not answer on :{self.port} within 60s", error=True)
            raise SystemExit(1)
        return "mongod --fork"

    def primary(self, started: str) -> int:
        """The single-node set initiated once (the catch makes it idempotent on restarts); change streams need a
        PRIMARY."""
        done = subprocess.run(["mongosh", self.uri, "--quiet", "--eval", INITIATE % self.port],
                              stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        print(last_line(done.stdout), flush=True)
        for _ in range(30):
            if self.evaluates("print(db.hello().isWritablePrimary)") == "true":
                said(f"PRIMARY ready on :{self.port} ({started})")
                return 0
            time.sleep(1)
        said("WARN: did not reach PRIMARY within 30s", error=True)
        return 1


def main() -> int:
    port = mongo_local.local_mongo_port(machine.environment())
    prefix = (output(["brew", "--prefix"], quiet=False) or "").strip()
    if not prefix:
        return 1
    conf = config(prefix, port)
    mongo = LocalMongo(prefix, port)
    return mongo.primary(mongo.started(installed_formula(), conf))


if __name__ == "__main__":
    sys.exit(main())
