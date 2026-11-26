"""THE DAEMON PARENT'S UPKEEP (mirror.py): its log kept under a size cap, what an earlier run left behind swept up, the
collection-list digest it publishes to the tailers, and the digest of its own code, whose change makes it exit for
launchd (or service.sh) to restart it on the new code.

THE LOG. Under launchd the daemon's stdout IS the log (log-path.sh's MIRROR_LOG, the path service.sh writes into the
plist) and nothing else rotates it; launchd holds an append-mode descriptor on it, so it is trimmed IN PLACE (truncated
and refilled, the inode kept), never renamed.

THE SWEEP stops exactly: a mirror.sh / mirror.py process reparented to launchd (ppid 1 -- never launchd's own
`service.sh agent`) and every process under it, where its tailer lives (node rewrites mongosh's argv to a bare
"mongosh", so the tree is the only way to know it); and a prod-tunnel forward with ppid 1 and its exact signature (a
port in 27100-27899 forwarded to 127.0.0.1:27017 with ensure_prod_tunnel's options), so a hand-made `ssh -L` never
matches. Never this process.

THE CODE DIGEST covers every file the daemon executes, DERIVED from its own references -- mirror.sh, the modules
mirror.py imports (beside it or in scripts/lib) and each `here("<file>")` they name -- never a hand-written list, so a
file added later is covered without anyone remembering this. Specs and the README are not referenced, so editing them
churns nothing. Empty means "don't know", never a change."""
from __future__ import annotations

import hashlib
import os
import re
import signal
import subprocess
import sys
from typing import Dict, List, Set, Tuple

from mirror_settings import HERE, Settings, here, say

sys.path.insert(0, os.path.join(HERE, "..", "lib"))
import ps_table  # noqa: E402

LIB = os.path.join(os.path.dirname(HERE), "lib")
MAX_LOG_BYTES, LOG_KEEP_LINES = 8 * 1024 * 1024, 2000
MIRROR = re.compile(r"/scripts/local-mirror/mirror\.(sh|py)( |$)")
FORWARD = re.compile(r"^ssh -N -L 27[1-8][0-9][0-9]:127\.0\.0\.1:27017 -o ExitOnForwardFailure=yes -o BatchMode=yes "
                     r".*ServerAliveCountMax=3 ")
REFERENCE = re.compile(r"""\bhere\("([A-Za-z0-9_-]+\.[A-Za-z]+)"\)""")
IMPORT = re.compile(r"^(?:import|from) ([A-Za-z_][A-Za-z0-9_]*)", re.M)


def log_path(env: Dict[str, str]) -> str:
    done = subprocess.run(["bash", "-c", '. "$1"; printf %s "$MIRROR_LOG"', "log-path", here("log-path.sh")],
                          capture_output=True, text=True, env=env)
    return done.stdout


def rotate_log(path: str) -> None:
    try:
        if os.path.getsize(path) <= MAX_LOG_BYTES:
            return
        with open(path, "r+b") as log:
            kept = log.read().splitlines(keepends=True)[-LOG_KEEP_LINES:]
            log.seek(0)
            log.truncate()
            log.writelines(kept)
    except OSError:
        return
    say(f"log passed {MAX_LOG_BYTES // 1024 // 1024}MB — trimmed to its last {LOG_KEEP_LINES} lines")


def orphan_root(args: str) -> str:
    if MIRROR.search(args):
        return "orphaned mirror.sh"
    return "orphaned prod tunnel" if FORWARD.search(args) else ""


def roots(table: ps_table.Table, self_pid: int) -> Dict[int, str]:
    picked: Dict[int, str] = {}
    for pid, process in table.items():
        reason = orphan_root(process.args) if pid != self_pid and process.ppid == 1 else ""
        if reason:
            picked[pid] = reason
    return picked


def orphans(table: ps_table.Table, self_pid: int) -> List[Tuple[int, str, str]]:
    """(pid, reason, args) of every process to stop, from the process table."""
    pick = roots(table, self_pid)
    mirrors = {pid for pid, reason in pick.items() if reason == "orphaned mirror.sh"}
    for pid in ps_table.descendants(table, mirrors):
        pick.setdefault(pid, "under an orphaned mirror.sh")
    return [(pid, reason, table[pid].args) for pid, reason in pick.items()]


def sweep_orphans(env: Dict[str, str]) -> None:
    for pid, reason, args in orphans(ps_table.ProcessTable.here(env).read(), os.getpid()):
        say(f"sweep: stopping pid {pid} ({reason}): {args[:120]}")
        try:
            os.kill(pid, signal.SIGTERM)
        except OSError:
            pass


def targets_fingerprint(settings: Settings) -> str:
    """The digest of the mirrored collection names (targets-fingerprint.js), or nothing when the list is unreadable."""
    lines = settings.nodb("--file", here("mirror-targets.js"), "--file", here("targets-fingerprint.js")).splitlines()
    return lines[-1] if lines else ""


def runtime_files() -> List[str]:
    files: Set[str] = {here("mirror.sh")}
    modules: List[str] = ["mirror"]
    seen: Set[str] = set()
    while modules:
        name = modules.pop()
        if name in seen:
            continue
        seen.add(name)
        for directory in (HERE, LIB):
            path = os.path.join(directory, name + ".py")
            if os.path.isfile(path):
                files.add(path)
                with open(path) as f:
                    text = f.read()
                files.update(here(file) for file in REFERENCE.findall(text))
                modules.extend(IMPORT.findall(text))
                break
    return sorted(files)


def code_fingerprint() -> str:
    digest = hashlib.sha1()
    try:
        for path in runtime_files():
            with open(path, "rb") as f:
                digest.update(f.read())
    except OSError:
        return ""
    return digest.hexdigest()
