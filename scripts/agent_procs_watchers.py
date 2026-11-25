"""Orphaned watchers for scripts/agent-procs: poll loops and waiting tools whose agent session has ended, found from
the process table and the CLAUDE_PID / CLAUDE_CODE_SESSION_ID in each one's environment. Stdlib only."""
from __future__ import annotations

import os
import re
from typing import Dict, List, Optional, Set, Tuple

from agent_procs_core import Process, run, table_command

# A poll loop or a waiting tool, in a command line or a scratchpad script's text.
WATCHING = re.compile(r"\b(until|while)\b[\s\S]*\bsleep\b|\btail\s+(\S+\s+)*(-[a-zA-Z0-9]*[fF]|--follow)"
                      r"|(^|[\s/])(wait-for|ci-wait)(\s|$)|\bagentctl\s+(inbox\s+)?wait\b")
SCRATCH = re.compile(r"^(/private)?/tmp/|^(/private)?/var/folders/|/scratchpad/")
SESSION_KEYS = ("CLAUDE_PID", "CLAUDE_CODE_SESSION_ID")


def environment_line(pid: int) -> str:
    try:
        with open(f"/proc/{pid}/environ", "rb") as handle:
            return f"{pid} " + handle.read().decode(errors="replace").replace("\0", " ")
    except OSError:
        return f"{pid} " + run(["ps", "eww", "-o", "command=", "-p", str(pid)]).stdout


def environment_lines(pids: Set[int]) -> List[str]:
    override = os.environ.get("AGENT_PROCS_ENV_COMMAND")
    if override:
        return run(table_command("AGENT_PROCS_ENV_COMMAND", [])).stdout.splitlines()
    return [environment_line(pid) for pid in pids]


def session_values(rest: str) -> Dict[str, str]:
    values: Dict[str, str] = {}
    for key in SESSION_KEYS:
        match = re.search(rf"(?:^|\s){key}=(\S+)", rest)
        if match:
            values[key] = match.group(1)
    return values


def session_environments(pids: Set[int]) -> Dict[int, Dict[str, str]]:
    """{pid: {CLAUDE_PID, CLAUDE_CODE_SESSION_ID}} for those of `pids` started by an agent session."""
    out: Dict[int, Dict[str, str]] = {}
    for line in environment_lines(pids):
        head, _, rest = line.partition(" ")
        if not head.isdigit() or int(head) not in pids:
            continue
        values = session_values(rest)
        if values.get("CLAUDE_PID", "").isdigit():
            out[int(head)] = values
    return out


def script_is_watching(word: str) -> bool:
    if not (SCRATCH.search(word) and os.path.isfile(word)):
        return False
    try:
        with open(word, errors="replace") as handle:
            return bool(WATCHING.search(handle.read(256 * 1024)))
    except OSError:
        return False


def watching(process: Process, found: Dict[int, Process], depth: int = 0) -> bool:
    """Whether this process is a poll loop / waiting tool, or the `sleep` / `tail` such a loop left behind."""
    if WATCHING.search(process.command):
        return True
    if any(script_is_watching(word) for word in process.command.split()[1:3]):
        return True
    if process.first_word in ("sleep", "tail") and depth == 0:
        parent: Optional[Process] = found.get(process.ppid)
        return process.ppid == 1 or (parent is not None and watching(parent, found, 1))
    return False


def orphaned_watchers(found: Dict[int, Process]) -> List[Tuple[Process, str]]:
    """[(process, its session id)] for every watcher whose agent session (CLAUDE_PID) has ended."""
    candidates = {p.pid: p for p in found.values() if p.pid != os.getpid() and watching(p, found)}
    environments = session_environments(set(candidates))
    orphans: List[Tuple[Process, str]] = []
    for pid, values in environments.items():
        owner = found.get(int(values["CLAUDE_PID"]))
        if owner is None or owner.first_word != "claude":
            orphans.append((candidates[pid], values.get("CLAUDE_CODE_SESSION_ID", "?")))
    return sorted(orphans, key=lambda pair: pair[0].pid)
