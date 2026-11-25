"""The process and worktree model behind scripts/agent-procs: the ps / lsof tables read into `Process` records, the
`git worktree list` read into `Worktree` records, each worktree's state and the suggestion drawn from it. Stdlib
only; the tool and its test read it from beside scripts/agent-procs."""
from __future__ import annotations

import concurrent.futures
import os
import re
import shlex
import subprocess
import time
from typing import Dict, List, Optional

GIB_KB = 1024 * 1024
# Processes that are only somebody's open terminal or agent session: they sit in a worktree without working in it.
IDLE_COMMANDS = {"zsh", "-zsh", "bash", "-bash", "sh", "login", "claude", "tmux", "less", "man", "ssh"}
WORK_KEYWORDS = ("sbt", "java", "identity-slice", "identity_slice", "convergence-local", "mongod", "playwright",
                 "chrome", "node ")


def run(command: List[str], cwd: Optional[str] = None) -> "subprocess.CompletedProcess[str]":
    environment = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.run(command, cwd=cwd, env=environment, capture_output=True, text=True)


def git(root: str, *args: str) -> str:
    return run(["git", "-C", root, *args]).stdout


def parse_clock(text: str) -> float:
    """ps etime / cputime: [[dd-]hh:]mm:ss[.cc] -> seconds."""
    days = 0
    if "-" in text:
        day_part, text = text.split("-", 1)
        days = int(day_part)
    seconds = 0.0
    for part in text.split(":"):
        seconds = seconds * 60 + float(part)
    return days * 86400 + seconds


def humanize(seconds: float) -> str:
    whole = int(seconds)
    for unit, size in (("d", 86400), ("h", 3600), ("m", 60)):
        if whole >= size:
            return f"{whole // size}{unit}"
    return f"{whole}s"


def first_word(command: str) -> str:
    words = command.split()
    return os.path.basename(words[0]) if words else ""


class Process:
    def __init__(self, pid: int, ppid: int, rss_kb: int, cpu_seconds: float, age_seconds: float, command: str) -> None:
        self.pid, self.ppid, self.rss_kb = pid, ppid, rss_kb
        self.cpu_seconds, self.age_seconds, self.command = cpu_seconds, age_seconds, command
        self.cwd: Optional[str] = None

    @property
    def idle(self) -> bool:
        return first_word(self.command) in IDLE_COMMANDS and not any(k in self.command for k in WORK_KEYWORDS)

    @property
    def first_word(self) -> str:
        return first_word(self.command)

    def describe(self) -> str:
        return (f"pid {self.pid:<6} rss {self.rss_kb // 1024:>5}MB cpu {humanize(self.cpu_seconds):>4} "
                f"age {humanize(self.age_seconds):>4}  {self.command[:110]}")


def notable(process: Process) -> bool:
    """Worth a line in the listing: a build, a JVM, a tool run, or anything big — not an open shell or a sleep."""
    return not process.idle and (process.rss_kb >= 100 * 1024 or any(k in process.command for k in WORK_KEYWORDS))


def parse_process_line(line: str) -> Optional[Process]:
    fields = line.split(None, 5)
    if len(fields) < 6:
        return None
    try:
        return Process(int(fields[0]), int(fields[1]), int(fields[2]), parse_clock(fields[3]), parse_clock(fields[4]),
                       fields[5])
    except ValueError:
        return None


def table_command(override_name: str, default: List[str]) -> List[str]:
    override = os.environ.get(override_name)
    return shlex.split(override) if override else default


def processes() -> Dict[int, Process]:
    ps = table_command("AGENT_PROCS_PS_COMMAND",
                       ["ps", "-U", str(os.getuid()), "-o", "pid=,ppid=,rss=,cputime=,etime=,command="])
    found: Dict[int, Process] = {}
    for line in run(ps).stdout.splitlines():
        process = parse_process_line(line)
        if process is not None:
            found[process.pid] = process
    lsof = table_command("AGENT_PROCS_CWD_COMMAND", ["lsof", "-a", "-d", "cwd", "-u", str(os.getuid()), "-Fpn"])
    current: Optional[Process] = None
    for line in run(lsof).stdout.splitlines():
        if line.startswith("p") and line[1:].isdigit():
            current = found.get(int(line[1:]))
        elif line.startswith("n") and current is not None:
            current.cwd = line[1:]
    return found


def ancestors(found: Dict[int, Process]) -> "set[int]":
    """This process and every parent up the real process tree — the caller's shell and agent session — read from
    the process table already listed (`found`), so a shell where `ps -p <pid>` cannot see a parent (a sandboxed
    agent's) is still spared; ps per hop only for a pid the table lacks."""
    chain: "set[int]" = set()
    pid = os.getpid()
    while pid > 1 and pid not in chain:
        chain.add(pid)
        if pid == os.getpid():
            pid = os.getppid()
        elif pid in found:
            pid = found[pid].ppid
        else:
            parent = run(["ps", "-o", "ppid=", "-p", str(pid)]).stdout.strip()
            pid = int(parent) if parent.isdigit() else 0
    return chain


def started_by_session(process: Process, found: Dict[int, Process]) -> bool:
    """Whether an agent session (`claude`) started this process itself -- an MCP server, however it runs (node, npx,
    uvx) -- rather than through a tool call's shell: up the process table, a `claude` reached before any shell."""
    seen: "set[int]" = set()
    pid = process.ppid
    while pid in found and pid not in seen:
        seen.add(pid)
        first = found[pid].first_word
        if first == "claude":
            return True
        if first in IDLE_COMMANDS:
            return False
        pid = found[pid].ppid
    return False


class Worktree:
    def __init__(self, path: str, branch: Optional[str], lock: Optional[str]) -> None:
        self.path, self.branch, self.lock = path, branch, lock
        self.processes: List[Process] = []
        self.exists = False
        self.unreadable = False
        self.dirty = 0
        self.unmerged = 0
        self.commit_age: Optional[float] = None
        self.lock_pid: Optional[int] = None
        self.live_owner = False
        self.is_main = False
        self.suggestion = "keep"

    def under(self, cwd: str) -> bool:
        return cwd == self.path or cwd.startswith(self.path.rstrip("/") + "/")


def apply_worktree_line(tree: Worktree, line: str) -> None:
    if line.startswith("branch "):
        tree.branch = line[7:].removeprefix("refs/heads/")
    elif line == "detached":
        tree.branch = "(detached)"
    elif line.startswith("locked"):
        tree.lock = line[6:].strip() or "locked"


def worktrees(root: str) -> List[Worktree]:
    found: List[Worktree] = []
    current: Optional[Worktree] = None
    for line in git(root, "worktree", "list", "--porcelain").splitlines() + [""]:
        if line.startswith("worktree "):
            current = Worktree(line[9:], None, None)
        elif current is None:
            continue
        elif not line:
            found.append(current)
            current = None
        else:
            apply_worktree_line(current, line)
    return found


def alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except PermissionError:
        return True
    except OSError:
        return False


def owner_of(worktree: str, found: List[Worktree]) -> Optional[Worktree]:
    """The most specific worktree a process's cwd lies in (the root checkout contains the others)."""
    best: Optional[Worktree] = None
    for candidate in found:
        if candidate.path and candidate.under(worktree) and (best is None or len(candidate.path) > len(best.path)):
            best = candidate
    return best


def count_lines(output: str, prefix: str = "") -> int:
    return len([line for line in output.splitlines() if line and line.startswith(prefix)])


def read_git_state(tree: Worktree, origin: str) -> None:
    tree.exists = os.path.isdir(tree.path)
    if not tree.exists:
        tree.unreadable, tree.dirty, tree.unmerged, tree.commit_age = False, 0, 0, None
        return
    status = run(["git", "-C", tree.path, "status", "--porcelain"])
    cherry = run(["git", "-C", tree.path, "cherry", origin, "HEAD"])
    # A git that FAILS here is no evidence of a clean, merged tree: read as "0 dirty, 0 unmerged" it made a
    # worktree git could not read `removable`, and prune deleted it with whatever it held.
    tree.unreadable = status.returncode != 0 or cherry.returncode != 0
    tree.dirty = count_lines(status.stdout)
    tree.unmerged = count_lines(cherry.stdout, "+")
    stamp = git(tree.path, "log", "-1", "--format=%ct").strip()
    tree.commit_age = time.time() - int(stamp) if stamp.isdigit() else None


def suggest(tree: Worktree) -> str:
    if tree.is_main or tree.unreadable or tree.dirty or tree.unmerged or tree.live_owner:
        return "keep"
    return "orphan-procs" if any(not p.idle for p in tree.processes) else "removable"


def inspect_one(tree: Worktree, main_path: str, origin: str) -> None:
    read_git_state(tree, origin)
    match = re.search(r"pid (\d+)", tree.lock or "")
    tree.lock_pid = int(match.group(1)) if match else None
    tree.live_owner = tree.lock_pid is not None and alive(tree.lock_pid)
    tree.is_main = tree.path == main_path
    tree.suggestion = suggest(tree)


def inspect(all_worktrees: List[Worktree], main_path: str, origin: str) -> None:
    # Three git calls per worktree, on a machine that is busy by definition: fan them out.
    with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
        list(pool.map(inspect_one, all_worktrees, [main_path] * len(all_worktrees), [origin] * len(all_worktrees)))
