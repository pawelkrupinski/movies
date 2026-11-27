"""THE KILL OF A GOVERNED RUN, judged (scripts/claude-hooks/governed-kill-guard): `run-script-tests` and `sbt-warm`
hold a governor ticket, and while the laptop is starved one WAITS for it for minutes. Two lanes ended their own queued
runs as hung (2026-11-27), losing their place and the cores charged. A command that kills the pid of such a run -- `kill`,
or `pkill` / `killall` naming one -- is refused while the governor lists that pid (running or WAITING) and the run is
younger than its own bound: its `--timeout S`, else DEFAULT_BOUND_SECONDS. Past the bound it is a hung run, and the kill
is allowed.

The processes and the governor's listing are read through injected functions (`ps`, `agentctl gov status`), so the
judgement is a pure function of what they say. Stdlib only."""
from __future__ import annotations

import re
from typing import Callable, List, Optional, Sequence, Tuple

GOVERNED_TOOLS = ("run-script-tests", "sbt-warm")
DEFAULT_BOUND_SECONDS = 3000
KILLERS = ("kill", "pkill", "killall")
LISTED = re.compile(r"\bpid\s+(\d+)\b")
ETIME = re.compile(r"^(?:(?:(\d+)-)?(\d+):)?(\d+):(\d+)$")
TIMEOUT = re.compile(r"--timeout[ =](\d+)")
Ps = Callable[[Sequence[str]], Optional[str]]  # ps's arguments -> its stdout, None on failure


def listed_pids(status: str) -> List[int]:
    """The pids `agentctl gov status` lists, admitted or WAITING."""
    return [int(m.group(1)) for line in status.splitlines() for m in LISTED.finditer(line)]


def elapsed_seconds(etime: str) -> Optional[int]:
    """ps's `etime` ([[dd-]hh:]mm:ss) in seconds."""
    found = ETIME.match(etime.strip())
    if not found:
        return None
    days, hours, minutes, seconds = (int(g or 0) for g in found.groups())
    return ((days * 24 + hours) * 60 + minutes) * 60 + seconds


def bound_seconds(command: str) -> int:
    found = TIMEOUT.search(command)
    return int(found.group(1)) if found else DEFAULT_BOUND_SECONDS


def run_rows(ps: Ps) -> List[Tuple[int, str]]:
    """(pid, command) of every process `ps` lists."""
    rows = [line.split(None, 1) for line in (ps(["-axo", "pid=,command="]) or "").splitlines() if line.strip()]
    return [(int(row[0]), row[1]) for row in rows if len(row) > 1 and row[0].isdigit()]


def matching_runs(patterns: Sequence[str], ps: Ps) -> List[int]:
    """The pids of the governed tools' runs a pkill / killall pattern would match."""
    matched = [t for t in GOVERNED_TOOLS if any(t in w or w in t for w in patterns)]
    return [pid for pid, command in run_rows(ps) if any(t in command for t in matched)] if matched else []


def killed_pids(words: Sequence[str], ps: Ps) -> List[int]:
    """The pids a kill command names: `kill [-SIG] pid...`; for pkill / killall, the pids of the governed tools' runs
    its pattern would match."""
    operands = [w for w in words[1:] if not w.startswith("-")]
    if words[0].rsplit("/", 1)[-1] == "kill":
        return [int(w) for w in operands if w.isdigit()]
    return matching_runs(operands, ps)


def protected(pid: int, ps: Ps, listed: Sequence[int]) -> Optional[Tuple[str, int, int]]:
    """(the run's command, its age, its bound) when `pid` is a governed tool's run younger than its bound, else None."""
    if pid not in listed:
        return None
    command = (ps(["-o", "command=", "-p", str(pid)]) or "").strip()
    if not any(tool in command for tool in GOVERNED_TOOLS):
        return None
    age = elapsed_seconds(ps(["-o", "etime=", "-p", str(pid)]) or "")
    bound = bound_seconds(command)
    return (command, age, bound) if age is not None and age < bound else None


def refusal(words_by_command: Sequence[Sequence[str]], ps: Ps, status: str) -> Optional[str]:
    """Why a command's kills are refused, or None."""
    listed = listed_pids(status)
    for words in words_by_command:
        if not words or words[0].rsplit("/", 1)[-1] not in KILLERS:
            continue
        for pid in killed_pids(words, ps):
            found = protected(pid, ps, listed)
            if found:
                command, age, bound = found
                return (f"governed-kill-guard: pid {pid} is a governed run (`{command[:80]}`) the governor still lists, "
                        f"{age}s into its {bound}s bound: it may be WAITING for its ticket, not hung, and killing it loses "
                        "its place. `agentctl gov status` shows its queue position. Wait for it (the notification when it "
                        "exits), or kill it once it is past its bound.")
    return None

