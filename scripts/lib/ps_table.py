"""THE PROCESS TABLE, the one way the Python tools read `ps` (scripts/lib): every process a `Process` (pid, parent, its
arguments with whitespace collapsed), the table by pid, the tree under some of them, and `ps`'s [[dd-]hh:]mm:ss clocks.

  parse(text)                  the table of `ps -o pid=,ppid=,args=` text (a line that is no process is skipped)
  ProcessTable(runner, env).read()   the table of every process, `ps -Ao pid=,ppid=,args=`, through the injected runner;
                               ProcessTable.here(env) over this process's one subprocess runner
  descendants(table, of)       every pid under one of `of`, to any depth (not `of` themselves)
  clock_seconds("1-02:03:04.50")   ps's etime / cputime in seconds (0.0 when garbled)"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Mapping, Set

import tool_git
import tool_process

Table = Dict[int, "Process"]


@dataclass(frozen=True)
class Process:
    pid: int
    ppid: int
    args: str

    @property
    def words(self) -> List[str]:
        return self.args.split()


def parse(text: str) -> Table:
    table: Table = {}
    for line in text.splitlines():
        fields = line.split()
        if len(fields) >= 3 and fields[0].isdigit() and fields[1].isdigit():
            table[int(fields[0])] = Process(int(fields[0]), int(fields[1]), " ".join(fields[2:]))
    return table


class ProcessTable:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.runner = runner
        self.env = env

    @classmethod
    def here(cls, env: Mapping[str, str]) -> "ProcessTable":
        return cls(tool_git.shared_runner(), env)

    def read(self) -> Table:
        """Every process; empty when `ps` cannot be run."""
        return parse(self.runner.capture(tool_process.Command(["ps", "-Ao", "pid=,ppid=,args="], self.env)).stdout)


def descendants(table: Table, of: Set[int]) -> Set[int]:
    found: Set[int] = set()
    grew = True
    while grew:
        under = {p.pid for p in table.values() if p.pid not in found and p.pid not in of and p.ppid in of | found}
        found |= under
        grew = bool(under)
    return found


def clock_seconds(text: str) -> float:
    """ps's clock in seconds; 0.0 for a garbled one (a torn `ps` row must not stop a sweep)."""
    try:
        days, _, rest = text.rpartition("-")
        seconds = 0.0
        for part in rest.split(":"):
            seconds = seconds * 60 + float(part)
        return seconds + (int(days) * 86400 if days else 0)
    except ValueError:
        return 0.0
