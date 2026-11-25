"""identity-drift-ci's reads of a run (scripts/identity_drift_ci.py): its takes-drift rows, read through
convergence_reuse.py's one reader of job names, and a baseline run's artifacts, downloaded once, for the seeds its rows
replayed and their verdicts."""
from __future__ import annotations

import glob
import json
import os
import re
import sys
from dataclasses import dataclass
from typing import Any, Callable, List, Mapping, Optional, cast

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
sys.path.insert(0, os.path.join(HERE, "lib"))
import convergence_reuse  # noqa: E402
import tool_cli  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402

say = tool_cli.say_as("identity-drift-ci")


@dataclass(frozen=True)
class Row:
    name: str
    status: str
    conclusion: str

    @property
    def number(self) -> str:
        return self.name[len("takes-drift-"):].split(" ", 1)[0]

    @property
    def country(self) -> str:
        """`germany` of `takes-drift-2 (germany)`."""
        return self.name.split("(", 1)[1][:-1] if "(" in self.name else self.name


class Runs:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], gh: tool_github.GitHub,
                 timeout: float, base_root: str, workflow: str) -> None:
        self.workflow = workflow  # the workflow whose runs these are
        self.runner = runner
        self.env = env
        self.gh = gh
        self.timeout = timeout
        self.base_root = base_root

    def drift_jobs(self, run: str) -> Optional[List[Row]]:
        """Each takes-drift row of the run (none yet: an empty list); None when its jobs cannot be read."""
        out = self.gh.read(["run", "view", run, "--json", "jobs"], self.timeout)
        if out is None:
            return None
        answer: Any = json.loads(out or "{}") or {}
        reader: Callable[[Any], List[str]] = getattr(convergence_reuse, "drift_rows")  # `name\tstatus\tconclusion`
        lines: List[str] = reader(answer.get("jobs") or [])
        return [Row(*(line.split("\t") + ["", ""])[:3]) for line in lines if line]

    def status(self, run: str) -> Optional[str]:
        out = self.gh.read(["run", "view", run, "--json", "status"], self.timeout)
        return None if out is None else str(json.loads(out).get("status") or "")

    def listed(self, args: List[str]) -> Optional[List[Any]]:
        out = self.gh.read(["run", "list", "--workflow", self.workflow, *args], self.timeout)
        return None if out is None else cast(List[Any], json.loads(out or "[]"))

    def download(self, run: str, directory: str) -> bool:
        """The run's identity-drift artifacts into `directory` (tool-conventions.sh's gh_download: absent told from
        failed, a failed read asked again)."""
        return tool_shell.download_run(self.runner, self.env, self.timeout, run, "identity-drift-*", directory)

    def base(self, run: str) -> str:
        """The baseline run's artifacts, downloaded once: their directory."""
        directory = os.path.join(self.base_root, f"base-{run}")
        if not os.path.isdir(directory):
            os.makedirs(directory)
            if not self.download(run, directory):
                say(f"could not download run {run}'s identity-drift artifacts")
        return directory

    def row_seed(self, run: str, row: str) -> str:
        """The seed that row of the run replayed: its artifact's `seed` file; a run whose artifacts name none predates
        the rotation (row N replayed seed N); a run with no artifacts left names nothing (empty): its row replays its
        FIXED set (scripts/ci/drift_seeds.py, drift-gate-seeds.tsv)."""
        directory = self.base(run)
        seeds = ""
        for path in sorted(glob.glob(os.path.join(directory, f"identity-drift-*-takes-drift-{row}", "seed"))):
            with open(path) as f:
                seeds += f.read()
        if seeds.split("\n", 1)[0]:
            return seeds.split("\n", 1)[0]
        if glob.glob(os.path.join(directory, "identity-drift-*-takes-drift-*")):
            return row
        say(f"run {run} left no takes-drift-{row} artifact naming its seed: the row replays its fixed seeds")
        return ""

    def pins_of(self, run: str, rows: List[str]) -> str:
        """`S1,S2` for the run's rows named, the others' sides empty; nothing when no row named a seed."""
        one = self.row_seed(run, "1") if "1" in rows else ""
        two = self.row_seed(run, "2") if "2" in rows else ""
        return f"{one},{two}" if one + two else ""


def open_findings(verdict: str) -> str:
    """Divergences plus flips beyond the allow-lists (identity-drift-check's verdict line); empty when unreadable."""
    try:
        with open(verdict) as f:
            lines = f.read().splitlines()
    except OSError:
        return ""
    for line in lines:
        found = re.match(r"^([0-9]+) divergence\(s\).* ([0-9]+) flip\(s\)", line)
        if found:
            return str(int(found.group(1)) + int(found.group(2)))
    return ""
